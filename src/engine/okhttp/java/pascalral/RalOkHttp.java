package pascalral;

import java.net.Socket;
import java.security.MessageDigest;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509ExtendedTrustManager;
import javax.net.ssl.X509TrustManager;

import okhttp3.Handshake;
import okhttp3.Headers;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Protocol;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;

/**
 * Bridge between PascalRAL and OkHttp, so that an Android client can speak
 * HTTP/2.
 *
 * Android's own HttpURLConnection - what Delphi's TNetHTTPClient uses
 * underneath - is HTTP/1.1 only: AOSP hands its copy of OkHttp a protocol list
 * without h2, so ALPN never offers it. Measured against a server proven to
 * serve h2 to Edge, to a Java 17 client and to WinHTTP, the same application
 * on the handset always arrived as HTTP/1.1.
 *
 * The API is deliberately flat - static methods, primitive arguments, one
 * result per thread - because all of it is reached from Pascal through JNI,
 * where every extra object costs a binding.
 */
public final class RalOkHttp {

  private RalOkHttp() { }

  /** One call's outcome, kept per thread: RAL serves requests from a pool. */
  private static final class Result {
    int status;
    String protocol = "";
    String headers = "";
    byte[] body = new byte[0];
    String error = "";
    String certSha256 = "";
    String certSubject = "";
    String certIssuer = "";
    boolean certRefused;
  }

  private static final ThreadLocal<Result> RESULT = new ThreadLocal<Result>() {
    @Override protected Result initialValue() { return new Result(); }
  };

  /**
   * Who judges the certificate of the call running on THIS thread.
   *
   * Per thread and not per client because the client is cached and shared,
   * while the judge belongs to whoever is calling. The handshake happens
   * inside execute() on this same thread, so the value is always the right one
   * by the time the trust manager asks for it.
   */
  private static final ThreadLocal<RalCertJudge> JUDGE = new ThreadLocal<RalCertJudge>();

  /** Whether the judge has already approved this thread's certificate. */
  private static final ThreadLocal<Boolean> APPROVED = new ThreadLocal<Boolean>() {
    @Override protected Boolean initialValue() { return Boolean.FALSE; }
  };

  /** One client per configuration, mirroring RAL's own transport pool. */
  private static final Map<String, OkHttpClient> CLIENTS = new HashMap<String, OkHttpClient>();

  private static X509TrustManager platformTrustManager() {
    try {
      TrustManagerFactory f = TrustManagerFactory.getInstance(
          TrustManagerFactory.getDefaultAlgorithm());
      f.init((java.security.KeyStore) null);
      for (TrustManager tm : f.getTrustManagers()) {
        if (tm instanceof X509TrustManager) {
          return (X509TrustManager) tm;
        }
      }
    } catch (Exception e) {
      // falls through to null - judge() then reports "not trusted"
    }
    return null;
  }

  private static final X509TrustManager PLATFORM = platformTrustManager();

  /**
   * The trust manager that hands the decision to Pascal.
   *
   * It always asks the platform FIRST, and passes that verdict along instead of
   * replacing it: RAL needs to know whether the chain was trusted on its own,
   * because with no pin and no event that verdict is the answer. Refusing here
   * throws, which aborts the handshake - nothing of the request has been sent
   * yet, and that is the reason this lives in the trust manager rather than in
   * a check after the response.
   */
  private static final class JudgingTrustManager extends X509ExtendedTrustManager {

    public X509Certificate[] getAcceptedIssuers() {
      return PLATFORM == null ? new X509Certificate[0] : PLATFORM.getAcceptedIssuers();
    }

    public void checkClientTrusted(X509Certificate[] c, String t) { }

    public void checkClientTrusted(X509Certificate[] c, String t, Socket s) { }

    public void checkClientTrusted(X509Certificate[] c, String t, SSLEngine e) { }

    public void checkServerTrusted(X509Certificate[] chain, String authType)
        throws CertificateException {
      judge(chain, authType);
    }

    public void checkServerTrusted(X509Certificate[] chain, String authType, Socket s)
        throws CertificateException {
      judge(chain, authType);
    }

    public void checkServerTrusted(X509Certificate[] chain, String authType, SSLEngine e)
        throws CertificateException {
      judge(chain, authType);
    }

    private void judge(X509Certificate[] chain, String authType) throws CertificateException {
      Result r = RESULT.get();
      APPROVED.set(Boolean.FALSE);

      boolean trusted = false;
      if (PLATFORM != null) {
        try {
          PLATFORM.checkServerTrusted(chain, authType);
          trusted = true;
        } catch (Exception ex) {
          trusted = false;
        }
      }

      if (chain != null && chain.length > 0) {
        try {
          r.certSha256 = sha256Hex(chain[0].getEncoded());
        } catch (Exception ex) {
          r.certSha256 = "";
        }
        r.certSubject = chain[0].getSubjectDN().getName();
        r.certIssuer = chain[0].getIssuerDN().getName();
      }

      RalCertJudge judge = JUDGE.get();
      boolean ok = (judge == null)
          ? trusted
          : judge.ok(r.certSha256, r.certSubject, r.certIssuer, trusted);

      if (!ok) {
        r.certRefused = true;
        throw new CertificateException("certificate refused by PascalRAL");
      }
      APPROVED.set(Boolean.TRUE);
    }
  }

  /**
   * shareKey is what decides who shares a transport with whom: empty means
   * "share by configuration", which is what RAL asks for by default, and a
   * value of its own isolates one client - ShareConnection turned off. The
   * isolated ones are dropped by release(), so the map does not grow with
   * every client an application creates.
   */
  private static synchronized OkHttpClient client(int connectMs, int readMs,
                                                  boolean allowHttp2,
                                                  boolean followRedirects,
                                                  String shareKey) {
    String key = (shareKey == null ? "" : shareKey) + "|" + connectMs + "|" + readMs
               + "|" + allowHttp2 + "|" + followRedirects;
    OkHttpClient cached = CLIENTS.get(key);
    if (cached != null) {
      return cached;
    }

    OkHttpClient.Builder b = new OkHttpClient.Builder()
        .connectTimeout(connectMs, TimeUnit.MILLISECONDS)
        .readTimeout(readMs, TimeUnit.MILLISECONDS)
        .writeTimeout(readMs, TimeUnit.MILLISECONDS)
        .followRedirects(followRedirects)
        .followSslRedirects(followRedirects)
        .retryOnConnectionFailure(true);

    // h2 first, with 1.1 alongside it in the same ALPN offer. Asking for h2
    // alone would fail outright against any server that does not have it.
    b.protocols(allowHttp2
        ? Arrays.asList(Protocol.HTTP_2, Protocol.HTTP_1_1)
        : Arrays.asList(Protocol.HTTP_1_1));

    try {
      JudgingTrustManager tm = new JudgingTrustManager();
      SSLContext ctx = SSLContext.getInstance("TLS");
      ctx.init(null, new TrustManager[] { tm }, new java.security.SecureRandom());
      b.sslSocketFactory(ctx.getSocketFactory(), tm);

      // The name is part of what the judge already decided on: a pinned
      // certificate is the certificate, whatever name the URL used, and that
      // is the same latitude the other engines give their handler. With no
      // judge installed the platform verdict rules, and this stays strict.
      b.hostnameVerifier(new HostnameVerifier() {
        public boolean verify(String hostname, SSLSession session) {
          if (JUDGE.get() != null && Boolean.TRUE.equals(APPROVED.get())) {
            return true;
          }
          return okhttp3.internal.tls.OkHostnameVerifier.INSTANCE.verify(hostname, session);
        }
      });
    } catch (Exception e) {
      // keep OkHttp's own strict defaults - failing closed
    }

    OkHttpClient built = b.build();
    CLIENTS.put(key, built);
    return built;
  }

  /**
   * Drops every client isolated under this shareKey and closes the sockets it
   * was holding. Called when a RAL client that was NOT sharing is destroyed -
   * without it the map would keep one entry per client ever created.
   */
  public static synchronized void release(String shareKey) {
    if (shareKey == null || shareKey.isEmpty()) {
      return;
    }
    String prefix = shareKey + "|";
    List<String> doomed = new ArrayList<String>();
    for (String k : CLIENTS.keySet()) {
      if (k.startsWith(prefix)) {
        doomed.add(k);
      }
    }
    for (Iterator<String> it = doomed.iterator(); it.hasNext(); ) {
      String k = it.next();
      OkHttpClient c = CLIENTS.remove(k);
      if (c != null) {
        try {
          c.dispatcher().executorService().shutdown();
          c.connectionPool().evictAll();
        } catch (Exception e) {
          // nothing useful to do while tearing down
        }
      }
    }
  }

  /**
   * Runs one request. Returns 0 when a response arrived, whatever its status,
   * and -1 when the call itself failed - error() then says why, and
   * certRefused() separates "the certificate was turned down" from the rest.
   *
   * headerBlock is one "Name: Value" per line, which keeps the JNI surface at
   * a single string instead of an array of objects.
   */
  public static int execute(String method, String url, String headerBlock,
                            byte[] body, String contentType,
                            int connectMs, int readMs, boolean allowHttp2,
                            boolean followRedirects, String shareKey,
                            RalCertJudge judge) {
    Result r = new Result();
    RESULT.set(r);
    JUDGE.set(judge);
    APPROVED.set(Boolean.FALSE);
    Response resp = null;
    try {
      RequestBody rb = null;
      if (permitsBody(method) && body != null && body.length > 0) {
        MediaType mt = (contentType == null || contentType.isEmpty())
            ? null : MediaType.parse(contentType);
        rb = RequestBody.create(body, mt);
      } else if (requiresBody(method)) {
        // POST/PUT/PATCH with nothing to send still need an empty body, or
        // OkHttp refuses to build the request.
        rb = RequestBody.create(new byte[0], null);
      }

      Request.Builder q = new Request.Builder().url(url).method(method, rb);
      if (headerBlock != null && !headerBlock.isEmpty()) {
        for (String line : headerBlock.split("\n")) {
          int i = line.indexOf(':');
          if (i <= 0) {
            continue;
          }
          String name = line.substring(0, i).trim();
          String value = line.substring(i + 1).trim();
          if (name.isEmpty()) {
            continue;
          }
          // Content-Type travels with the body; Content-Length is OkHttp's.
          if ("content-type".equalsIgnoreCase(name)) {
            continue;
          }
          if ("content-length".equalsIgnoreCase(name)) {
            continue;
          }
          q.addHeader(name, value);
        }
      }

      resp = client(connectMs, readMs, allowHttp2, followRedirects, shareKey)
               .newCall(q.build()).execute();
      r.status = resp.code();
      r.protocol = resp.protocol().toString();

      StringBuilder sb = new StringBuilder();
      Headers hs = resp.headers();
      for (int i = 0; i < hs.size(); i++) {
        sb.append(hs.name(i)).append(": ").append(hs.value(i)).append('\n');
      }
      r.headers = sb.toString();

      ResponseBody rbody = resp.body();
      r.body = (rbody == null) ? new byte[0] : rbody.bytes();

      Handshake hk = resp.handshake();
      if (hk != null && r.certSha256.isEmpty()) {
        List<Certificate> certs = hk.peerCertificates();
        if (!certs.isEmpty() && certs.get(0) instanceof X509Certificate) {
          X509Certificate x = (X509Certificate) certs.get(0);
          r.certSha256 = sha256Hex(x.getEncoded());
          r.certSubject = x.getSubjectDN().getName();
          r.certIssuer = x.getIssuerDN().getName();
        }
      }
      return 0;
    } catch (Throwable t) {
      r.error = t.getClass().getName() + ": " + String.valueOf(t.getMessage());
      return -1;
    } finally {
      JUDGE.remove();
      APPROVED.remove();
      if (resp != null) {
        resp.close();
      }
    }
  }

  /** Which protocol a plain GET settles on - used to prove h2 on a device. */
  public static String probe(String url, int connectMs, int readMs) {
    int rc = execute("GET", url, "", null, "", connectMs, readMs,
                     true, true, "", ACCEPT_ALL);
    return (rc == 0) ? (protocol() + " status=" + status()) : ("error: " + error());
  }

  /** Only for probe(): the application's own judge runs in a real call. */
  private static final RalCertJudge ACCEPT_ALL = new RalCertJudge() {
    public boolean ok(String sha256, String subject, String issuer, boolean platformTrusted) {
      return true;
    }
  };

  /** Methods that MUST carry a body, even an empty one. */
  private static boolean requiresBody(String method) {
    return "POST".equals(method) || "PUT".equals(method) || "PATCH".equals(method);
  }

  /** Methods OkHttp refuses to attach a body to. */
  private static boolean permitsBody(String method) {
    return !"GET".equals(method) && !"HEAD".equals(method);
  }

  private static String sha256Hex(byte[] der) {
    try {
      byte[] h = MessageDigest.getInstance("SHA-256").digest(der);
      StringBuilder sb = new StringBuilder(h.length * 2);
      for (int i = 0; i < h.length; i++) {
        sb.append(String.format("%02X", h[i]));
      }
      return sb.toString();
    } catch (Exception e) {
      return "";
    }
  }

  public static int status() { return RESULT.get().status; }

  public static String protocol() { return RESULT.get().protocol; }

  public static String headers() { return RESULT.get().headers; }

  public static byte[] body() { return RESULT.get().body; }

  public static String error() { return RESULT.get().error; }

  public static boolean certRefused() { return RESULT.get().certRefused; }

  public static String certSha256() { return RESULT.get().certSha256; }

  public static String certSubject() { return RESULT.get().certSubject; }

  public static String certIssuer() { return RESULT.get().certIssuer; }
}
