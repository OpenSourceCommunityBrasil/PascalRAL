package pascalral;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

import tech.kwik.core.QuicClientConnection;
import tech.kwik.core.QuicStream;

/**
 * The Java half of RAL's Kwik (QUIC) client engine.
 *
 * It exists because Android has no other way to speak QUIC with a stream of
 * its own: every official stack there - OkHttp, Cronet, HttpEngine - is an
 * HTTP client, and RAL's QUIC engine does not speak HTTP. It puts a frame of
 * its own on a raw bidirectional stream, which only a QUIC library exposes.
 *
 * ONE REQUEST IS ONE STREAM, and the delimiter is the stream end: the request
 * frame is written and the output closed (which is the QUIC FIN), then the
 * response frame is read until EOF. That is exactly what TRALMsQuicServer
 * does on the other side, so this bridge and the MsQuic engine are wire
 * compatible - the frame itself is built and parsed on the Pascal side and
 * crosses JNI as one byte array, untouched.
 *
 * The API is deliberately flat - static methods, primitive arguments, one
 * result per thread - because every object crossing JNI costs a binding.
 */
public final class RalKwik {

  private RalKwik() {
  }

  /** execute() answers one of these; the Pascal side maps them to TRALTransportError. */
  public static final int OK = 0;
  public static final int ERR_CONNECT = 1;
  public static final int ERR_TIMEOUT = 2;
  public static final int ERR_CERTIFICATE = 3;
  public static final int ERR_OTHER = 4;

  /** certMode, mirroring TRALMsQuicCertMode on the Pascal side. */
  public static final int CERT_SYSTEM = 0;
  public static final int CERT_NONE = 1;
  public static final int CERT_JUDGE = 2;

  private static final class Result {
    byte[] body;
    String error = "";
    boolean certRefused;
    String certSha256 = "";
    String certSubject = "";
    String certIssuer = "";
  }

  private static final ThreadLocal<Result> RESULT = new ThreadLocal<Result>() {
    @Override
    protected Result initialValue() {
      return new Result();
    }
  };

  /**
   * Who judges the certificate of the handshake running on THIS thread. The
   * trust manager is called from inside connect(), on the calling thread, so a
   * thread local reaches it without the judge having to be a field of anything
   * shared.
   */
  private static final ThreadLocal<RalQuicCertJudge> JUDGE = new ThreadLocal<RalQuicCertJudge>();

  /**
   * Connections kept between requests, keyed by what the Pascal side calls the
   * share key: the certificate policy plus the destination. A TLS connection
   * is judged ONCE, at its handshake, and a reused one has no handshake at
   * all - so only clients that judge certificates alike may ever share one.
   */
  private static final Map<String, QuicClientConnection> CONNECTIONS =
      new HashMap<String, QuicClientConnection>();

  /**
   * Reading the answer runs here so that it can be given up on after
   * RequestTimeout. Daemon threads: the pool must never be what keeps the
   * process alive.
   */
  private static final ExecutorService READERS =
      Executors.newCachedThreadPool(new ThreadFactory() {
        public Thread newThread(Runnable r) {
          Thread t = new Thread(r, "ral-kwik-reader");
          t.setDaemon(true);
          return t;
        }
      });

  private static X509TrustManager platformTrustManager() {
    try {
      TrustManagerFactory factory =
          TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
      factory.init((KeyStore) null);
      TrustManager[] managers = factory.getTrustManagers();
      for (int i = 0; i < managers.length; i++) {
        if (managers[i] instanceof X509TrustManager) {
          return (X509TrustManager) managers[i];
        }
      }
    } catch (Exception e) {
      // no platform trust store: judged certificates still work, system ones do not
    }
    return null;
  }

  private static final X509TrustManager PLATFORM = platformTrustManager();

  /** Marks the refusal as ours, so execute() can tell it from any other TLS failure. */
  private static final String REFUSED = "ral-certificate-refused";

  /**
   * Hands the leaf certificate to the Pascal side and lets it decide. The
   * platform's own verdict travels as a flag rather than as an exception,
   * because RAL wants it as TRALCertInfo.Trusted even when it is going to
   * accept a certificate the platform refused - a pinned self signed one.
   */
  private static final class JudgingTrustManager implements X509TrustManager {

    public X509Certificate[] getAcceptedIssuers() {
      return new X509Certificate[0];
    }

    public void checkClientTrusted(X509Certificate[] chain, String authType) {
    }

    public void checkServerTrusted(X509Certificate[] chain, String authType)
        throws CertificateException {
      if (chain == null || chain.length == 0) {
        throw new CertificateException(REFUSED);
      }

      boolean trusted = false;
      if (PLATFORM != null) {
        try {
          PLATFORM.checkServerTrusted(chain, authType);
          trusted = true;
        } catch (Exception e) {
          trusted = false;
        }
      }

      Result result = RESULT.get();
      result.certSha256 = sha256Hex(chain[0]);
      result.certSubject = String.valueOf(chain[0].getSubjectDN());
      result.certIssuer = String.valueOf(chain[0].getIssuerDN());

      RalQuicCertJudge judge = JUDGE.get();
      boolean accepted = (judge == null)
          ? trusted
          : judge.ok(result.certSha256, result.certSubject, result.certIssuer, trusted);

      if (!accepted) {
        result.certRefused = true;
        throw new CertificateException(REFUSED);
      }
    }
  }

  private static String sha256Hex(X509Certificate certificate) {
    try {
      byte[] digest = MessageDigest.getInstance("SHA-256").digest(certificate.getEncoded());
      StringBuilder text = new StringBuilder(digest.length * 2);
      for (int i = 0; i < digest.length; i++) {
        int value = digest[i] & 0xFF;
        if (value < 0x10) {
          text.append('0');
        }
        text.append(Integer.toHexString(value).toUpperCase());
      }
      return text.toString();
    } catch (Exception e) {
      return "";
    }
  }

  /**
   * The connection for this share key, opened if there is none or if the one
   * kept has died. Only the cache is synchronized: requests run on their own
   * streams and must not queue behind each other, which is the whole reason a
   * shared QUIC connection is not a shared HTTP/1.1 socket.
   */
  private static QuicClientConnection acquire(String shareKey, String host, int port,
      String alpn, int connectMs, int idleMs, int keepAliveSec, int certMode)
      throws Exception {

    synchronized (CONNECTIONS) {
      QuicClientConnection kept = CONNECTIONS.get(shareKey);
      if (kept != null && kept.isConnected()) {
        return kept;
      }
      if (kept != null) {
        CONNECTIONS.remove(shareKey);
        closeQuietly(kept);
      }
    }

    QuicClientConnection.Builder builder = QuicClientConnection.newBuilder()
        .host(host)
        .port(port)
        .applicationProtocol(alpn)
        .connectTimeout(Duration.ofMillis(connectMs))
        .maxIdleTimeout(Duration.ofMillis(idleMs));

    if (certMode == CERT_NONE) {
      builder = builder.noServerCertificateCheck();
    } else if (certMode == CERT_JUDGE) {
      builder = builder.customTrustManager(new JudgingTrustManager());
    }

    QuicClientConnection connection = builder.build();
    connection.connect();

    if (keepAliveSec > 0) {
      connection.keepAlive(keepAliveSec);
    }

    // The chain is read even when nobody judged it, so SSL.Pins and the event
    // see the same fields on every path.
    fillCertificate(connection);

    synchronized (CONNECTIONS) {
      QuicClientConnection raced = CONNECTIONS.get(shareKey);
      if (raced != null && raced.isConnected()) {
        // another thread got there first: keep theirs, drop ours
        closeQuietly(connection);
        return raced;
      }
      CONNECTIONS.put(shareKey, connection);
    }
    return connection;
  }

  private static void fillCertificate(QuicClientConnection connection) {
    Result result = RESULT.get();
    if (!result.certSha256.isEmpty()) {
      return;
    }
    try {
      List<X509Certificate> chain = connection.getServerCertificateChain();
      if (chain != null && !chain.isEmpty()) {
        X509Certificate leaf = chain.get(0);
        result.certSha256 = sha256Hex(leaf);
        result.certSubject = String.valueOf(leaf.getSubjectDN());
        result.certIssuer = String.valueOf(leaf.getIssuerDN());
      }
    } catch (Throwable t) {
      // a connection that cannot describe its certificate is still a usable one
    }
  }

  /**
   * One request: a bidirectional stream, the frame written and finished, the
   * answer read to the end of the stream.
   *
   * @return OK, or one of the ERR_ codes; the detail is in error()
   */
  public static int execute(String host, int port, String alpn, final byte[] frame,
      int connectMs, final int readMs, int idleMs, int keepAliveSec, int certMode,
      String shareKey, RalQuicCertJudge judge) {

    Result result = RESULT.get();
    result.body = null;
    result.error = "";
    result.certRefused = false;
    result.certSha256 = "";
    result.certSubject = "";
    result.certIssuer = "";

    JUDGE.set(judge);
    try {
      QuicClientConnection connection;
      try {
        connection = acquire(shareKey, host, port, alpn, connectMs, idleMs,
            keepAliveSec, certMode);
      } catch (Exception e) {
        result.error = describe(e);
        return (result.certRefused || isCertificateFailure(e))
            ? ERR_CERTIFICATE : ERR_CONNECT;
      }

      final QuicStream stream;
      try {
        stream = connection.createStream(true);
      } catch (Exception e) {
        // the kept connection died between isConnected() and here
        forget(shareKey, connection);
        result.error = describe(e);
        return ERR_CONNECT;
      }

      Future<byte[]> answer = READERS.submit(new Callable<byte[]>() {
        public byte[] call() throws Exception {
          OutputStream out = stream.getOutputStream();
          out.write(frame);
          // the FIN: it is what tells the server the request frame is whole
          out.close();
          return readAll(stream.getInputStream());
        }
      });

      try {
        result.body = answer.get(readMs, TimeUnit.MILLISECONDS);
        return OK;
      } catch (TimeoutException e) {
        answer.cancel(true);
        abort(stream);
        result.error = "request timed out after " + readMs + " ms";
        return ERR_TIMEOUT;
      } catch (Exception e) {
        abort(stream);
        Throwable cause = (e.getCause() != null) ? e.getCause() : e;
        forget(shareKey, connection);
        result.error = describe(cause);
        return isCertificateFailure(cause) ? ERR_CERTIFICATE : ERR_OTHER;
      }
    } catch (Throwable t) {
      result.error = describe(t);
      return ERR_OTHER;
    } finally {
      JUDGE.remove();
    }
  }

  /**
   * Reads to the end of the stream. Written as a loop on purpose:
   * InputStream.readAllBytes is Java 9, and on Android it only arrives with
   * API 33 - this has to run below that.
   */
  private static byte[] readAll(InputStream input) throws Exception {
    ByteArrayOutputStream buffer = new ByteArrayOutputStream(8192);
    byte[] chunk = new byte[8192];
    int read;
    while ((read = input.read(chunk)) > 0) {
      buffer.write(chunk, 0, read);
    }
    input.close();
    return buffer.toByteArray();
  }

  private static void abort(QuicStream stream) {
    try {
      stream.abortReading(0);
    } catch (Throwable t) {
      // best effort: the stream is being given up on either way
    }
    try {
      stream.resetStream(0);
    } catch (Throwable t) {
    }
  }

  private static void forget(String shareKey, QuicClientConnection connection) {
    synchronized (CONNECTIONS) {
      if (CONNECTIONS.get(shareKey) == connection) {
        CONNECTIONS.remove(shareKey);
      }
    }
    closeQuietly(connection);
  }

  private static void closeQuietly(QuicClientConnection connection) {
    try {
      connection.close();
    } catch (Throwable t) {
    }
  }

  /** Drops the connection kept for this key, if any. */
  public static void release(String shareKey) {
    QuicClientConnection connection;
    synchronized (CONNECTIONS) {
      connection = CONNECTIONS.remove(shareKey);
    }
    if (connection != null) {
      closeQuietly(connection);
    }
  }

  /** Drops every kept connection. */
  public static void releaseAll() {
    QuicClientConnection[] all;
    synchronized (CONNECTIONS) {
      all = CONNECTIONS.values().toArray(new QuicClientConnection[0]);
      CONNECTIONS.clear();
    }
    for (int i = 0; i < all.length; i++) {
      closeQuietly(all[i]);
    }
  }

  private static boolean isCertificateFailure(Throwable t) {
    while (t != null) {
      if (t instanceof CertificateException) {
        return true;
      }
      String name = t.getClass().getName().toLowerCase();
      if (name.contains("certificate") || name.contains("badcertificate")) {
        return true;
      }
      String message = t.getMessage();
      if (message != null) {
        String lower = message.toLowerCase();
        if (lower.contains(REFUSED) || lower.contains("bad_certificate")
            || lower.contains("certificate_unknown") || lower.contains("unknown_ca")) {
          return true;
        }
      }
      t = t.getCause();
    }
    return false;
  }

  private static String describe(Throwable t) {
    if (t == null) {
      return "";
    }
    String message = t.getMessage();
    if (message == null || message.isEmpty()) {
      return t.getClass().getName();
    }
    return t.getClass().getSimpleName() + ": " + message;
  }

  /** The version of the Kwik actually linked, asked of the library itself. */
  public static String version() {
    try {
      Package p = QuicClientConnection.class.getPackage();
      String version = (p == null) ? null : p.getImplementationVersion();
      return (version == null) ? "" : version;
    } catch (Throwable t) {
      return "";
    }
  }

  /**
   * True when the platform can actually run Kwik's TLS 1.3: agent15 needs an
   * ECDHE group it can build a key pair for, and X25519 only reaches Android's
   * JCA with API 33. Answering here, once, beats a handshake that fails with a
   * NoSuchAlgorithmException nobody can read.
   */
  public static String cryptoProbe() {
    StringBuilder missing = new StringBuilder();
    check(missing, "KeyPairGenerator", "EC");
    check(missing, "KeyAgreement", "ECDH");
    check(missing, "Cipher", "AES/GCM/NoPadding");
    check(missing, "Mac", "HmacSHA256");
    return missing.toString();
  }

  private static void check(StringBuilder missing, String kind, String algorithm) {
    try {
      if ("KeyPairGenerator".equals(kind)) {
        java.security.KeyPairGenerator.getInstance(algorithm);
      } else if ("KeyAgreement".equals(kind)) {
        javax.crypto.KeyAgreement.getInstance(algorithm);
      } else if ("Cipher".equals(kind)) {
        javax.crypto.Cipher.getInstance(algorithm);
      } else if ("Mac".equals(kind)) {
        javax.crypto.Mac.getInstance(algorithm);
      }
    } catch (Throwable t) {
      if (missing.length() > 0) {
        missing.append(", ");
      }
      missing.append(kind).append('/').append(algorithm);
    }
  }

  public static byte[] body() {
    return RESULT.get().body;
  }

  public static String error() {
    return RESULT.get().error;
  }

  public static boolean certRefused() {
    return RESULT.get().certRefused;
  }

  public static String certSha256() {
    return RESULT.get().certSha256;
  }

  public static String certSubject() {
    return RESULT.get().certSubject;
  }

  public static String certIssuer() {
    return RESULT.get().certIssuer;
  }
}
