package pascalral;

/**
 * Who decides whether a server certificate is acceptable - implemented on the
 * Pascal side, so the single rule RAL already has (SSL.Pins, then
 * OnValidateServerCert, then what the trust store concluded) keeps deciding
 * here too, instead of a second rule growing inside this bridge.
 *
 * It is called DURING the TLS 1.3 handshake, which is the whole point:
 * answering "no" aborts the connection before a single byte of the request -
 * the token included - has been written.
 *
 * Deliberately NOT the same type as pascalral.RalCertJudge of the OkHttp
 * bridge: an application may carry both jars, and two classes with one name
 * would collide at dex time.
 */
public interface RalQuicCertJudge {

  /**
   * @param sha256          fingerprint of the leaf certificate, uppercase hex,
   *                        the same string "openssl x509 -fingerprint -sha256"
   *                        prints once the colons are stripped
   * @param subject         leaf subject DN
   * @param issuer          leaf issuer DN
   * @param platformTrusted whether the platform trust store accepted the chain
   *                        - what RAL calls TRALCertInfo.Trusted
   * @return true to carry on with the handshake
   */
  boolean ok(String sha256, String subject, String issuer, boolean platformTrusted);
}
