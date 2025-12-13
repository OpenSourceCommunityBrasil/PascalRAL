# OpenSSL compiled binaries

Windows Versions:
v1.0 (Indy) = 1.0.2u
v1.1 = 1.1.1w 
v3 = 3.6.0

v1.1 and v3.6 binaries downloaded from FireDaemon: https://kb.firedaemon.com/support/solutions/articles/4000121705-openssl-binary-distributions-for-microsoft-windows
v1.0.2u is Indy-only and is a legacy version, marked as dangerous. If you use Indy as the data engine, you should use NGINX as the HTTPS provider since it is compatible with OpenSSL v3+

For Linux users, your distro should already have the required libraries, possibly v3.6 which is current latest.