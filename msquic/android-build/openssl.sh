#!/bin/bash
# OpenSSL 3.5 (msquic 2.6.1's submodule), static, for Android - in Git's bash
# with RAD Studio's NDK r21, nothing to install. Usage:
#   bash openssl.sh arm64-v8a      (64-bit)
#   bash openssl.sh armeabi-v7a    (32-bit)
# The options are the ones msquic passes itself (submodules/CMakeLists.txt).
# The environment trips four times, and this is what gets around each:
# - Git's perl ships without Locale::Maketext::Simple, ExtUtils::MakeMaker and
#   Pod::Usage: minimal stand-ins live in perl5/ (only what Configure uses);
# - the NDK's make is a native Windows program and does not understand
#   /c/...: the generated Makefile is rewritten to C:/...;
# - Git's shell converts a variable that looks like a path when it calls a
#   native program (PERL5LIB became "C:/..." and perl split it at the ":"):
#   the stand-ins go into the build folder, which make already passes as -I.;
# - the NDK enters the Makefile as $(ANDROID_NDK_ROOT): Configure needs it as
#   /c/... (to match "which clang"), make as C:/... (clang is native).
set -e
case "$1" in
  arm64-v8a)   TARGET=android-arm64; NAME=arm64 ;;
  armeabi-v7a) TARGET=android-arm;   NAME=arm ;;
  *) echo "usage: bash openssl.sh arm64-v8a|armeabi-v7a"; exit 1 ;;
esac
NDK_POSIX=/c/Users/Public/Documents/Embarcadero/Studio/23.0/CatalogRepository/AndroidNDK-21-23.0.53982.0329/android-ndk-r21
NDK_WIN=C:/Users/Public/Documents/Embarcadero/Studio/23.0/CatalogRepository/AndroidNDK-21-23.0.53982.0329/android-ndk-r21
export PATH="$NDK_POSIX/toolchains/llvm/prebuilt/windows-x86_64/bin:$NDK_POSIX/prebuilt/windows-x86_64/bin:$PATH"
SRC=/c/msq/msquic/submodules/openssl
OBJ=/c/msq/openssl-$NAME
PREFIX=C:/msq/openssl-$NAME-inst
rm -rf "$OBJ"
mkdir -p "$OBJ"
cd "$OBJ"
cp -r /c/msq/perl5/* .
ANDROID_NDK_ROOT=$NDK_POSIX PERL5LIB=. perl "$SRC/Configure" $TARGET -D__ANDROID_API__=26 \
  enable-tls1_3 no-makedepend no-dgram no-ssl3 no-psk no-srp no-zlib no-egd \
  no-idea no-rc5 no-rc4 no-afalgeng no-comp no-cms no-ct no-srtp no-ts no-gost \
  no-dso no-ec2m no-tls1 no-tls1_1 no-tls1_2 no-dtls no-dtls1 no-dtls1_2 no-ssl \
  no-ssl3-method no-tls1-method no-tls1_1-method no-tls1_2-method no-dtls1-method \
  no-dtls1_2-method no-siphash no-whirlpool no-aria no-bf no-blake2 no-sm2 no-sm3 \
  no-sm4 no-camellia no-cast no-md4 no-mdc2 no-ocb no-rc2 no-rmd160 no-scrypt \
  no-seed no-weak-ssl-ciphers no-shared no-tests no-uplink no-cmp no-fips \
  no-padlockeng no-siv no-legacy no-deprecated \
  --libdir=lib --prefix="$PREFIX" --openssldir=/system/etc/ssl
sed -i 's#/c/msq/#C:/msq/#g' Makefile
export ANDROID_NDK_ROOT=$NDK_WIN
make -j4 build_libs
make install_dev
echo "== done"
ls -la "$PREFIX/lib"
