@echo off
rem msquic 2.6.1 for Android, built on Windows with nothing to install: CMake
rem and Ninja come with RAD Studio's Android SDK, and so does NDK r21. OpenSSL
rem is built apart (openssl.sh, in Git's bash), because msquic's embedded
rem OpenSSL build uses Unix shell syntax that Ninja on Windows hands to cmd.exe.
rem
rem Everything lives in C:\msq - see README.md for why the path has to be short.
rem After openssl.sh for the same ABI:
rem   build.cmd arm64-v8a          configure (64-bit)
rem   build.cmd arm64-v8a build    compile and strip the debug information
rem and the same with armeabi-v7a (32-bit). The finished library lands in
rem C:\msq\out-<abi>\ready.
setlocal
set ABI=%1
set OSSL=
if "%ABI%"=="arm64-v8a" set OSSL=C:/msq/openssl-arm64-inst
if "%ABI%"=="armeabi-v7a" set OSSL=C:/msq/openssl-arm-inst
if "%OSSL%"=="" (
  echo usage: build.cmd arm64-v8a^|armeabi-v7a [build]
  exit /b 1
)
rem RAD Studio 12's GetIt copies of the Android SDK and NDK; adjust for others.
set SDK=C:\Users\Public\Documents\Embarcadero\Studio\23.0\CatalogRepository\Android-SDK
set HERE=%~dp0
rem The NDK goes through a short junction: the NDK linker does not get past
rem 260 characters, and on 32 bits the path it builds for libatomic.a
rem (lib/gcc/.../4.9.x/../../../../arm-linux-androideabi/lib/../lib/armv7-a/
rem thumb) went over it - "cannot open", and CMake did not even find pthread.h.
if not exist "%HERE%ndk" mklink /J "%HERE%ndk" "C:\Users\Public\Documents\Embarcadero\Studio\23.0\CatalogRepository\AndroidNDK-21-23.0.53982.0329\android-ndk-r21" >nul
set NDK=%HERE%ndk
set PATH=%SDK%\cmake\3.22.1\bin;%NDK%\toolchains\llvm\prebuilt\windows-x86_64\bin;%PATH%
if "%2"=="build" goto :build
rem API 28 and not lower: msquic's selfsign_openssl.c calls glob(), which the
rem NDK only declares from API 28 - see README.md for what that costs.
rem max-page-size is the 16 KB alignment Android 15 and later demand.
cmake -G Ninja -S "%HERE%msquic" -B "%HERE%build-%ABI%" ^
  -DCMAKE_TOOLCHAIN_FILE="%NDK%\build\cmake\android.toolchain.cmake" ^
  -DANDROID_NDK="%NDK%" -DANDROID_ABI=%ABI% -DANDROID_PLATFORM=android-28 ^
  -DCMAKE_BUILD_TYPE=Release -DQUIC_TLS_LIB=openssl -DQUIC_BUILD_SHARED=ON ^
  -DCMAKE_SHARED_LINKER_FLAGS="-Wl,-z,max-page-size=16384" ^
  -DQUIC_OPENSSL_INCLUDE_DIR=%OSSL%/include ^
  -DQUIC_OPENSSL_LIB_DIR=%OSSL%/lib ^
  -DLIB_CRYPTO=%OSSL%/lib/libcrypto.a ^
  -DLIB_SSL=%OSSL%/lib/libssl.a ^
  -DQUIC_BUILD_TEST=OFF -DQUIC_BUILD_TOOLS=OFF -DQUIC_BUILD_PERF=OFF ^
  -DQUIC_ENABLE_LOGGING=OFF -DQUIC_SKIP_CI_CHECKS=ON ^
  -DQUIC_OUTPUT_DIR="%HERE%out-%ABI%" -DQUIC_LIBRARY_NAME=msquic
exit /b %errorlevel%
:build
cmake --build "%HERE%build-%ABI%" -- -j 4
if errorlevel 1 exit /b 1
if not exist "%HERE%out-%ABI%\ready" mkdir "%HERE%out-%ABI%\ready"
llvm-strip --strip-unneeded -o "%HERE%out-%ABI%\ready\libmsquic.so" "%HERE%out-%ABI%\libmsquic.so"
exit /b %errorlevel%
