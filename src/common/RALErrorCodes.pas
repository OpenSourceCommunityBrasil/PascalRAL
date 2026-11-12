/// Stable error identifiers and localized message templates.
unit RALErrorCodes;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$H+}
{$ENDIF}

interface

const
  /// No error identifier was assigned to the exception.
  RAL_ERROR_UNSPECIFIED = 0;

  /// Error identifier for emCryptEmptyKey.
  ecCryptEmptyKey = 1033;
  /// Error identifier for emCryptInvalidLength.
  ecCryptInvalidLength = 1034;
  /// Error identifier for emCryptInvalidPadding.
  ecCryptInvalidPadding = 1035;
  /// Error identifier for emCryptInvalidMAC.
  ecCryptInvalidMAC = 1036;
  /// Error identifier for emStorageInvalidBinary.
  ecStorageInvalidBinary = 1057;
  /// Error identifier for emOpenSSLCallFailed.
  ecOpenSSLCallFailed = 1068;
  /// Error identifier for emOpenSSLNotLoaded.
  ecOpenSSLNotLoaded = 1069;
  /// Error identifier for emJSONMissingNumberDigits.
  ecJSONMissingNumberDigits = 2000;
  /// Error identifier for emJSONInvalidNumber.
  ecJSONInvalidNumber = 2001;
  /// Error identifier for emUTF8InvalidLeadingByte.
  ecUTF8InvalidLeadingByte = 2002;
  /// Error identifier for emUTF8IncompleteSequence.
  ecUTF8IncompleteSequence = 2003;
  /// Error identifier for emUTF8InvalidContinuationByte.
  ecUTF8InvalidContinuationByte = 2004;
  /// Error identifier for emUTF8InvalidCodePoint.
  ecUTF8InvalidCodePoint = 2005;
  /// Error identifier for emUTF16UnpairedSurrogate.
  ecUTF16UnpairedSurrogate = 2006;
  /// Error identifier for emJSONStreamRequired.
  ecJSONStreamRequired = 2007;
  /// Error identifier for emJSONStreamReadFailed.
  ecJSONStreamReadFailed = 2008;
  /// Error identifier for emJSONStringToNumber.
  ecJSONStringToNumber = 2009;
  /// Error identifier for emJSONValueDoubleRange.
  ecJSONValueDoubleRange = 2010;
  /// Error identifier for emJSONValueToString.
  ecJSONValueToString = 2011;
  /// Error identifier for emJSONValueToNumber.
  ecJSONValueToNumber = 2012;
  /// Error identifier for emJSONStringNotInt64.
  ecJSONStringNotInt64 = 2013;
  /// Error identifier for emJSONValueToInt64.
  ecJSONValueToInt64 = 2014;
  /// Error identifier for emJSONValueToBoolean.
  ecJSONValueToBoolean = 2015;
  /// Error identifier for emJSONValueNotObject.
  ecJSONValueNotObject = 2016;
  /// Error identifier for emJSONValueNotArray.
  ecJSONValueNotArray = 2017;
  /// Error identifier for emJSONInvalidChild.
  ecJSONInvalidChild = 2018;
  /// Error identifier for emJSONDuplicateMember.
  ecJSONDuplicateMember = 2019;
  /// Error identifier for emJSONMemberNotFound.
  ecJSONMemberNotFound = 2020;
  /// Error identifier for emJSONNumberDoubleRange.
  ecJSONNumberDoubleRange = 2021;
  /// Error identifier for emJSONNumberNotInt64.
  ecJSONNumberNotInt64 = 2022;
  /// Error identifier for emJSONUnexpectedCharacter.
  ecJSONUnexpectedCharacter = 2024;
  /// Error identifier for emJSONInvalidUnicodeEscape.
  ecJSONInvalidUnicodeEscape = 2025;
  /// Error identifier for emJSONUnterminatedString.
  ecJSONUnterminatedString = 2026;
  /// Error identifier for emJSONUnpairedUnicodeSurrogate.
  ecJSONUnpairedUnicodeSurrogate = 2027;
  /// Error identifier for emJSONInvalidEscape.
  ecJSONInvalidEscape = 2028;
  /// Error identifier for emJSONResetBeforeLimits.
  ecJSONResetBeforeLimits = 2029;
  /// Error identifier for emJSONNegativeMaxDocumentSize.
  ecJSONNegativeMaxDocumentSize = 2030;
  /// Error identifier for emJSONNegativeMaxTokenSize.
  ecJSONNegativeMaxTokenSize = 2031;
  /// Error identifier for emJSONNegativeMaxDepth.
  ecJSONNegativeMaxDepth = 2032;
  /// Error identifier for emJSONParserFinished.
  ecJSONParserFinished = 2033;
  /// Error identifier for emJSONParserFailed.
  ecJSONParserFailed = 2034;
  /// Error identifier for emJSONTokenTooLarge.
  ecJSONTokenTooLarge = 2036;
  /// Error identifier for emJSONUnexpectedTrailingContent.
  ecJSONUnexpectedTrailingContent = 2037;
  /// Error identifier for emJSONUnexpectedValue.
  ecJSONUnexpectedValue = 2038;
  /// Error identifier for emJSONIncompleteLiteral.
  ecJSONIncompleteLiteral = 2039;
  /// Error identifier for emJSONNoTokenToComplete.
  ecJSONNoTokenToComplete = 2040;
  /// Error identifier for emJSONDepthExceeded.
  ecJSONDepthExceeded = 2041;
  /// Error identifier for emJSONUnexpectedClosingDelimiter.
  ecJSONUnexpectedClosingDelimiter = 2042;
  /// Error identifier for emJSONIncompleteObject.
  ecJSONIncompleteObject = 2043;
  /// Error identifier for emJSONIncompleteArray.
  ecJSONIncompleteArray = 2044;
  /// Error identifier for emUTF8InvalidBOM.
  ecUTF8InvalidBOM = 2045;
  /// Error identifier for emJSONUnescapedControlCharacter.
  ecJSONUnescapedControlCharacter = 2046;
  /// Error identifier for emJSONInvalidLiteral.
  ecJSONInvalidLiteral = 2047;
  /// Error identifier for emJSONUnexpectedColon.
  ecJSONUnexpectedColon = 2048;
  /// Error identifier for emJSONUnexpectedComma.
  ecJSONUnexpectedComma = 2049;
  /// Error identifier for emJSONNegativeBlockSize.
  ecJSONNegativeBlockSize = 2050;
  /// Error identifier for emJSONInputTooLarge.
  ecJSONInputTooLarge = 2051;
  /// Error identifier for emUTF8IncompleteBOM.
  ecUTF8IncompleteBOM = 2052;
  /// Error identifier for emJSONIncompleteToken.
  ecJSONIncompleteToken = 2053;
  /// Error identifier for emJSONIncompleteContainer.
  ecJSONIncompleteContainer = 2054;
  /// Error identifier for emJSONValueExpected.
  ecJSONValueExpected = 2055;
  /// Error identifier for emBSONNegativeLimits.
  ecBSONNegativeLimits = 3000;
  /// Error identifier for emExtendedJSONStringRequired.
  ecExtendedJSONStringRequired = 3001;
  /// Error identifier for emExtendedJSONObjectRequired.
  ecExtendedJSONObjectRequired = 3002;
  /// Error identifier for emExtendedJSONInvalidMemberCount.
  ecExtendedJSONInvalidMemberCount = 3003;
  /// Error identifier for emHexInvalidText.
  ecHexInvalidText = 3004;
  /// Error identifier for emHexInvalidLength.
  ecHexInvalidLength = 3005;
  /// Error identifier for emBSONInvalidRegexOption.
  ecBSONInvalidRegexOption = 3006;
  /// Error identifier for emBSONRegexOptionsOrder.
  ecBSONRegexOptionsOrder = 3007;
  /// Error identifier for emBSONSourceRequired.
  ecBSONSourceRequired = 3008;
  /// Error identifier for emBSONValueBeyondDocument.
  ecBSONValueBeyondDocument = 3009;
  /// Error identifier for emBSONInvalidRead.
  ecBSONInvalidRead = 3010;
  /// Error identifier for emBSONInvalidStringLength.
  ecBSONInvalidStringLength = 3011;
  /// Error identifier for emBSONMissingStringTerminator.
  ecBSONMissingStringTerminator = 3012;
  /// Error identifier for emBSONDepthExceeded.
  ecBSONDepthExceeded = 3013;
  /// Error identifier for emBSONInvalidDocumentLength.
  ecBSONInvalidDocumentLength = 3014;
  /// Error identifier for emBSONDocumentTooLarge.
  ecBSONDocumentTooLarge = 3015;
  /// Error identifier for emBSONPrematureDocumentTerminator.
  ecBSONPrematureDocumentTerminator = 3016;
  /// Error identifier for emBSONNonSequentialArrayIndexes.
  ecBSONNonSequentialArrayIndexes = 3017;
  /// Error identifier for emBSONDuplicateMember.
  ecBSONDuplicateMember = 3018;
  /// Error identifier for emBSONMissingDocumentTerminator.
  ecBSONMissingDocumentTerminator = 3019;
  /// Error identifier for emBSONInvalidBinaryLength.
  ecBSONInvalidBinaryLength = 3020;
  /// Error identifier for emBSONInvalidOldBinaryLength.
  ecBSONInvalidOldBinaryLength = 3021;
  /// Error identifier for emBSONOldBinaryLengthMismatch.
  ecBSONOldBinaryLengthMismatch = 3022;
  /// Error identifier for emBSONInvalidBoolean.
  ecBSONInvalidBoolean = 3023;
  /// Error identifier for emBSONInvalidCodeWithScopeLength.
  ecBSONInvalidCodeWithScopeLength = 3024;
  /// Error identifier for emBSONCodeWithScopeLengthMismatch.
  ecBSONCodeWithScopeLengthMismatch = 3025;
  /// Error identifier for emBSONDecimal128Unsupported.
  ecBSONDecimal128Unsupported = 3026;
  /// Error identifier for emBSONUnknownType.
  ecBSONUnknownType = 3027;
  /// Error identifier for emBSONOutputTooLarge.
  ecBSONOutputTooLarge = 3028;
  /// Error identifier for emBSONCStringContainsNUL.
  ecBSONCStringContainsNUL = 3029;
  /// Error identifier for emBSONDocumentRequired.
  ecBSONDocumentRequired = 3030;
  /// Error identifier for emBSONTimestampUnsignedRequired.
  ecBSONTimestampUnsignedRequired = 3031;
  /// Error identifier for emBSONTimestampUInt32Range.
  ecBSONTimestampUInt32Range = 3032;
  /// Error identifier for emBSONValueRequired.
  ecBSONValueRequired = 3033;
  /// Error identifier for emBSONInvalidBinarySubtype.
  ecBSONInvalidBinarySubtype = 3034;
  /// Error identifier for emBSONInvalidUndefined.
  ecBSONInvalidUndefined = 3035;
  /// Error identifier for emBSONNumberIntRange.
  ecBSONNumberIntRange = 3036;
  /// Error identifier for emBSONInvalidMinMaxKey.
  ecBSONInvalidMinMaxKey = 3037;
  /// Error identifier for emBSONRootRequired.
  ecBSONRootRequired = 3038;
  /// Error identifier for emBSONUnexpectedTrailingBytes.
  ecBSONUnexpectedTrailingBytes = 3039;
  /// Error identifier for emBSONDestinationRequired.
  ecBSONDestinationRequired = 3040;
  /// Error identifier for emOpenSSLMissingExport.
  ecOpenSSLMissingExport = 4000;
  /// Error identifier for emAESInvalidOutputLength.
  ecAESInvalidOutputLength = 4001;
  /// Error identifier for emAESStreamsRequired.
  ecAESStreamsRequired = 4002;
  /// Error identifier for emAESStreamsMustDiffer.
  ecAESStreamsMustDiffer = 4003;
  /// Error identifier for emAESInvalidKeyLength.
  ecAESInvalidKeyLength = 4004;
  /// Error identifier for emAESCBCInvalidIVLength.
  ecAESCBCInvalidIVLength = 4005;
  /// Error identifier for emAESECBUnexpectedIV.
  ecAESECBUnexpectedIV = 4006;
  /// Error identifier for emAESUnsupportedMode.
  ecAESUnsupportedMode = 4007;
  /// Error identifier for emAESCipherUnavailable.
  ecAESCipherUnavailable = 4008;
  /// Error identifier for emAESContextAllocationFailed.
  ecAESContextAllocationFailed = 4009;
  /// Error identifier for emAESInvalidReadCount.
  ecAESInvalidReadCount = 4010;
  /// Error identifier for emAESOutputTooLarge.
  ecAESOutputTooLarge = 4011;
  /// Error identifier for emOpenSSLLibraryFilenameRequired.
  ecOpenSSLLibraryFilenameRequired = 4012;
  /// Error identifier for emDERTruncatedElement.
  ecDERTruncatedElement = 4013;
  /// Error identifier for emDERUnexpectedTag.
  ecDERUnexpectedTag = 4014;
  /// Error identifier for emDERInvalidLength.
  ecDERInvalidLength = 4015;
  /// Error identifier for emDERElementTooLarge.
  ecDERElementTooLarge = 4016;
  /// Error identifier for emDERTruncatedValue.
  ecDERTruncatedValue = 4017;
  /// Error identifier for emOpenSSLNoCompatibleLibrary.
  ecOpenSSLNoCompatibleLibrary = 4018;
  /// Error identifier for emOpenSSLUnsupportedLibrary.
  ecOpenSSLUnsupportedLibrary = 4019;
  /// Error identifier for emOpenSSLLegacyWindowsOnly.
  ecOpenSSLLegacyWindowsOnly = 4020;
  /// Error identifier for emOpenSSLKeyBIOAllocationFailed.
  ecOpenSSLKeyBIOAllocationFailed = 4021;
  /// Error identifier for emOpenSSLInvalidPEMPrivateKey.
  ecOpenSSLInvalidPEMPrivateKey = 4022;
  /// Error identifier for emOpenSSLRSAContextFailed.
  ecOpenSSLRSAContextFailed = 4023;
  /// Error identifier for emOpenSSLPEMBIOAllocationFailed.
  ecOpenSSLPEMBIOAllocationFailed = 4024;
  /// Error identifier for emRSAPrivateKeyRequired.
  ecRSAPrivateKeyRequired = 4025;
  /// Error identifier for emRSAInvalidPublicKeyDER.
  ecRSAInvalidPublicKeyDER = 4026;
  /// Error identifier for emRSAEmptyParameter.
  ecRSAEmptyParameter = 4027;
  /// Error identifier for emCertificateMissingValidity.
  ecCertificateMissingValidity = 4028;
  /// Error identifier for emCertificateInvalidTimeTag.
  ecCertificateInvalidTimeTag = 4029;
  /// Error identifier for emCertificateMissingExpiration.
  ecCertificateMissingExpiration = 4030;
  /// Error identifier for emCertificateTimeAllocationFailed.
  ecCertificateTimeAllocationFailed = 4031;
  /// Error identifier for emRSAParameterUnavailable.
  ecRSAParameterUnavailable = 4032;
  /// Error identifier for emSHA256UnexpectedLength.
  ecSHA256UnexpectedLength = 4033;
  /// Error identifier for emOpenSSLSigningContextFailed.
  ecOpenSSLSigningContextFailed = 4034;
  /// Error identifier for emDNSInvalidNameLength.
  ecDNSInvalidNameLength = 4035;
  /// Error identifier for emCSRRequiresASCIIDomain.
  ecCSRRequiresASCIIDomain = 4036;
  /// Error identifier for emDNSInvalidLabel.
  ecDNSInvalidLabel = 4037;
  /// Error identifier for emDNSLabelTooLong.
  ecDNSLabelTooLong = 4038;
  /// Error identifier for emDNSInvalidName.
  ecDNSInvalidName = 4039;
  /// Error identifier for emCSRDomainRequired.
  ecCSRDomainRequired = 4040;
  /// Error identifier for emCSRAllocationFailed.
  ecCSRAllocationFailed = 4041;
  /// Error identifier for emSANExtensionFailed.
  ecSANExtensionFailed = 4042;
  /// Error identifier for emSANStackAllocationFailed.
  ecSANStackAllocationFailed = 4043;
  /// Error identifier for emCertificateBIOAllocationFailed.
  ecCertificateBIOAllocationFailed = 4044;
  /// Error identifier for emCertificateInvalidPEM.
  ecCertificateInvalidPEM = 4045;
  /// Error identifier for emCertificateNotYetValid.
  ecCertificateNotYetValid = 4046;
  /// Error identifier for emAESKeySize.
  ecAESKeySize = 6310;

  /// Error identifier for emCertPinInvalid.
  ecCertPinInvalid = 1002;
  /// Error identifier for emCertPinUnsupported.
  ecCertPinUnsupported = 1003;
  /// Error identifier for emConnectionClosedNoResponse.
  ecConnectionClosedNoResponse = 1005;
  /// Error identifier for emCertRejected.
  ecCertRejected = 1006;
  /// Error identifier for emCertRequiresTLS.
  ecCertRequiresTLS = 1007;
  /// Error identifier for emHTTP2Unsupported.
  ecHTTP2Unsupported = 1008;
  /// Error identifier for emOkHttpAndroidOnly.
  ecOkHttpAndroidOnly = 1009;
  /// Error identifier for emKwikAndroidOnly.
  ecKwikAndroidOnly = 1010;
  /// Error identifier for emHttpSysAddUrl.
  ecHttpSysAddUrl = 1013;
  /// Error identifier for emHttpSysWindowsOnly.
  ecHttpSysWindowsOnly = 1014;
  /// Error identifier for emHTTP10NotRequestable.
  ecHTTP10NotRequestable = 1015;
  /// Error identifier for emServerListenFailed.
  ecServerListenFailed = 1016;
  /// Error identifier for emQuicRequiresTLS.
  ecQuicRequiresTLS = 1017;
  /// Error identifier for emQuicFileNotFound.
  ecQuicFileNotFound = 1018;
  /// Error identifier for emQuicLibrary.
  ecQuicLibrary = 1019;
  /// Error identifier for emQuicApiFailed.
  ecQuicApiFailed = 1020;
  /// Error identifier for emQuicListenFailed.
  ecQuicListenFailed = 1021;
  /// Error identifier for emQuicHandshakeTimeout.
  ecQuicHandshakeTimeout = 1022;
  /// Error identifier for emQuicConnectFailed.
  ecQuicConnectFailed = 1023;
  /// Error identifier for emQuicFrameTooLarge.
  ecQuicFrameTooLarge = 1025;
  /// Error identifier for emCompressInvalidFormat.
  ecCompressInvalidFormat = 1029;
  /// Error identifier for emCompressFailed.
  ecCompressFailed = 1031;
  /// Error identifier for emContentCheckError.
  ecContentCheckError = 1032;
  /// Error identifier for emDBConnectionUndefined.
  ecDBConnectionUndefined = 1037;
  /// Error identifier for emDBInvalidTableName.
  ecDBInvalidTableName = 1039;
  /// Error identifier for emDBSQLRejected.
  ecDBSQLRejected = 1040;
  /// Error identifier for emStreamSizeBeyondEnd.
  ecStreamSizeBeyondEnd = 1041;
  /// Error identifier for emDecompressLimit.
  ecDecompressLimit = 1042;
  /// Error identifier for emParamFileNameEmpty.
  ecParamFileNameEmpty = 1043;
  /// Error identifier for emDBDriverMissing.
  ecDBDriverMissing = 1044;
  /// Error identifier for emDBEmptyBody.
  ecDBEmptyBody = 1045;
  /// Error identifier for emDBLinkMissing.
  ecDBLinkMissing = 1046;
  /// Error identifier for emDBPoolTimeout.
  ecDBPoolTimeout = 1047;
  /// Error identifier for emDBUpdateSQLMissing.
  ecDBUpdateSQLMissing = 1050;
  /// Error identifier for emHMACEmptyText.
  ecHMACEmptyText = 1051;
  /// Error identifier for emInvalidFormat.
  ecInvalidFormat = 1052;
  /// Error identifier for emInvalidJSONFormat.
  ecInvalidJSONFormat = 1053;
  /// Error identifier for emQueryVersionError.
  ecQueryVersionError = 1054;
  /// Error identifier for emStorageClassNotFound.
  ecStorageClassNotFound = 1056;
  /// Error identifier for emStorageLinkNotFound.
  ecStorageLinkNotFound = 1058;
  /// Error identifier for emStorageNotFound.
  ecStorageNotFound = 1059;
  /// Error identifier for emEngineNotFound.
  ecEngineNotFound = 1060;
  /// Error identifier for emDBFieldLoad.
  ecDBFieldLoad = 1061;
  /// Error identifier for emRandomBytesFailed.
  ecRandomBytesFailed = 1070;
  /// Error identifier for emSaguiLibraryLoadError.
  ecSaguiLibraryLoadError = 1072;
  /// Error identifier for emSaguiServerCreateError.
  ecSaguiServerCreateError = 1073;
  /// Error identifier for emSaguiServerUnsupportedTLS.
  ecSaguiServerUnsupportedTLS = 1074;
  /// Error identifier for wmIPv6notImplemented.
  ecIPv6NotImplemented = 1077;
  /// Error identifier for emResponseCommitted.
  ecResponseCommitted = 6002;
  /// Error identifier for emSSEEventTooBig.
  ecSSEEventTooBig = 6003;
  /// Error identifier for emStreamCipher.
  ecStreamCipher = 6004;
  /// Error identifier for emStreamLimit.
  ecStreamLimit = 6005;
  /// Error identifier for emStreamNoLive.
  ecStreamNoLive = 6006;
  /// Error identifier for emStreamNoServer.
  ecStreamNoServer = 6007;
  /// Error identifier for emStreamCancelled.
  ecStreamCancelled = 6008;
  /// Error identifier for emStreamClientCipher.
  ecStreamClientCipher = 6009;
  /// Error identifier for emStreamClientCoding.
  ecStreamClientCoding = 6010;
  /// Error identifier for emDBLegacyWireProc.
  ecDBLegacyWireProc = 6012;
  /// Error identifier for emDBNoQueue.
  ecDBNoQueue = 6013;
  /// Error identifier for emDBNoStoredProc.
  ecDBNoStoredProc = 6014;
  /// Error identifier for emDBNoTransaction.
  ecDBNoTransaction = 6015;
  /// Error identifier for emDBProcAmbiguous.
  ecDBProcAmbiguous = 6016;
  /// Error identifier for emDBQueueOpen.
  ecDBQueueOpen = 6020;
  /// Error identifier for emJWSInvalidKey.
  ecJWSInvalidKey = 6032;
  /// Error identifier for emJWSNoPrivateKey.
  ecJWSNoPrivateKey = 6033;
  /// Error identifier for emJWSOpenSSL.
  ecJWSOpenSSL = 6034;
  /// Error identifier for emJWSUnsupportedKey.
  ecJWSUnsupportedKey = 6035;
  /// Error identifier for emBigNumInvalidHex.
  ecBigNumInvalidHex = 6036;
  /// Error identifier for emBigNumNegative.
  ecBigNumNegative = 6037;
  /// Error identifier for emBigNumDivByZero.
  ecBigNumDivByZero = 6038;
  /// Error identifier for emBigNumNoInverse.
  ecBigNumNoInverse = 6039;
  /// Error identifier for emBigNumPrimeSize.
  ecBigNumPrimeSize = 6040;
  /// Error identifier for emASN1Negative.
  ecASN1Negative = 6041;
  /// Error identifier for emASN1InvalidOID.
  ecASN1InvalidOID = 6042;
  /// Error identifier for emASN1Truncated.
  ecASN1Truncated = 6043;
  /// Error identifier for emASN1Unsupported.
  ecASN1Unsupported = 6044;
  /// Error identifier for emASN1UnexpectedTag.
  ecASN1UnexpectedTag = 6045;
  /// Error identifier for emASN1InvalidTime.
  ecASN1InvalidTime = 6046;
  /// Error identifier for emASN1Trailing.
  ecASN1Trailing = 6047;
  /// Error identifier for emRSAInvalidKey.
  ecRSAInvalidKey = 6048;
  /// Error identifier for emRSANoPrimes.
  ecRSANoPrimes = 6049;
  /// Error identifier for emRSAKeySize.
  ecRSAKeySize = 6050;
  /// Error identifier for emRSAExponent.
  ecRSAExponent = 6051;
  /// Error identifier for emRSANotRSA.
  ecRSANotRSA = 6052;
  /// Error identifier for emRSAEncryptedKey.
  ecRSAEncryptedKey = 6053;
  /// Error identifier for emRSANoKeyInPEM.
  ecRSANoKeyInPEM = 6054;
  /// Error identifier for emRSANotPrivate.
  ecRSANotPrivate = 6055;
  /// Error identifier for emRSADigestSize.
  ecRSADigestSize = 6056;
  /// Error identifier for emRSASignFault.
  ecRSASignFault = 6057;
  /// Error identifier for emX509NoCertificate.
  ecX509NoCertificate = 6058;
  /// Error identifier for emX509Invalid.
  ecX509Invalid = 6059;
  /// Error identifier for emX509InvalidDetail.
  ecX509InvalidDetail = 6060;
  /// Error identifier for emX509InvalidAddress.
  ecX509InvalidAddress = 6061;
  /// Error identifier for emX509NoKey.
  ecX509NoKey = 6062;
  /// Error identifier for emX509IssuerKeyMismatch.
  ecX509IssuerKeyMismatch = 6063;
  /// Error identifier for emX509InvalidValidity.
  ecX509InvalidValidity = 6064;
  /// Error identifier for emX509AESKey.
  ecX509AESKey = 6065;
  /// Error identifier for emX509KeyMismatch.
  ecX509KeyMismatch = 6066;
  /// Error identifier for emHttpSysCallFailed.
  ecHttpSysCallFailed = 6067;
  /// Error identifier for emHttpSysNoCertificate.
  ecHttpSysNoCertificate = 6068;
  /// Error identifier for emHttpSysImportFailed.
  ecHttpSysImportFailed = 6069;
  /// Error identifier for emHttpSysNoKey.
  ecHttpSysNoKey = 6070;
  /// Error identifier for emHttpSysStoreFailed.
  ecHttpSysStoreFailed = 6071;
  /// Error identifier for emSynopseNoTls.
  ecSynopseNoTls = 6072;
  /// Error identifier for emSelfSignedWriteFailed.
  ecSelfSignedWriteFailed = 6073;
  /// Error identifier for emSelfSignedNoCertificate.
  ecSelfSignedNoCertificate = 6074;
  /// Error identifier for emOAuth2StateMismatch.
  ecOAuth2StateMismatch = 6078;
  /// Error identifier for emRouteBadConstraint.
  ecRouteBadConstraint = 6177;
  /// Error identifier for emMethodUnsupported.
  ecMethodUnsupported = 6178;
  /// Error identifier for emStoreLockTimeout.
  ecStoreLockTimeout = 6179;
  /// Error identifier for emCircuitOpen.
  ecCircuitOpen = 6180;
  /// Error identifier for emTLSError.
  ecTLSError = 6192;
  /// Error identifier for emTLSClosed.
  ecTLSClosed = 6195;
  /// Error identifier for emTLSTimeout.
  ecTLSTimeout = 6196;
  /// Error identifier for emProxyRefused.
  ecProxyRefused = 6206;
  /// Error identifier for emForwardedBadRange.
  ecForwardedBadRange = 6210;
  /// Error identifier for emInProcNameTaken.
  ecInProcNameTaken = 6400;
  /// Error identifier for emInProcNotFound.
  ecInProcNotFound = 6401;
  /// Error identifier for emServiceNoServer.
  ecServiceNoServer = 6402;
  /// Error identifier for emConfigInvalidJSON.
  ecConfigInvalidJSON = 6403;
  /// Error identifier for emCronFieldCount.
  ecCronFieldCount = 6404;
  /// Error identifier for emCronOutOfRange.
  ecCronOutOfRange = 6405;
  /// Error identifier for emCronBadStep.
  ecCronBadStep = 6406;
  /// Error identifier for emCronBadRange.
  ecCronBadRange = 6407;
  /// Error identifier for emTemplateUnclosedTag.
  ecTemplateUnclosedTag = 6410;
  /// Error identifier for emTemplateUnclosedSection.
  ecTemplateUnclosedSection = 6411;
  /// Error identifier for emTemplateUnexpectedClose.
  ecTemplateUnexpectedClose = 6412;
  /// Error identifier for emTemplateBadDelimiter.
  ecTemplateBadDelimiter = 6413;
  /// Error identifier for emTemplateTooDeep.
  ecTemplateTooDeep = 6414;
  /// Error identifier for emTemplateNotFound.
  ecTemplateNotFound = 6415;
  /// Error identifier for emWebSocketUnsupported.
  ecWebSocketUnsupported = 6420;
  /// Error identifier for emWebSocketNotConnected.
  ecWebSocketNotConnected = 6421;
  /// Error identifier for emWebSocketRefused.
  ecWebSocketRefused = 6422;
  /// Error identifier for emWebSocketBadURL.
  ecWebSocketBadURL = 6423;
  /// Error identifier for emHubUnknownMethod.
  ecHubUnknownMethod = 6424;
  /// Error identifier for emHubTimeout.
  ecHubTimeout = 6425;
  /// Error identifier for emHubError.
  ecHubError = 6426;
  /// Error identifier for emSessionObjectNotStored.
  ecSessionObjectNotStored = 6430;
  /// Error identifier for emSessionReadOnly.
  ecSessionReadOnly = 6431;
  /// Error identifier for emSessionCookieTooLarge.
  ecSessionCookieTooLarge = 6432;
  /// Error identifier for emSessionNoSecret.
  ecSessionNoSecret = 6433;
  /// Error identifier for emSessionNoStore.
  ecSessionNoStore = 6434;
  /// Error identifier for emScanFailed.
  ecScanFailed = 6435;
  /// Error identifier for emClamAVAnswer.
  ecClamAVAnswer = 6436;
  /// Error identifier for emViewNoEngine.
  ecViewNoEngine = 6437;
  /// Error identifier for emJobNoHandler.
  ecJobNoHandler = 6440;
  /// Error identifier for emJobAttemptsExhausted.
  ecJobAttemptsExhausted = 6441;
  /// Error identifier for emJobQueueActive.
  ecJobQueueActive = 6442;
  /// Error identifier for emJobStoreWrite.
  ecJobStoreWrite = 6443;
  /// Error identifier for emJobStoreNoDB.
  ecJobStoreNoDB = 6444;

/// Returns the localized template; an unknown code includes its number.
function RALGetErrorMessage(AErrorCode: Integer): string;

implementation

uses
  SysUtils, RALConsts;

function RALGetErrorMessage(AErrorCode: Integer): string;
begin
  case AErrorCode of
    ecCryptEmptyKey: Result := emCryptEmptyKey;
    ecCryptInvalidLength: Result := emCryptInvalidLength;
    ecCryptInvalidPadding: Result := emCryptInvalidPadding;
    ecCryptInvalidMAC: Result := emCryptInvalidMAC;
    ecStorageInvalidBinary: Result := emStorageInvalidBinary;
    ecOpenSSLCallFailed: Result := emOpenSSLCallFailed;
    ecOpenSSLNotLoaded: Result := emOpenSSLNotLoaded;
    ecJSONMissingNumberDigits: Result := emJSONMissingNumberDigits;
    ecJSONInvalidNumber: Result := emJSONInvalidNumber;
    ecUTF8InvalidLeadingByte: Result := emUTF8InvalidLeadingByte;
    ecUTF8IncompleteSequence: Result := emUTF8IncompleteSequence;
    ecUTF8InvalidContinuationByte: Result := emUTF8InvalidContinuationByte;
    ecUTF8InvalidCodePoint: Result := emUTF8InvalidCodePoint;
    ecUTF16UnpairedSurrogate: Result := emUTF16UnpairedSurrogate;
    ecJSONStreamRequired: Result := emJSONStreamRequired;
    ecJSONStreamReadFailed: Result := emJSONStreamReadFailed;
    ecJSONStringToNumber: Result := emJSONStringToNumber;
    ecJSONValueDoubleRange: Result := emJSONValueDoubleRange;
    ecJSONValueToString: Result := emJSONValueToString;
    ecJSONValueToNumber: Result := emJSONValueToNumber;
    ecJSONStringNotInt64: Result := emJSONStringNotInt64;
    ecJSONValueToInt64: Result := emJSONValueToInt64;
    ecJSONValueToBoolean: Result := emJSONValueToBoolean;
    ecJSONValueNotObject: Result := emJSONValueNotObject;
    ecJSONValueNotArray: Result := emJSONValueNotArray;
    ecJSONInvalidChild: Result := emJSONInvalidChild;
    ecJSONDuplicateMember: Result := emJSONDuplicateMember;
    ecJSONMemberNotFound: Result := emJSONMemberNotFound;
    ecJSONNumberDoubleRange: Result := emJSONNumberDoubleRange;
    ecJSONNumberNotInt64: Result := emJSONNumberNotInt64;
    ecJSONUnexpectedCharacter: Result := emJSONUnexpectedCharacter;
    ecJSONInvalidUnicodeEscape: Result := emJSONInvalidUnicodeEscape;
    ecJSONUnterminatedString: Result := emJSONUnterminatedString;
    ecJSONUnpairedUnicodeSurrogate: Result := emJSONUnpairedUnicodeSurrogate;
    ecJSONInvalidEscape: Result := emJSONInvalidEscape;
    ecJSONResetBeforeLimits: Result := emJSONResetBeforeLimits;
    ecJSONNegativeMaxDocumentSize: Result := emJSONNegativeMaxDocumentSize;
    ecJSONNegativeMaxTokenSize: Result := emJSONNegativeMaxTokenSize;
    ecJSONNegativeMaxDepth: Result := emJSONNegativeMaxDepth;
    ecJSONParserFinished: Result := emJSONParserFinished;
    ecJSONParserFailed: Result := emJSONParserFailed;
    ecJSONTokenTooLarge: Result := emJSONTokenTooLarge;
    ecJSONUnexpectedTrailingContent: Result := emJSONUnexpectedTrailingContent;
    ecJSONUnexpectedValue: Result := emJSONUnexpectedValue;
    ecJSONIncompleteLiteral: Result := emJSONIncompleteLiteral;
    ecJSONNoTokenToComplete: Result := emJSONNoTokenToComplete;
    ecJSONDepthExceeded: Result := emJSONDepthExceeded;
    ecJSONUnexpectedClosingDelimiter: Result := emJSONUnexpectedClosingDelimiter;
    ecJSONIncompleteObject: Result := emJSONIncompleteObject;
    ecJSONIncompleteArray: Result := emJSONIncompleteArray;
    ecUTF8InvalidBOM: Result := emUTF8InvalidBOM;
    ecJSONUnescapedControlCharacter: Result := emJSONUnescapedControlCharacter;
    ecJSONInvalidLiteral: Result := emJSONInvalidLiteral;
    ecJSONUnexpectedColon: Result := emJSONUnexpectedColon;
    ecJSONUnexpectedComma: Result := emJSONUnexpectedComma;
    ecJSONNegativeBlockSize: Result := emJSONNegativeBlockSize;
    ecJSONInputTooLarge: Result := emJSONInputTooLarge;
    ecUTF8IncompleteBOM: Result := emUTF8IncompleteBOM;
    ecJSONIncompleteToken: Result := emJSONIncompleteToken;
    ecJSONIncompleteContainer: Result := emJSONIncompleteContainer;
    ecJSONValueExpected: Result := emJSONValueExpected;
    ecBSONNegativeLimits: Result := emBSONNegativeLimits;
    ecExtendedJSONStringRequired: Result := emExtendedJSONStringRequired;
    ecExtendedJSONObjectRequired: Result := emExtendedJSONObjectRequired;
    ecExtendedJSONInvalidMemberCount: Result := emExtendedJSONInvalidMemberCount;
    ecHexInvalidText: Result := emHexInvalidText;
    ecHexInvalidLength: Result := emHexInvalidLength;
    ecBSONInvalidRegexOption: Result := emBSONInvalidRegexOption;
    ecBSONRegexOptionsOrder: Result := emBSONRegexOptionsOrder;
    ecBSONSourceRequired: Result := emBSONSourceRequired;
    ecBSONValueBeyondDocument: Result := emBSONValueBeyondDocument;
    ecBSONInvalidRead: Result := emBSONInvalidRead;
    ecBSONInvalidStringLength: Result := emBSONInvalidStringLength;
    ecBSONMissingStringTerminator: Result := emBSONMissingStringTerminator;
    ecBSONDepthExceeded: Result := emBSONDepthExceeded;
    ecBSONInvalidDocumentLength: Result := emBSONInvalidDocumentLength;
    ecBSONDocumentTooLarge: Result := emBSONDocumentTooLarge;
    ecBSONPrematureDocumentTerminator: Result := emBSONPrematureDocumentTerminator;
    ecBSONNonSequentialArrayIndexes: Result := emBSONNonSequentialArrayIndexes;
    ecBSONDuplicateMember: Result := emBSONDuplicateMember;
    ecBSONMissingDocumentTerminator: Result := emBSONMissingDocumentTerminator;
    ecBSONInvalidBinaryLength: Result := emBSONInvalidBinaryLength;
    ecBSONInvalidOldBinaryLength: Result := emBSONInvalidOldBinaryLength;
    ecBSONOldBinaryLengthMismatch: Result := emBSONOldBinaryLengthMismatch;
    ecBSONInvalidBoolean: Result := emBSONInvalidBoolean;
    ecBSONInvalidCodeWithScopeLength: Result := emBSONInvalidCodeWithScopeLength;
    ecBSONCodeWithScopeLengthMismatch: Result := emBSONCodeWithScopeLengthMismatch;
    ecBSONDecimal128Unsupported: Result := emBSONDecimal128Unsupported;
    ecBSONUnknownType: Result := emBSONUnknownType;
    ecBSONOutputTooLarge: Result := emBSONOutputTooLarge;
    ecBSONCStringContainsNUL: Result := emBSONCStringContainsNUL;
    ecBSONDocumentRequired: Result := emBSONDocumentRequired;
    ecBSONTimestampUnsignedRequired: Result := emBSONTimestampUnsignedRequired;
    ecBSONTimestampUInt32Range: Result := emBSONTimestampUInt32Range;
    ecBSONValueRequired: Result := emBSONValueRequired;
    ecBSONInvalidBinarySubtype: Result := emBSONInvalidBinarySubtype;
    ecBSONInvalidUndefined: Result := emBSONInvalidUndefined;
    ecBSONNumberIntRange: Result := emBSONNumberIntRange;
    ecBSONInvalidMinMaxKey: Result := emBSONInvalidMinMaxKey;
    ecBSONRootRequired: Result := emBSONRootRequired;
    ecBSONUnexpectedTrailingBytes: Result := emBSONUnexpectedTrailingBytes;
    ecBSONDestinationRequired: Result := emBSONDestinationRequired;
    ecOpenSSLMissingExport: Result := emOpenSSLMissingExport;
    ecAESInvalidOutputLength: Result := emAESInvalidOutputLength;
    ecAESStreamsRequired: Result := emAESStreamsRequired;
    ecAESStreamsMustDiffer: Result := emAESStreamsMustDiffer;
    ecAESInvalidKeyLength: Result := emAESInvalidKeyLength;
    ecAESCBCInvalidIVLength: Result := emAESCBCInvalidIVLength;
    ecAESECBUnexpectedIV: Result := emAESECBUnexpectedIV;
    ecAESUnsupportedMode: Result := emAESUnsupportedMode;
    ecAESCipherUnavailable: Result := emAESCipherUnavailable;
    ecAESContextAllocationFailed: Result := emAESContextAllocationFailed;
    ecAESInvalidReadCount: Result := emAESInvalidReadCount;
    ecAESOutputTooLarge: Result := emAESOutputTooLarge;
    ecOpenSSLLibraryFilenameRequired: Result := emOpenSSLLibraryFilenameRequired;
    ecDERTruncatedElement: Result := emDERTruncatedElement;
    ecDERUnexpectedTag: Result := emDERUnexpectedTag;
    ecDERInvalidLength: Result := emDERInvalidLength;
    ecDERElementTooLarge: Result := emDERElementTooLarge;
    ecDERTruncatedValue: Result := emDERTruncatedValue;
    ecOpenSSLNoCompatibleLibrary: Result := emOpenSSLNoCompatibleLibrary;
    ecOpenSSLUnsupportedLibrary: Result := emOpenSSLUnsupportedLibrary;
    ecOpenSSLLegacyWindowsOnly: Result := emOpenSSLLegacyWindowsOnly;
    ecOpenSSLKeyBIOAllocationFailed: Result := emOpenSSLKeyBIOAllocationFailed;
    ecOpenSSLInvalidPEMPrivateKey: Result := emOpenSSLInvalidPEMPrivateKey;
    ecOpenSSLRSAContextFailed: Result := emOpenSSLRSAContextFailed;
    ecOpenSSLPEMBIOAllocationFailed: Result := emOpenSSLPEMBIOAllocationFailed;
    ecRSAPrivateKeyRequired: Result := emRSAPrivateKeyRequired;
    ecRSAInvalidPublicKeyDER: Result := emRSAInvalidPublicKeyDER;
    ecRSAEmptyParameter: Result := emRSAEmptyParameter;
    ecCertificateMissingValidity: Result := emCertificateMissingValidity;
    ecCertificateInvalidTimeTag: Result := emCertificateInvalidTimeTag;
    ecCertificateMissingExpiration: Result := emCertificateMissingExpiration;
    ecCertificateTimeAllocationFailed: Result := emCertificateTimeAllocationFailed;
    ecRSAParameterUnavailable: Result := emRSAParameterUnavailable;
    ecSHA256UnexpectedLength: Result := emSHA256UnexpectedLength;
    ecOpenSSLSigningContextFailed: Result := emOpenSSLSigningContextFailed;
    ecDNSInvalidNameLength: Result := emDNSInvalidNameLength;
    ecCSRRequiresASCIIDomain: Result := emCSRRequiresASCIIDomain;
    ecDNSInvalidLabel: Result := emDNSInvalidLabel;
    ecDNSLabelTooLong: Result := emDNSLabelTooLong;
    ecDNSInvalidName: Result := emDNSInvalidName;
    ecCSRDomainRequired: Result := emCSRDomainRequired;
    ecCSRAllocationFailed: Result := emCSRAllocationFailed;
    ecSANExtensionFailed: Result := emSANExtensionFailed;
    ecSANStackAllocationFailed: Result := emSANStackAllocationFailed;
    ecCertificateBIOAllocationFailed: Result := emCertificateBIOAllocationFailed;
    ecCertificateInvalidPEM: Result := emCertificateInvalidPEM;
    ecCertificateNotYetValid: Result := emCertificateNotYetValid;
    ecAESKeySize: Result := emAESKeySize;
    ecCertPinInvalid: Result := emCertPinInvalid;
    ecCertPinUnsupported: Result := emCertPinUnsupported;
    ecConnectionClosedNoResponse: Result := emConnectionClosedNoResponse;
    ecCertRejected: Result := emCertRejected;
    ecCertRequiresTLS: Result := emCertRequiresTLS;
    ecHTTP2Unsupported: Result := emHTTP2Unsupported;
    ecOkHttpAndroidOnly: Result := emOkHttpAndroidOnly;
    ecKwikAndroidOnly: Result := emKwikAndroidOnly;
    ecHttpSysAddUrl: Result := emHttpSysAddUrl;
    ecHttpSysWindowsOnly: Result := emHttpSysWindowsOnly;
    ecHTTP10NotRequestable: Result := emHTTP10NotRequestable;
    ecServerListenFailed: Result := emServerListenFailed;
    ecQuicRequiresTLS: Result := emQuicRequiresTLS;
    ecQuicFileNotFound: Result := emQuicFileNotFound;
    ecQuicLibrary: Result := emQuicLibrary;
    ecQuicApiFailed: Result := emQuicApiFailed;
    ecQuicListenFailed: Result := emQuicListenFailed;
    ecQuicHandshakeTimeout: Result := emQuicHandshakeTimeout;
    ecQuicConnectFailed: Result := emQuicConnectFailed;
    ecQuicFrameTooLarge: Result := emQuicFrameTooLarge;
    ecCompressInvalidFormat: Result := emCompressInvalidFormat;
    ecCompressFailed: Result := emCompressFailed;
    ecContentCheckError: Result := emContentCheckError;
    ecDBConnectionUndefined: Result := emDBConnectionUndefined;
    ecDBInvalidTableName: Result := emDBInvalidTableName;
    ecDBSQLRejected: Result := emDBSQLRejected;
    ecStreamSizeBeyondEnd: Result := emStreamSizeBeyondEnd;
    ecDecompressLimit: Result := emDecompressLimit;
    ecParamFileNameEmpty: Result := emParamFileNameEmpty;
    ecDBDriverMissing: Result := emDBDriverMissing;
    ecDBEmptyBody: Result := emDBEmptyBody;
    ecDBLinkMissing: Result := emDBLinkMissing;
    ecDBPoolTimeout: Result := emDBPoolTimeout;
    ecDBUpdateSQLMissing: Result := emDBUpdateSQLMissing;
    ecHMACEmptyText: Result := emHMACEmptyText;
    ecInvalidFormat: Result := emInvalidFormat;
    ecInvalidJSONFormat: Result := emInvalidJSONFormat;
    ecQueryVersionError: Result := emQueryVersionError;
    ecStorageClassNotFound: Result := emStorageClassNotFound;
    ecStorageLinkNotFound: Result := emStorageLinkNotFound;
    ecStorageNotFound: Result := emStorageNotFound;
    ecEngineNotFound: Result := emEngineNotFound;
    ecDBFieldLoad: Result := emDBFieldLoad;
    ecRandomBytesFailed: Result := emRandomBytesFailed;
    ecSaguiLibraryLoadError: Result := emSaguiLibraryLoadError;
    ecSaguiServerCreateError: Result := emSaguiServerCreateError;
    ecSaguiServerUnsupportedTLS: Result := emSaguiServerUnsupportedTLS;
    ecIPv6NotImplemented: Result := wmIPv6notImplemented;
    ecResponseCommitted: Result := emResponseCommitted;
    ecSSEEventTooBig: Result := emSSEEventTooBig;
    ecStreamCipher: Result := emStreamCipher;
    ecStreamLimit: Result := emStreamLimit;
    ecStreamNoLive: Result := emStreamNoLive;
    ecStreamNoServer: Result := emStreamNoServer;
    ecStreamCancelled: Result := emStreamCancelled;
    ecStreamClientCipher: Result := emStreamClientCipher;
    ecStreamClientCoding: Result := emStreamClientCoding;
    ecDBLegacyWireProc: Result := emDBLegacyWireProc;
    ecDBNoQueue: Result := emDBNoQueue;
    ecDBNoStoredProc: Result := emDBNoStoredProc;
    ecDBNoTransaction: Result := emDBNoTransaction;
    ecDBProcAmbiguous: Result := emDBProcAmbiguous;
    ecDBQueueOpen: Result := emDBQueueOpen;
    ecJWSInvalidKey: Result := emJWSInvalidKey;
    ecJWSNoPrivateKey: Result := emJWSNoPrivateKey;
    ecJWSOpenSSL: Result := emJWSOpenSSL;
    ecJWSUnsupportedKey: Result := emJWSUnsupportedKey;
    ecBigNumInvalidHex: Result := emBigNumInvalidHex;
    ecBigNumNegative: Result := emBigNumNegative;
    ecBigNumDivByZero: Result := emBigNumDivByZero;
    ecBigNumNoInverse: Result := emBigNumNoInverse;
    ecBigNumPrimeSize: Result := emBigNumPrimeSize;
    ecASN1Negative: Result := emASN1Negative;
    ecASN1InvalidOID: Result := emASN1InvalidOID;
    ecASN1Truncated: Result := emASN1Truncated;
    ecASN1Unsupported: Result := emASN1Unsupported;
    ecASN1UnexpectedTag: Result := emASN1UnexpectedTag;
    ecASN1InvalidTime: Result := emASN1InvalidTime;
    ecASN1Trailing: Result := emASN1Trailing;
    ecRSAInvalidKey: Result := emRSAInvalidKey;
    ecRSANoPrimes: Result := emRSANoPrimes;
    ecRSAKeySize: Result := emRSAKeySize;
    ecRSAExponent: Result := emRSAExponent;
    ecRSANotRSA: Result := emRSANotRSA;
    ecRSAEncryptedKey: Result := emRSAEncryptedKey;
    ecRSANoKeyInPEM: Result := emRSANoKeyInPEM;
    ecRSANotPrivate: Result := emRSANotPrivate;
    ecRSADigestSize: Result := emRSADigestSize;
    ecRSASignFault: Result := emRSASignFault;
    ecX509NoCertificate: Result := emX509NoCertificate;
    ecX509Invalid: Result := emX509Invalid;
    ecX509InvalidDetail: Result := emX509InvalidDetail;
    ecX509InvalidAddress: Result := emX509InvalidAddress;
    ecX509NoKey: Result := emX509NoKey;
    ecX509IssuerKeyMismatch: Result := emX509IssuerKeyMismatch;
    ecX509InvalidValidity: Result := emX509InvalidValidity;
    ecX509AESKey: Result := emX509AESKey;
    ecX509KeyMismatch: Result := emX509KeyMismatch;
    ecHttpSysCallFailed: Result := emHttpSysCallFailed;
    ecHttpSysNoCertificate: Result := emHttpSysNoCertificate;
    ecHttpSysImportFailed: Result := emHttpSysImportFailed;
    ecHttpSysNoKey: Result := emHttpSysNoKey;
    ecHttpSysStoreFailed: Result := emHttpSysStoreFailed;
    ecSynopseNoTls: Result := emSynopseNoTls;
    ecSelfSignedWriteFailed: Result := emSelfSignedWriteFailed;
    ecSelfSignedNoCertificate: Result := emSelfSignedNoCertificate;
    ecOAuth2StateMismatch: Result := emOAuth2StateMismatch;
    ecRouteBadConstraint: Result := emRouteBadConstraint;
    ecMethodUnsupported: Result := emMethodUnsupported;
    ecStoreLockTimeout: Result := emStoreLockTimeout;
    ecCircuitOpen: Result := emCircuitOpen;
    ecTLSError: Result := emTLSError;
    ecTLSClosed: Result := emTLSClosed;
    ecTLSTimeout: Result := emTLSTimeout;
    ecProxyRefused: Result := emProxyRefused;
    ecForwardedBadRange: Result := emForwardedBadRange;
    ecInProcNameTaken: Result := emInProcNameTaken;
    ecInProcNotFound: Result := emInProcNotFound;
    ecServiceNoServer: Result := emServiceNoServer;
    ecConfigInvalidJSON: Result := emConfigInvalidJSON;
    ecCronFieldCount: Result := emCronFieldCount;
    ecCronOutOfRange: Result := emCronOutOfRange;
    ecCronBadStep: Result := emCronBadStep;
    ecCronBadRange: Result := emCronBadRange;
    ecTemplateUnclosedTag: Result := emTemplateUnclosedTag;
    ecTemplateUnclosedSection: Result := emTemplateUnclosedSection;
    ecTemplateUnexpectedClose: Result := emTemplateUnexpectedClose;
    ecTemplateBadDelimiter: Result := emTemplateBadDelimiter;
    ecTemplateTooDeep: Result := emTemplateTooDeep;
    ecTemplateNotFound: Result := emTemplateNotFound;
    ecWebSocketUnsupported: Result := emWebSocketUnsupported;
    ecWebSocketNotConnected: Result := emWebSocketNotConnected;
    ecWebSocketRefused: Result := emWebSocketRefused;
    ecWebSocketBadURL: Result := emWebSocketBadURL;
    ecHubUnknownMethod: Result := emHubUnknownMethod;
    ecHubTimeout: Result := emHubTimeout;
    ecHubError: Result := emHubError;
    ecSessionObjectNotStored: Result := emSessionObjectNotStored;
    ecSessionReadOnly: Result := emSessionReadOnly;
    ecSessionCookieTooLarge: Result := emSessionCookieTooLarge;
    ecSessionNoSecret: Result := emSessionNoSecret;
    ecSessionNoStore: Result := emSessionNoStore;
    ecScanFailed: Result := emScanFailed;
    ecClamAVAnswer: Result := emClamAVAnswer;
    ecViewNoEngine: Result := emViewNoEngine;
    ecJobNoHandler: Result := emJobNoHandler;
    ecJobAttemptsExhausted: Result := emJobAttemptsExhausted;
    ecJobQueueActive: Result := emJobQueueActive;
    ecJobStoreWrite: Result := emJobStoreWrite;
    ecJobStoreNoDB: Result := emJobStoreNoDB;
  else
    Result := Format(emUnknownError, [AErrorCode]);
  end;
end;

end.