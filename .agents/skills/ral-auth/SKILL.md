---
name: ral-auth
description: >
  Adds login to a PascalRAL server and client: Basic, JWT, Digest or OAuth2 as
  authentication plugins (package RALPluginAuthPackage), public routes, reading the
  logged user, and the security plugins that go with it (brute force, flood, IP lists,
  CORS). Use for "require login", "JWT token", "OAuth2", "Digest", "401 everywhere",
  or "make this route public".
---

# Authentication

Every scheme is a pair: a `TRALServer...` plugin on the server and a `TRALClient...`
component on the client, both in the package **`RALPluginAuthPackage`** (unit
`RALAuthentication` for Basic and JWT, `RALDigest`, `RALOAuth2`). Link the server one
with `Server.Authentication := ...` (or as a plugin: several may be linked, and any
that accepts lets the request in); the client one with `Client.Authentication := ...`.

Login without HTTPS sends the secret in clear: use HTTPS in production (`ral-https`).

| scheme | how | use when |
|---|---|---|
| Basic | user and password on every request | internal systems, under HTTPS |
| JWT | the client trades credentials for a token at `/gettoken`, then sends the token | most APIs and apps |
| Digest | the password never travels, only a hash (RFC 7616) | devices and clients that already speak it |
| OAuth2 | an authorization server issues tokens for several systems | integrations, third-party login |

OAuth 1.0a does not exist in 2.0 (it never worked and is not OAuth2's predecessor).

## Public routes

`CreateRoute(...).SkipAuth([amGET])` (or `SkipAuthMethods`): those methods need no
login. OPTIONS and methods outside `Allow` are answered before any 401.

## Basic

```pascal
RALServerBasicAuth1.UserName := 'admin';           // or OnValidate for a database check
RALServerBasicAuth1.Password := 'secret';
Server.Authentication := RALServerBasicAuth1;

procedure TForm1.BasicValidate(ARequest: TRALRequest; AResponse: TRALResponse;
  var AResult: boolean);
begin
  with ARequest.Authorization.AsAuthBasic do
    AResult := CheckPassword(UserName, Password);   // your function
end;
```

Client: `RALClientBasicAuth1.UserName/Password`, `Client.Authentication := ...`.
`AuthDialog := True` makes a browser show its login box.

## JWT

```pascal
RALServerJWTAuth1.SignSecretKey := '<long secret, kept out of the source>';  // empty = 500
RALServerJWTAuth1.ExpirationSecs := 1800;
Server.Authentication := RALServerJWTAuth1;

procedure TForm1.JWTGetToken(ARequest: TRALRequest; AResponse: TRALResponse;
  AParams: TRALJWTParams; var AResult: boolean);
begin
  // read the credentials from ARequest.RequestText (JSON); never put the password in a claim
  AResult := CheckPassword(...);
  if AResult then
    AParams.AddClaim('user', ...);
end;
```

- **Without `OnGetToken` the server issues no token.** The handlers are methods
  (`of object`).
- Client: fill `RALClientJWTAuth1.Payload` with what `/gettoken` expects; `AutoGetToken`
  (default on) fetches and renews the token by itself.
- In a route: `ARequest.Authorization.AsAuthBearer.Payload.GetClaim('user')`.
- Other settings: `Algorithm` (HS256 default; RS/ES with a key), `AuthRoute`
  (`/gettoken`), `UseCookie` + `CookieSameSite` (token in the `raltoken` cookie, for
  web pages), `OnValidate` (per request), `OnRenewToken`.
- A token's content is readable by anyone; only the signature is protected.

## Digest

Server `TRALServerDigest`: `UserName`/`Password` or `OnGetCredential` (password or
H(A1)); `Algorithms` (SHA-256, SHA-512-256, MD5, each with `-sess`), `Realm`,
`NonceLifetime`, `ReplayProtection`. Client `TRALClientDigest`: `UserName`/`Password`.

## OAuth2

`TRALServerOAuth2` has two roles:

- **issue tokens** (`Issuing`, default on): `/oauth/token` (`client_credentials`,
  `authorization_code` with mandatory PKCE, `refresh_token`), `/oauth/authorize`,
  introspection, revocation, JWKS and metadata. `SignSecretKey`; clients and scopes are
  yours: `OnValidateClient`, `OnValidateScope`. No password grant, no implicit flow.
- **check tokens** (`Validation`): `ovLocal`, `ovJWKS` (`JWKSURL`), `ovIntrospection`
  (`IntrospectionURL`). `RouteScopes` (`route=scopes` per line) answers 403 without
  the scope.

Client `TRALClientOAuth2`: `TokenURL`, `ClientID`, `ClientSecret`, `Scope`; it renews
ahead of expiry. Desktop login through a browser: `TRALOAuth2Loopback`.

## Protect the login

On a server reachable from outside, link with the authentication:
`TRALBruteForcePlugin` (blocks an address after `MaxTry` refused secrets - only real
guesses count), `TRALFloodPlugin`, `TRALBlackListPlugin`/`TRALWhiteListPlugin`
(`RALSecurity`), and `TRALCORSPlugin` with explicit origins (`RALCORS`). Turn on
`HideErrorDetails` on the server.

## Common errors

| symptom | cause |
|---|---|
| 401 everywhere | the client has no `Authentication`, or the pair does not match |
| 401 on a route meant to be public | `SkipAuth` missing on that route |
| JWT 500 about `SignSecretKey` | empty key |
| JWT issues no token | `OnGetToken` not assigned, or it left `AResult` False |
| 403 after several tries | brute force blocked the address for `ExpirationTime` |
| `Class TRALServerJWTAuth not found` opening a form | `RALPluginAuthPackage` not installed |

Guide (Portuguese): wiki
[autenticacao](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/autenticacao),
[seguranca](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/seguranca).
