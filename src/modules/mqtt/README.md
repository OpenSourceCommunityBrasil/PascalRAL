# MQTT in PascalRAL

`RALModuleMQTTPackage` is a complete MQTT stack in Object Pascal: client and broker for
MQTT 3.1.1 and 5.0, MQTT-SN 1.2 and 2.0 (draft) client and gateway, and a bridge to the
REST server. It needs no third-party library except OpenSSL for TLS, loaded at run time.
This file is the map for whoever changes it; the user guide is the wiki (`mqtt*.md`
pages), and every normative statement of the standards is traced in
`.agents/MQTT_CONFORMANCE.md`.

## Packages

| package | holds | requires |
|---|---|---|
| `RALModuleMQTTPackage` (runtime) | every unit of this folder | `PascalRAL`, `RALPluginAuthPackage` |
| `RALModuleMQTTDsgn` (design) | `design/RALModuleMQTTReg`: palette, "Test connection", "Test ports", the publication list of `TRALMQTTPublish` | `RALModuleMQTTPackage`, `designide` |
| `RALModuleMQTTDBPackage` (run+design, optional) | `db/RALMQTTDataSetLink`: messages into any `TDataSet` | `RALModuleMQTTPackage`, `RALDBPackage` |

`RALPluginAuthPackage` is required because `TRALMQTTNetTokenAuth` takes its token from
a `TRALClientJWTAuth` or `TRALClientOAuth2`.

## Layers

| layer | units |
|---|---|
| codec | `RALMQTTTypes` (reason codes, properties, reader/writer, UTF-8 rules, monotonic clock), `RALMQTTPacket` (the 15 packets and AUTH, `TRALMQTTFrameReader`), `RALMQTTTopic` (names, filters, matching, `$share`, the broker's topic tree) |
| transport | `RALMQTTSocket` (one socket code for every OS: ws2_32 or libc declarations, `poll`/`WSAPoll`, Happy Eyeballs), `RALMQTTTLS` (OpenSSL TLS and DTLS, client and server), `RALMQTTWebSocket` (RFC 6455, both sides, subprotocol `mqtt`), `RALMQTTProxy` (HTTP CONNECT, SOCKS5) |
| session | `RALMQTTSession` (packet ids, topic aliases, broker URIs), `RALMQTTStore` (client session file with a CRC per record), `RALMQTTBrokerStore` (broker journal) |
| authentication | `RALMQTTAuth`: SCRAM-SHA-1/256/512 both sides, PBKDF2, and the client authenticators |
| client | `RALMQTTClient` (`TRALMQTTCustomClient`, `TRALMQTTClient`), `RALMQTTActions` |
| MQTT-SN | `RALMQTTSNTypes` (one packet object, the codec interface), `RALMQTTSNPacket12`, `RALMQTTSNPacket20`, `RALMQTTSNClient`, `RALMQTTSNGateway` |
| broker | `RALMQTTBroker` (`TRALMQTTBroker`, a `TRALServer`), `RALMQTTRestModule` |

## The client

- `BrokerURL` takes `mqtt://`, `mqtts://`, `ws://`, `wss://` (also `tcp`, `ssl`, `tls`);
  `BrokerURLs` lists alternatives tried in turn. `ProtocolVersion` is 5.0 by default and
  `VersionFallback` retries with 3.1.1 when the broker refuses 5.0, closes or stays
  silent before the CONNACK.
- One thread per client runs the connection. Events run on the main thread
  (`SynchronizeEvents`), except in a console program, which has no loop to run them.
  `AckMode`: `amAfterHandler` (default) acknowledges after `OnMessage` returns, so a
  program that dies in the handler gets the message again.
- `Subscriptions` (with `OnMessage`, `Messages`, `MessagesControl` per item) and
  `Publications` (published by name or by `TRALMQTTPublish`) are collections for the
  Object Inspector. A component derived from `TRALMQTTMessageListener` hears every
  message through `AddListener` (that is how `TRALMQTTDataSetLink` works).
- QoS 1 and 2 are stored before they are sent: `SessionStore` `mskFile` (default, in
  `SessionFolder`, `SyncWrites`) or `mskMemory`. Messages published while offline wait
  in the queue (`OfflineQueueSize`).
- `TestConnection` connects a copy of the connection settings and reports; the IDE's
  "Test connection" calls it. The component itself is never connected by the IDE.

### Certificates

`SSL.Pins`, `SSL.Verify` and `OnValidateServerCert` follow the rule of `TRALClient`
(`src/engine/SSL.md`): the event decides, else a pin, else OpenSSL's verdict (chain and
host name). `RALMQTTAcceptCertificate` is that rule. A client certificate is
`SSL.CertFile`/`SSL.KeyFile`.

### Authentication

| component | for |
|---|---|
| `TRALMQTTBasicAuth` | user name and password in the CONNECT |
| `TRALMQTTSCRAMAuth` | SCRAM-SHA-1/256/512 (5.0 enhanced authentication) |
| `TRALMQTTNetTokenAuth` | a token from a JWT or OAuth2 authenticator of the REST client |
| `TRALMQTTSelfTokenAuth` | a JWT the client signs itself (cloud brokers that want one) |
| `TRALMQTTSASAuth` | shared access signatures (Azure IoT Hub, Event Grid) |
| `TRALMQTTCustomAuth` | any other AUTH method, by events |

## The broker

`TRALMQTTBroker` descends from `TRALServer`. Each CONNECT goes through
`ValidateRequest` and `ProcessCommands` on an internal route `mqtt/connect`, so the
server's security plugins (lists, flood, brute force) and authenticators judge MQTT
logins as they judge HTTP requests. The HTTP properties that mean nothing to a broker
are hidden in the IDE (`IsPropertyRelevant`).

- Ports: `Port` (TCP), `TLSPort` (needs `SSL.Enabled` and a certificate),
  `WebSocketPort` (`WebSocketTLS` for wss, `WebSocketPath`).
- Users: an authenticator linked to the server, else `Users` (`name=password`, also the
  SCRAM users), else `OnConnect`. `AllowAnonymous` is False by default, as in Mosquitto 2.
- `Permissions`: rules of user, client id, topic filter (`%u`, `%c` stand for the user
  and the client), access and allow; the first that matches decides. With no rule
  everything is allowed; with rules, what no rule matches is refused.
- Retained messages, will with delay, shared subscriptions, session expiry, topic
  aliases, Receive Maximum, `$SYS` topics (`SysTopics`), takeover of a client id.
- The journal (`SessionStore = mskFile`, in `DataFolder`) keeps sessions, queued
  messages and retained messages across a restart; writes are grouped and synced before
  the acknowledgements that depend on them.
- Each connection has a send window that grows like TCP slow start; after connections
  that ended soon it starts small. That is what keeps a client on a link that keeps
  dropping from live-locking on redeliveries.
- `TestPorts` says whether the ports are free and the certificate loads; the IDE's
  "Test ports" calls it.

### Extensions

`TRALMQTTBrokerExtension` is how something that is not a TCP connection becomes a
client of the broker: `AttachSession`, `SubscribeSession`, `PublishFrom`,
`TakeQueued`/`ReturnQueued`, `DetachSession`, `SetExternal` (a wake callback, called
under the broker's lock: only signal there). The MQTT-SN gateway is the extension in
this package.

## MQTT-SN

- The client (`TRALMQTTSNClient`, version in `SNVersion`) keeps one request in flight,
  as the standard asks, resent every `RetryInterval` up to `RetryCount`; past that the
  gateway is taken as lost. QoS -1 (`PublishMinusOne`) needs no connection. `Sleep_`
  and `Wake` are the sleeping-client procedure. `DTLS` uses the same TLS options.
- The gateway (`TRALMQTTSNGateway`, linked by `Broker`) is aggregating: each MQTT-SN
  client is a session of the broker itself. One thread, plain UDP on `Port`, DTLS on
  `DTLSPort` with the broker's certificate, 1.2 and 2.0 side by side
  (`Versions`). A client that is not known yet is told apart by its CONNECT: a 1.2
  CONNECT has protocol id 1 where 2.0 has its correlation id, so the whole 1.2 reading
  is checked (`RALMQTTSNConnectIs12`). A client the gateway gave up on gets a
  DISCONNECT when it talks again, so it knows to reconnect.
- 1.2 learns topic ids through REGISTER (both ways); 2.0 sends the topic name until the
  client has an alias for it. In 2.0 the SUBACK reason code is the granted QoS.
- Not implemented: the 2.0 PROTECTION envelope.

## The REST bridge

`TRALMQTTRestModule` is a module of any RAL HTTP server, linked to a broker:
`POST <Domain>/publish/<topic>` (`qos`, `retain` from the query or `X-MQTT-QoS`,
`X-MQTT-Retain`), `GET <Domain>/retained/<topic>`, `GET <Domain>/clients`,
`GET <Domain>/stats`. The server's plugins and authentication apply; the topic goes
through the broker's `Permissions` with the HTTP user (Basic name or token subject).

## Traps

- **Wire text on Delphi.** `string` is UTF-16 on Delphi and 8-bit on FPC. Text that
  goes on the wire (HTTP of the WebSocket handshake, proxy requests) must be built in
  `AnsiString`/`StringRAL`; a `Move(Pointer(S)^, ..., Length(S))` of a `string` sends
  half of a UTF-16 text on Delphi and works on FPC.
- **Poll constants differ on Windows** (`POLLIN` is `POLLRDNORM or POLLRDBAND`):
  `RALMQTTPoll` maps them; never pass the POSIX numbers to `WSAPoll`.
- **SIGPIPE** kills a POSIX process that writes to a closed TLS socket: it is ignored
  when the TLS unit loads, unless the application installed a handler.
- **Mosquitto 2.0 resends its whole in-flight set at each reconnect**, beyond the
  Receive Maximum the new CONNECT announces. On a link that is cut after a few hundred
  bytes the acknowledgements never get back; a subscriber there keeps
  `ReceiveMaximum` small from the first connection.
- **The broker's wake callback runs under its lock**: an extension only signals there
  and takes the messages from its own thread (`TakeQueued`).

## Tests

The orchestrator suite `mqtt`/`mqttfpc` (`RALOrquestrador/suite/mqtt`) runs against the
broker of RAL itself: exchange in both versions, WebSocket, TLS with a pin, a link that
cuts connections, MQTT-SN 1.2 and 2.0 with datagram loss, and the REST bridge.
Interoperability with Mosquitto, Paho (Python) and an independent MQTT-SN 1.2 encoder
was checked during development; see the plan's state for what each covered.
