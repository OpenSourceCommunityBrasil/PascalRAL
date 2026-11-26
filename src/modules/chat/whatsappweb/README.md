# WhatsApp Web channel (`TRALChatWhatsAppWeb`)

The application joins as a linked device of an ordinary WhatsApp number and speaks the
WhatsApp Web protocol itself: WebSocket, Noise XX, the binary node format, Signal end to
end. No browser and no injected script: a change in the web page does not reach the
channel; a change in the protocol does. It violates WhatsApp's terms of service, the
number can be banned, and the protocol changes without notice.

## Reference implementations

The protocol is reverse-engineered. What the channel does follows these, read on
2026-11-26:

| project | language | licence | commit |
|---|---|---|---|
| [whatsmeow](https://github.com/tulir/whatsmeow) | Go | MPL-2.0 | `c2ce5c339ae0642ef60e7ed20ea61ae07aca1be0` (main, 2026-11-26, "send: update message metadata") |
| [Baileys](https://github.com/WhiskeySockets/Baileys) | TypeScript | MIT | `bb0a87e0326ca8f882fe716c7ee42da362f73fc0` (master, 2026-11-13) |

whatsmeow is the main reference; Baileys only for the keepalive node and for reading the
current version from `web.whatsapp.com/sw.js`. When WhatsApp changes the protocol, both
usually follow within days: compare them from these commits to their head and port what
changed.

## Where each part of the protocol lives

| part | unit | whatsmeow |
|---|---|---|
| frames, Noise XX, transport cipher | `RALWhatsAppNoise` | `socket/`, `handshake.go` |
| server certificate chain, root key `cWACertRoot` | `RALChatWhatsAppWeb` (`RALWhatsAppVerifyServerCert`) | `handshake.go` |
| client payload: version, `BuildHash`, device props | `RALChatWhatsAppWeb` (`ClientPayload`, `ResolveVersion`) | `store/clientpayload.go` |
| binary nodes, token dictionary | `RALWhatsAppBinary` | `binary/` |
| protobuf messages | `whatsappweb.proto` -> `RALWhatsAppProto` | `proto/` |
| Signal: X3DH, Double Ratchet, sender keys | `RALWhatsAppSignal` | `go.mau.fi/libsignal` |
| QR and code pairing, ADV signatures | `RALChatWhatsAppWeb` (`HandlePairDevice`, `RequestPairingCode`, `HandleCodePair`, `DoPairSuccess`) | `pair.go`, `pair-code.go`, `qrchan.go` |
| login follow-up: prekeys, passive/active | `RALChatWhatsAppWeb` (`PostLogin`, `UploadPreKeys`) | `connectionevents.go`, `prekeys.go` |
| failures, stream errors, keepalive | `RALChatWhatsAppWeb` (`HandleFailure`, `HandleStreamError`, `Tick`) | `connectionevents.go`, `keepalive.go` |
| sending: device lists, fan-out, groups, edit, revoke, reaction | `RALChatWhatsAppWeb` (`SendChatMessage`, `BuildContent`, `GetUserDevices`) | `send.go`, `user.go` |
| receiving, receipts, retry receipts | `RALChatWhatsAppWeb` (`HandleMessage`, `SendMessageReceipt`, `SendRetryReceipt`, `HandleRetryReceipt`) | `message.go`, `receipt.go`, `retry.go` |
| media: encryption, upload, `media_conn` | `RALWhatsAppMedia`, `RALChatWhatsAppWeb` (`UploadMedia`, `LoadMedia`) | `upload.go`, `download.go`, `mediaconn.go` |
| calls | `RALChatWhatsAppWeb` (`AnswerCall`) | `call.go` |

Not implemented, though the reference has them: the phone-number/LID mapping (sessions
and addresses by LID), the privacy tokens of direct messages (`tctoken`/`cstoken`),
hosted devices (business accounts on Meta's cloud) and the participant hash (`phash`) of
group messages.

## When the protocol changes

1. **Read the symptom.** `OnError`/`OnLog` of the bot, `OnStopped`/`OnPaused` of the
   channel. A login failure 405 is the version (`ClientVersion`, or the reading of
   `sw.js` in `ClientRevisionOf`); a handshake or certificate error is the Noise code or
   `cWACertRoot`; an `ack` with an error (487 to 552) or a message that never arrives is
   the stanza; a node that does not unpack is the token dictionary.
2. **Watch the nodes.** Assign `OnTrace`: every node in and out as text, binary content
   shown by its size and phone numbers cut. Compare what goes out with what the
   reference builds for the same thing.
3. **Compare the reference.** Diff whatsmeow from the commit above to its head, in the
   file of the table that matches the symptom, and port what changed.
4. **Protobuf fields.** Add them to `whatsappweb.proto` (the field numbers are the
   protocol's) and regenerate; `ralprotoc` reproduces the current unit byte for byte, so
   the diff shows only the change:
   `ralprotoc -unit=RALWhatsAppProto -nogrpc whatsappweb.proto`
5. **Check.** Compile on every platform (`ral-check-unit`), build the packages
   (`ral-build`), then try against the real service with a test number, never the
   production one.
6. **Update this file**: the new reference commits and what changed.

## Not losing the number

Logging in again and again is what marks a number. The channel waits between connects
(2 s doubled up to `ReconnectMaxDelay`, back to 2 s only after 5 minutes online),
counts its connects in the store (`ReconnectLimit` per `ReconnectWindow`, across
restarts), keeps a ban or a replaced session as a pause in the store (`OnPaused`), and
stops for good on what WhatsApp refused for good (`OnStopped`). `ClearPause` lifts a
pause once its cause is gone. While a fix is not ready, the bot can move to the official
API (`TRALChatWhatsApp`) or a gateway (`TRALChatGateway`) without changing its own code.
