# MQTT conformance matrix

Every normative statement of the MQTT standards, the unit of `RALModuleMQTTPackage` that answers for it and the test that proves its area. Generated from the OASIS documents; the statement text is not reproduced here - read it in the standard by its number.

- **binds**: who must obey it (client, server, or both: sender/receiver statements).
- **proven by**: the test programs of the scratch battery (`codec`, `transport`, `auth`, `client` against Mosquitto and against the RAL broker, `paho` - the Eclipse Paho Python client against the RAL broker, `adapter`, `persist`, `reliability`, `sn`, `rest`, `ide`) and the orchestrator suite `mqtt`/`mqttfpc`.
- **cited in**: the units whose source names the statement next to the code that implements it; `-` means the area is covered by the unit and its tests without a citation in the source.

Not implemented on purpose: MQTT over QUIC (future step MQ18); the MQTT-SN 2.0 PROTECTION envelope.

## MQTT 5.0 (OASIS Standard, 7 March 2019)

252 statements; cited in code: 96.

| statement | section | binds | unit | proven by | cited in |
|---|---|---|---|---|---|
| MQTT-1.5.4-1 | 1.5.4 UTF-8 Encoded String | both | RALMQTTTypes | codec | RALMQTTTypes |
| MQTT-1.5.4-2 | 1.5.4 UTF-8 Encoded String | both | RALMQTTTypes | codec | RALMQTTTypes |
| MQTT-1.5.4-3 | 1.5.4 UTF-8 Encoded String | both | RALMQTTTypes | codec | - |
| MQTT-1.5.5-1 | 1.5.5 Variable Byte Integer | both | RALMQTTTypes | codec | RALMQTTTypes |
| MQTT-1.5.7-1 | 1.5.7 UTF-8 String Pair | both | RALMQTTTypes | codec | - |
| MQTT-2.1.3-1 | 2.1.3 Flags | both | RALMQTTPacket, RALMQTTTypes | codec | RALMQTTPacket |
| MQTT-2.2.1-2 | 2.2.1 Packet Identifier | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.2.1-3 | 2.2.1 Packet Identifier | client | RALMQTTPacket, RALMQTTTypes | codec | RALMQTTPacket, RALMQTTSession |
| MQTT-2.2.1-4 | 2.2.1 Packet Identifier | server | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.2.1-5 | 2.2.1 Packet Identifier | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.2.1-6 | 2.2.1 Packet Identifier | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.2.2-1 | 2.2.2 Properties | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-3.1.0-1 | 3.1 CONNECT - Connection Request | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker, RALMQTTClient |
| MQTT-3.1.0-2 | 3.1 CONNECT - Connection Request | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-1 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-2 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient, RALMQTTPacket |
| MQTT-3.1.2-3 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.1.2-4 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-5 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-6 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-7 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-8 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-9 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-10 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-11 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.1.2-12 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-13 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.1.2-14 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-15 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-16 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-17 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-18 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-19 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-20 | 3.1.2 CONNECT Variable Header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient |
| MQTT-3.1.2-21 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient |
| MQTT-3.1.2-22 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker, RALMQTTPacket |
| MQTT-3.1.2-23 | 3.1.2 CONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-24 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-25 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker, RALMQTTPacket |
| MQTT-3.1.2-26 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-27 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-28 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-29 | 3.1.2 CONNECT Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-30 | 3.1.2 CONNECT Variable Header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.3-1 | 3.1.3 CONNECT Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.1.3-2 | 3.1.3 CONNECT Payload | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-3 | 3.1.3 CONNECT Payload | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-4 | 3.1.3 CONNECT Payload | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-5 | 3.1.3 CONNECT Payload | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-6 | 3.1.3 CONNECT Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.3-7 | 3.1.3 CONNECT Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-8 | 3.1.3 CONNECT Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.3-9 | 3.1.3 CONNECT Payload | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.3-10 | 3.1.3 CONNECT Payload | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-11 | 3.1.3 CONNECT Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-12 | 3.1.3 CONNECT Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.4-1 | 3.1.4 CONNECT Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.4-2 | 3.1.4 CONNECT Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.4-3 | 3.1.4 CONNECT Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.4-4 | 3.1.4 CONNECT Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.4-5 | 3.1.4 CONNECT Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.4-6 | 3.1.4 CONNECT Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.2.0-1 | 3.2 CONNACK - Connect acknowledgement | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker, RALMQTTClient |
| MQTT-3.2.0-2 | 3.2 CONNACK - Connect acknowledgement | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient |
| MQTT-3.2.2-1 | 3.2.2 CONNACK Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.2.2-2 | 3.2.2 CONNACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.2.2-3 | 3.2.2 CONNACK Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-4 | 3.2.2 CONNACK Variable Header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-5 | 3.2.2 CONNACK Variable Header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient |
| MQTT-3.2.2-6 | 3.2.2 CONNACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.2.2-7 | 3.2.2 CONNACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.2.2-8 | 3.2.2 CONNACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-9 | 3.2.2 CONNACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-10 | 3.2.2 CONNACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.2.2-11 | 3.2.2 CONNACK Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient |
| MQTT-3.2.2-12 | 3.2.2 CONNACK Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.2.2-13 | 3.2.2 CONNACK Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.2.2-14 | 3.2.2 CONNACK Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-15 | 3.2.2 CONNACK Variable Header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient |
| MQTT-3.2.2-16 | 3.2.2 CONNACK Variable Header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.2.2-17 | 3.2.2 CONNACK Variable Header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-18 | 3.2.2 CONNACK Variable Header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-19 | 3.2.2 CONNACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-20 | 3.2.2 CONNACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-21 | 3.2.2 CONNACK Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.2.2-22 | 3.2.2 CONNACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-1 | 3.3.1 PUBLISH Fixed Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-2 | 3.3.1 PUBLISH Fixed Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.3.1-3 | 3.3.1 PUBLISH Fixed Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-4 | 3.3.1 PUBLISH Fixed Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.3.1-5 | 3.3.1 PUBLISH Fixed Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.1-6 | 3.3.1 PUBLISH Fixed Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-7 | 3.3.1 PUBLISH Fixed Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-8 | 3.3.1 PUBLISH Fixed Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-9 | 3.3.1 PUBLISH Fixed Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.1-10 | 3.3.1 PUBLISH Fixed Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-11 | 3.3.1 PUBLISH Fixed Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-12 | 3.3.1 PUBLISH Fixed Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.1-13 | 3.3.1 PUBLISH Fixed Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.2-1 | 3.3.2 PUBLISH Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-2 | 3.3.2 PUBLISH Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.3.2-3 | 3.3.2 PUBLISH Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-4 | 3.3.2 PUBLISH Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.2-5 | 3.3.2 PUBLISH Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.2-6 | 3.3.2 PUBLISH Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.2-7 | 3.3.2 PUBLISH Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTSession |
| MQTT-3.3.2-8 | 3.3.2 PUBLISH Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-9 | 3.3.2 PUBLISH Variable Header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-10 | 3.3.2 PUBLISH Variable Header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTSession |
| MQTT-3.3.2-11 | 3.3.2 PUBLISH Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-12 | 3.3.2 PUBLISH Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTSession |
| MQTT-3.3.2-13 | 3.3.2 PUBLISH Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-14 | 3.3.2 PUBLISH Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-15 | 3.3.2 PUBLISH Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-16 | 3.3.2 PUBLISH Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-17 | 3.3.2 PUBLISH Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-18 | 3.3.2 PUBLISH Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-19 | 3.3.2 PUBLISH Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-20 | 3.3.2 PUBLISH Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.4-1 | 3.3.4 PUBLISH Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.4-2 | 3.3.4 PUBLISH Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.4-3 | 3.3.4 PUBLISH Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.4-4 | 3.3.4 PUBLISH Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.4-5 | 3.3.4 PUBLISH Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.4-6 | 3.3.4 PUBLISH Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker, RALMQTTClient |
| MQTT-3.3.4-7 | 3.3.4 PUBLISH Actions | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.4-8 | 3.3.4 PUBLISH Actions | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.4-9 | 3.3.4 PUBLISH Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.4-10 | 3.3.4 PUBLISH Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.4.2-1 | 3.4.2 PUBACK Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.4.2-2 | 3.4.2 PUBACK Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.4.2-3 | 3.4.2 PUBACK Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.5.2-1 | 3.5.2 PUBREC Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.5.2-2 | 3.5.2 PUBREC Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.5.2-3 | 3.5.2 PUBREC Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.6.1-1 | 3.6.1 PUBREL Fixed Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.6.2-1 | 3.6.2 PUBREL Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.6.2-2 | 3.6.2 PUBREL Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.6.2-3 | 3.6.2 PUBREL Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.7.2-1 | 3.7.2 PUBCOMP Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.7.2-2 | 3.7.2 PUBCOMP Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.7.2-3 | 3.7.2 PUBCOMP Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.8.1-1 | 3.8.1 SUBSCRIBE Fixed Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTPacket |
| MQTT-3.8.3-1 | 3.8.3 SUBSCRIBE Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.8.3-2 | 3.8.3 SUBSCRIBE Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTPacket |
| MQTT-3.8.3-3 | 3.8.3 SUBSCRIBE Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTBroker |
| MQTT-3.8.3-4 | 3.8.3 SUBSCRIBE Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTBroker |
| MQTT-3.8.3-5 | 3.8.3 SUBSCRIBE Payload | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTPacket |
| MQTT-3.8.4-1 | 3.8.4 SUBSCRIBE Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.8.4-2 | 3.8.4 SUBSCRIBE Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTBroker |
| MQTT-3.8.4-3 | 3.8.4 SUBSCRIBE Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTBroker |
| MQTT-3.8.4-4 | 3.8.4 SUBSCRIBE Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.8.4-5 | 3.8.4 SUBSCRIBE Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.8.4-6 | 3.8.4 SUBSCRIBE Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.8.4-7 | 3.8.4 SUBSCRIBE Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.8.4-8 | 3.8.4 SUBSCRIBE Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTBroker |
| MQTT-3.9.2-1 | 3.9.2 SUBACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.9.2-2 | 3.9.2 SUBACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.9.3-1 | 3.9.3 SUBACK Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.9.3-2 | 3.9.3 SUBACK Payload | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.1-1 | 3.10.1 UNSUBSCRIBE Fixed Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.10.3-1 | 3.10.3 UNSUBSCRIBE Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.3-2 | 3.10.3 UNSUBSCRIBE Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.10.4-1 | 3.10.4 UNSUBSCRIBE Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.4-2 | 3.10.4 UNSUBSCRIBE Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.4-3 | 3.10.4 UNSUBSCRIBE Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.4-4 | 3.10.4 UNSUBSCRIBE Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.4-5 | 3.10.4 UNSUBSCRIBE Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.4-6 | 3.10.4 UNSUBSCRIBE Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.11.2-1 | 3.11.2 UNSUBACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.11.2-2 | 3.11.2 UNSUBACK Variable Header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.11.3-1 | 3.11.3 UNSUBACK Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.11.3-2 | 3.11.3 UNSUBACK Payload | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.12.4-1 | 3.12.4 PINGREQ Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.14.0-1 | 3.14 DISCONNECT - Disconnect notification | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.1-1 | 3.14.1 DISCONNECT Fixed Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.2-1 | 3.14.2 DISCONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.2-2 | 3.14.2 DISCONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.2-3 | 3.14.2 DISCONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.2-4 | 3.14.2 DISCONNECT Variable Header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.4-1 | 3.14.4 DISCONNECT Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.4-2 | 3.14.4 DISCONNECT Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.4-3 | 3.14.4 DISCONNECT Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker, RALMQTTSNGateway |
| MQTT-3.15.1-1 | 3.15.1 AUTH Fixed Header | both | RALMQTTPacket, RALMQTTAuth, RALMQTTClient, RALMQTTBroker | codec, auth, adapter | - |
| MQTT-3.15.2-1 | 3.15.2 AUTH Variable Header | both | RALMQTTPacket, RALMQTTAuth, RALMQTTClient, RALMQTTBroker | codec, auth, adapter | - |
| MQTT-3.15.2-2 | 3.15.2 AUTH Variable Header | both | RALMQTTPacket, RALMQTTAuth, RALMQTTClient, RALMQTTBroker | codec, auth, adapter | - |
| MQTT-3.15.2-3 | 3.15.2 AUTH Variable Header | both | RALMQTTPacket, RALMQTTAuth, RALMQTTClient, RALMQTTBroker | codec, auth, adapter | - |
| MQTT-4.1.0-1 | 4.1 Session State | both | RALMQTTClient, RALMQTTBroker, RALMQTTStore, RALMQTTBrokerStore | client, persist, reliability | - |
| MQTT-4.1.0-2 | 4.1 Session State | server | RALMQTTClient, RALMQTTBroker, RALMQTTStore, RALMQTTBrokerStore | client, persist, reliability | RALMQTTBroker |
| MQTT-4.2.0-1 | 4.2 Network Connections | both | RALMQTTSocket | transport | - |
| MQTT-4.2-1 | 4.2 Network Connections | both | RALMQTTSocket | transport | - |
| MQTT-4.3.1-1 | 4.3.1 QoS 0: At most once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.2-1 | 4.3.2 QoS 1: At least once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.2-2 | 4.3.2 QoS 1: At least once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.2-3 | 4.3.2 QoS 1: At least once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.2-4 | 4.3.2 QoS 1: At least once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.2-5 | 4.3.2 QoS 1: At least once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-1 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-2 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-3 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-4 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | RALMQTTBroker |
| MQTT-4.3.3-5 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-6 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | RALMQTTClient |
| MQTT-4.3.3-7 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-8 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-9 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-10 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-11 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | RALMQTTBroker |
| MQTT-4.3.3-12 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-13 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.4.0-1 | 4.4 Message delivery retry | both | RALMQTTClient, RALMQTTBroker | reliability | RALMQTTBroker, RALMQTTClient |
| MQTT-4.4.0-2 | 4.4 Message delivery retry | both | RALMQTTClient, RALMQTTBroker | reliability | RALMQTTBroker, RALMQTTClient |
| MQTT-4.5.0-1 | 4.5 Message receipt | server | RALMQTTClient, RALMQTTBroker | client, paho | - |
| MQTT-4.5.0-2 | 4.5 Message receipt | client | RALMQTTClient, RALMQTTBroker | client, paho | RALMQTTClient |
| MQTT-4.6.0-1 | 4.6 Message ordering | client | RALMQTTClient, RALMQTTBroker | client, paho, reliability | RALMQTTClient |
| MQTT-4.6.0-2 | 4.6 Message ordering | client | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.6.0-3 | 4.6 Message ordering | client | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.6.0-4 | 4.6 Message ordering | client | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.6.0-5 | 4.6 Message ordering | server | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.6.0-6 | 4.6 Message ordering | server | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.7.0-1 | 4.7 Topic Names and Topic Filters | both | RALMQTTTopic | codec, client | RALMQTTTopic |
| MQTT-4.7.1-1 | 4.7.1 Topic wildcards | both | RALMQTTTopic | codec, client | RALMQTTTopic |
| MQTT-4.7.1-2 | 4.7.1 Topic wildcards | both | RALMQTTTopic | codec, client | RALMQTTTopic |
| MQTT-4.7.2-1 | 4.7.2 -Topics beginning with $ | server | RALMQTTTopic | codec, client | RALMQTTTopic |
| MQTT-4.7.3-1 | 4.7.3 Topic semantic and usage | both | RALMQTTTopic | codec, client | RALMQTTPacket, RALMQTTTopic |
| MQTT-4.7.3-2 | 4.7.3 Topic semantic and usage | both | RALMQTTTopic | codec, client | - |
| MQTT-4.7.3-3 | 4.7.3 Topic semantic and usage | both | RALMQTTTopic | codec, client | - |
| MQTT-4.7.3-4 | 4.7.3 Topic semantic and usage | server | RALMQTTTopic | codec, client | RALMQTTTopic |
| MQTT-4.8.2-1 | 4.8.2 Shared Subscriptions | both | RALMQTTBroker | client (RAL broker), paho | RALMQTTTopic |
| MQTT-4.8.2-2 | 4.8.2 Shared Subscriptions | both | RALMQTTBroker | client (RAL broker), paho | RALMQTTTopic |
| MQTT-4.8.2-3 | 4.8.2 Shared Subscriptions | both | RALMQTTBroker | client (RAL broker), paho | - |
| MQTT-4.8.2-4 | 4.8.2 Shared Subscriptions | both | RALMQTTBroker | client (RAL broker), paho | - |
| MQTT-4.8.2-5 | 4.8.2 Shared Subscriptions | both | RALMQTTBroker | client (RAL broker), paho | - |
| MQTT-4.8.2-6 | 4.8.2 Shared Subscriptions | both | RALMQTTBroker | client (RAL broker), paho | - |
| MQTT-4.9.0-1 | 4.9 Flow Control | both | RALMQTTClient, RALMQTTBroker | reliability | - |
| MQTT-4.9.0-2 | 4.9 Flow Control | both | RALMQTTClient, RALMQTTBroker | reliability | RALMQTTBroker, RALMQTTClient |
| MQTT-4.9.0-3 | 4.9 Flow Control | both | RALMQTTClient, RALMQTTBroker | reliability | - |
| MQTT-4.12.0-1 | 4.12 Enhanced authentication | both | RALMQTTAuth, RALMQTTClient, RALMQTTBroker | auth, adapter, paho | - |
| MQTT-4.12.0-2 | 4.12 Enhanced authentication | both | RALMQTTAuth, RALMQTTClient, RALMQTTBroker | auth, adapter, paho | RALMQTTBroker |
| MQTT-4.12.0-3 | 4.12 Enhanced authentication | both | RALMQTTAuth, RALMQTTClient, RALMQTTBroker | auth, adapter, paho | - |
| MQTT-4.12.0-4 | 4.12 Enhanced authentication | both | RALMQTTAuth, RALMQTTClient, RALMQTTBroker | auth, adapter, paho | - |
| MQTT-4.12.0-5 | 4.12 Enhanced authentication | both | RALMQTTAuth, RALMQTTClient, RALMQTTBroker | auth, adapter, paho | RALMQTTBroker |
| MQTT-4.12.0-6 | 4.12 Enhanced authentication | both | RALMQTTAuth, RALMQTTClient, RALMQTTBroker | auth, adapter, paho | - |
| MQTT-4.12.0-7 | 4.12 Enhanced authentication | client | RALMQTTAuth, RALMQTTClient, RALMQTTBroker | auth, adapter, paho | - |
| MQTT-4.12.1-1 | 4.12.1 Re-authentication | client | RALMQTTAuth, RALMQTTClient, RALMQTTBroker | auth, adapter, paho | RALMQTTBroker, RALMQTTClient |
| MQTT-4.12.1-2 | 4.12.1 Re-authentication | both | RALMQTTAuth, RALMQTTClient, RALMQTTBroker | auth, adapter, paho | RALMQTTBroker |
| MQTT-4.13.1-1 | 4.13.1 Malformed Packet and Protocol Errors | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, paho | RALMQTTBroker |
| MQTT-4.13.2-1 | 4.13.2 Other errors | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, paho | - |
| MQTT-6.0.0-1 | 6 Using WebSocket as a network transport | both | RALMQTTWebSocket | transport, paho | RALMQTTWebSocket |
| MQTT-6.0.0-2 | 6 Using WebSocket as a network transport | both | RALMQTTWebSocket | transport, paho | RALMQTTWebSocket |
| MQTT-6.0.0-3 | 6 Using WebSocket as a network transport | client | RALMQTTWebSocket | transport, paho | RALMQTTWebSocket |
| MQTT-6.0.0-4 | 6 Using WebSocket as a network transport | server | RALMQTTWebSocket | transport, paho | RALMQTTWebSocket |

## MQTT 3.1.1 (OASIS Standard with Errata 01, 10 December 2015)

140 statements; cited in code: 55.

| statement | section | binds | unit | proven by | cited in |
|---|---|---|---|---|---|
| MQTT-1.5.3-1 | 1.5.3 UTF-8 encoded strings | both | RALMQTTTypes | codec | - |
| MQTT-1.5.3-2 | 1.5.3 UTF-8 encoded strings | both | RALMQTTTypes | codec | - |
| MQTT-1.5.3-3 | 1.5.3 UTF-8 encoded strings | both | RALMQTTTypes | codec | - |
| MQTT-2.2.2-1 | 2.2.2 Flags | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.2.2-2 | 2.2.2 Flags | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.3.1-1 | 2.3.1 Packet Identifier | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.3.1-2 | 2.3.1 Packet Identifier | client | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.3.1-3 | 2.3.1 Packet Identifier | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.3.1-4 | 2.3.1 Packet Identifier | server | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.3.1-5 | 2.3.1 Packet Identifier | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.3.1-6 | 2.3.1 Packet Identifier | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-2.3.1-7 | 2.3.1 Packet Identifier | both | RALMQTTPacket, RALMQTTTypes | codec | - |
| MQTT-3.1.0-1 | 3.1 CONNECT - Client requests a connection to a Server | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker, RALMQTTClient |
| MQTT-3.1.0-2 | 3.1 CONNECT - Client requests a connection to a Server | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-1 | 3.1.2 Variable header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-2 | 3.1.2 Variable header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient, RALMQTTPacket |
| MQTT-3.1.2-3 | 3.1.2 Variable header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.1.2-4 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-5 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-6 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-8 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-9 | 3.1.2 Variable header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-10 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-11 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.1.2-12 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-13 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.1.2-14 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-15 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-16 | 3.1.2 Variable header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-17 | 3.1.2 Variable header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-18 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-19 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.2-20 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient |
| MQTT-3.1.2-21 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient |
| MQTT-3.1.2-22 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker, RALMQTTPacket |
| MQTT-3.1.2-23 | 3.1.2 Variable header | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.2-24 | 3.1.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.3-1 | 3.1.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.1.3-2 | 3.1.3 Payload | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-3 | 3.1.3 Payload | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-4 | 3.1.3 Payload | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-5 | 3.1.3 Payload | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-6 | 3.1.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.3-7 | 3.1.3 Payload | client | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-8 | 3.1.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.3-9 | 3.1.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.3-10 | 3.1.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.3-11 | 3.1.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.4-1 | 3.1.4 Response | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.4-2 | 3.1.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.4-3 | 3.1.4 Response | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.1.4-4 | 3.1.4 Response | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.1.4-5 | 3.1.4 Response | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.0-1 | 3.2 CONNACK - Acknowledge connection request | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker, RALMQTTClient |
| MQTT-3.2.2-1 | 3.2.2 Variable header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.2.2-2 | 3.2.2 Variable header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.2.2-3 | 3.2.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-4 | 3.2.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.2.2-5 | 3.2.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTClient |
| MQTT-3.2.2-6 | 3.2.2 Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.3.1.-1 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-1 | 3.3.1 Fixed header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-2 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.3.1-3 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-4 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.3.1-5 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.1-6 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-7 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-8 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-9 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.1-10 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-11 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.1-12 | 3.3.1 Fixed header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.3.2-1 | 3.3.2 -Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.2-2 | 3.3.2 -Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.3.2-3 | 3.3.2 -Variable header | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.4-1 | 3.3.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.5-1 | 3.3.5 Actions | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.3.5-2 | 3.3.5 Actions | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.6.1-1 | 3.6.1 Fixed header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.8.1-1 | 3.8.1 Fixed header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTPacket |
| MQTT-3.8.3-1 | 3.8.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.8.3-2 | 3.8.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTPacket |
| MQTT-3.8.3-3 | 3.8.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTBroker |
| MQTT-3.8.4-1 | 3.8.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.8.4-2 | 3.8.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTBroker |
| MQTT-3.8.4-3 | 3.8.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | RALMQTTBroker |
| MQTT-3.8.4-4 | 3.8.4 Response | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.8.4-5 | 3.8.4 Response | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.8.4-6 | 3.8.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker, RALMQTTTopic | codec, client, paho | - |
| MQTT-3.9.3-1 | 3.9.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.9.3-2 | 3.9.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.1-1 | 3.10.1 Fixed header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.10.3-1 | 3.10.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.3-2 | 3.10.3 Payload | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTPacket |
| MQTT-3.10.4-1 | 3.10.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.4-2 | 3.10.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.4-3 | 3.10.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.4-4 | 3.10.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.4-5 | 3.10.4 Response | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.10.4-6 | 3.10.4 Response | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.12.4-1 | 3.12.4 Response | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker |
| MQTT-3.14.1-1 | 3.14.1 Fixed header | server | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.4-1 | 3.14.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.4-2 | 3.14.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | - |
| MQTT-3.14.4-3 | 3.14.4 Response | both | RALMQTTPacket, RALMQTTClient, RALMQTTBroker | codec, client, paho | RALMQTTBroker, RALMQTTSNGateway |
| MQTT-4.1.0-1 | 4.1 Storing state | both | RALMQTTClient, RALMQTTBroker, RALMQTTStore, RALMQTTBrokerStore | client, persist, reliability | - |
| MQTT-4.1.0-2 | 4.1 Storing state | both | RALMQTTClient, RALMQTTBroker, RALMQTTStore, RALMQTTBrokerStore | client, persist, reliability | RALMQTTBroker |
| MQTT-4.3.1-1 | 4.3.1 QoS 0: At most once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.2-1 | 4.3.2 QoS 1: At least once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.2-2 | 4.3.2 QoS 1: At least once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-1 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.3.3-2 | 4.3.3 QoS 2: Exactly once delivery | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.4.0-1 | 4.4 Message delivery retry | both | RALMQTTClient, RALMQTTBroker | reliability | RALMQTTBroker, RALMQTTClient |
| MQTT-4.5.0-1 | 4.5 Message receipt | both | RALMQTTClient, RALMQTTBroker | client, paho | - |
| MQTT-4.5.0-2 | 4.5 Message receipt | client | RALMQTTClient, RALMQTTBroker | client, paho | RALMQTTClient |
| MQTT-4.6.0-1 | 4.6 Message ordering | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | RALMQTTClient |
| MQTT-4.6.0-2 | 4.6 Message ordering | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.6.0-3 | 4.6 Message ordering | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.6.0-4 | 4.6 Message ordering | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.6.0-5 | 4.6 Message ordering | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.6.0-6 | 4.6 Message ordering | both | RALMQTTClient, RALMQTTBroker | client, paho, reliability | - |
| MQTT-4.7.1-1 | 4.7.1 Topic wildcards | both | RALMQTTTopic | codec, client | RALMQTTTopic |
| MQTT-4.7.1-2 | 4.7.1 Topic wildcards | both | RALMQTTTopic | codec, client | RALMQTTTopic |
| MQTT-4.7.1-3 | 4.7.1 Topic wildcards | both | RALMQTTTopic | codec, client | - |
| MQTT-4.7.2-1 | 4.7.2 -Topics beginning with $ | server | RALMQTTTopic | codec, client | RALMQTTTopic |
| MQTT-4.7.3-1 | 4.7.3 Topic semantic and usage | both | RALMQTTTopic | codec, client | RALMQTTPacket, RALMQTTTopic |
| MQTT-4.7.3-2 | 4.7.3 Topic semantic and usage | both | RALMQTTTopic | codec, client | - |
| MQTT-4.7.3-3 | 4.7.3 Topic semantic and usage | both | RALMQTTTopic | codec, client | - |
| MQTT-4.7.3-4 | 4.7.3 Topic semantic and usage | server | RALMQTTTopic | codec, client | RALMQTTTopic |
| MQTT-4.8.0-1 | 4.8 Handling errors | both | RALMQTTBroker | client (RAL broker), paho | - |
| MQTT-4.8.0-2 | 4.8 Handling errors | both | RALMQTTBroker | client (RAL broker), paho | - |
| MQTT-6.0.0-1 | 6 Using WebSocket as a network transport | both | RALMQTTWebSocket | transport, paho | RALMQTTWebSocket |
| MQTT-6.0.0-2 | 6 Using WebSocket as a network transport | both | RALMQTTWebSocket | transport, paho | RALMQTTWebSocket |
| MQTT-6.0.0-3 | 6 Using WebSocket as a network transport | both | RALMQTTWebSocket | transport, paho | RALMQTTWebSocket |
| MQTT-6.0.0-4 | 6 Using WebSocket as a network transport | both | RALMQTTWebSocket | transport, paho | RALMQTTWebSocket |
| MQTT-7.0.0-1 | 7 Conformance | server | RALMQTTPacket | codec | - |
| MQTT-7.0.0-2 | 7 Conformance | both | RALMQTTPacket | codec | - |
| MQTT-7.1.1-1 | 7.1.1 MQTT Server | server | RALMQTTPacket | codec | - |
| MQTT-7.1.2-1 | 7.1.2 MQTT Client | client | RALMQTTPacket | codec | - |

## MQTT-SN 2.0 (OASIS working draft, read 26/10/2026)

The draft does not number its statements: each row is a MUST/SHALL sentence, named by its section and its order there. The PROTECTION envelope (section 3.x) is not implemented. MQTT-SN 1.2 has no numbered statements either; its behaviour is proven by the same `sn` cases in 1.2 and by `sn12-python`, an encoder written from the 1.2 text that shares no code with RAL.

138 statements.

| statement | section | unit | proven by |
|---|---|---|---|
| SN20-1.1.1-1 | 1.1.1 MQTT-SN and MQTT Differences | RALMQTTSNTypes | sn |
| SN20-1.4-1 | 1.4 Terminology | RALMQTTSNTypes | sn |
| SN20-1.4-2 | 1.4 Terminology | RALMQTTSNTypes | sn |
| SN20-1.4-3 | 1.4 Terminology | RALMQTTSNTypes | sn |
| SN20-1.4-4 | 1.4 Terminology | RALMQTTSNTypes | sn |
| SN20-1.4.4-1 | 1.4.4 UTF-8 Encoded String | RALMQTTSNTypes | sn |
| SN20-1.4.4-2 | 1.4.4 UTF-8 Encoded String | RALMQTTSNTypes | sn |
| SN20-1.4.4-3 | 1.4.4 UTF-8 Encoded String | RALMQTTSNTypes | sn |
| SN20-1.4.4-4 | 1.4.4 UTF-8 Encoded String | RALMQTTSNTypes | sn |
| SN20-2.1.2-1 | 2.1.2 Length | RALMQTTSNTypes | sn |
| SN20-2.2-1 | 2.2 Packet Identifier | RALMQTTSNTypes | sn |
| SN20-2.2-2 | 2.2 Packet Identifier | RALMQTTSNTypes | sn |
| SN20-2.2-3 | 2.2 Packet Identifier | RALMQTTSNTypes | sn |
| SN20-2.2-4 | 2.2 Packet Identifier | RALMQTTSNTypes | sn |
| SN20-2.2-5 | 2.2 Packet Identifier | RALMQTTSNTypes | sn |
| SN20-2.2-6 | 2.2 Packet Identifier | RALMQTTSNTypes | sn |
| SN20-3.1.2.2-1 | 3.1.2.2 Radius | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.2.2-2 | 3.1.2.2 Radius | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.2-1 | 3.1.4.2 Connect Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.2-2 | 3.1.4.2 Connect Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.4-1 | 3.1.4.4 Protocol Version | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.5-1 | 3.1.4.5 Keep Alive Timer | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.5-2 | 3.1.4.5 Keep Alive Timer | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.5-3 | 3.1.4.5 Keep Alive Timer | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.5-4 | 3.1.4.5 Keep Alive Timer | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.6-1 | 3.1.4.6 Session Expiry Interval | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.7-1 | 3.1.4.7 Max Packet Size | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.7-2 | 3.1.4.7 Max Packet Size | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.8-1 | 3.1.4.8 Client Identifier | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.8-2 | 3.1.4.8 Client Identifier | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.4.8-3 | 3.1.4.8 Client Identifier | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.2-1 | 3.1.5.2 Connack Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.2-2 | 3.1.5.2 Connack Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.2-3 | 3.1.5.2 Connack Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.2-4 | 3.1.5.2 Connack Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.2-5 | 3.1.5.2 Connack Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.2-6 | 3.1.5.2 Connack Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.2-7 | 3.1.5.2 Connack Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.2-8 | 3.1.5.2 Connack Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.2-9 | 3.1.5.2 Connack Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.4-1 | 3.1.5.4 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.4-2 | 3.1.5.4 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.10-1 | 3.1.5.10 Assigned Client Identifier | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.10-2 | 3.1.5.10 Assigned Client Identifier | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.5.10-3 | 3.1.5.10 Assigned Client Identifier | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.6.2-1 | 3.1.6.2 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.8.5-1 | 3.1.8.5 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.10-1 | 3.1.10 PUBLISH WITHOUT SESSION | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.11-1 | 3.1.11 PUBLISH (used for QoS 0) | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.12-1 | 3.1.12 PUBLISH (used for QoS 1 & 2) | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.13.3-1 | 3.1.13.3 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.14.3-1 | 3.1.14.3 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.15.3-1 | 3.1.15.3 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.16.3-1 | 3.1.16.3 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.17.2-1 | 3.1.17.2 SUBSCRIBE Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.18.5-1 | 3.1.18.5 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.20.3-1 | 3.1.20.3 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.21.3-1 | 3.1.21.3 Client Identifier (optional) | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.23.2-1 | 3.1.23.2 Disconnect Flags | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.23.3-1 | 3.1.23.3 Reason Code | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.25-1 | 3.1.25 Protection Encapsulation | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.25-2 | 3.1.25 Protection Encapsulation | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-3.1.25.9-1 | 3.1.25.9 Protected MQTT-SN Packet | RALMQTTSNPacket20, RALMQTTSNTypes | sn (codec), sn12-python (1.2) |
| SN20-4.1.2-1 | 4.1.2 Aggregating Gateway | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.2-1 | 4.2 Networks & Transport Layers | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.4-1 | 4.4 Session Establishment | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.4-2 | 4.4 Session Establishment | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.1-1 | 4.5.1 QoS 0: At most once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.2-1 | 4.5.2 QoS 1: At least once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.2-2 | 4.5.2 QoS 1: At least once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.2-3 | 4.5.2 QoS 1: At least once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.2-4 | 4.5.2 QoS 1: At least once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.2-5 | 4.5.2 QoS 1: At least once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.2-6 | 4.5.2 QoS 1: At least once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-1 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-2 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-3 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-4 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-5 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-6 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-7 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-8 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-9 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-10 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-11 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-12 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-13 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-14 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.5.3-15 | 4.5.3 QoS 2: Exactly once delivery | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.6-1 | 4.6 Client states | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.6.1-1 | 4.6.1 Gateway timers | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.9.1-1 | 4.9.1 Topic wildcards | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.9.1.2-1 | 4.9.1.2 Multi-level wildcard | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.9.1.2-2 | 4.9.1.2 Multi-level wildcard | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.9.1.3-1 | 4.9.1.3 Single-level wildcard | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.9.2-1 | 4.9.2 Topics beginning with $ | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.9.3-1 | 4.9.3 Topic semantic and usage | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.9.3-2 | 4.9.3 Topic semantic and usage | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.9.3-3 | 4.9.3 Topic semantic and usage | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.9.3-4 | 4.9.3 Topic semantic and usage | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.10-1 | 4.10 Topic Name and Topic Filter Registration Procedure | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.10-2 | 4.10 Topic Name and Topic Filter Registration Procedure | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.11-1 | 4.11 Topic Name and Topic Filter Mapping and Aliasing | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.14-1 | 4.14 Client’s Publish Procedure | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.14-2 | 4.14 Client’s Publish Procedure | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.15-1 | 4.15 Gateway’s Publish Procedure | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.15-2 | 4.15 Gateway’s Publish Procedure | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.16-1 | 4.16 Keep Alive and PING Procedure | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.18-1 | 4.18 Retransmission Procedure in the Client and Gateway | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.18-2 | 4.18 Retransmission Procedure in the Client and Gateway | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.18-3 | 4.18 Retransmission Procedure in the Client and Gateway | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.18-4 | 4.18 Retransmission Procedure in the Client and Gateway | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.18-5 | 4.18 Retransmission Procedure in the Client and Gateway | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.18-6 | 4.18 Retransmission Procedure in the Client and Gateway | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.19-1 | 4.19 Sleeping clients | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.19-2 | 4.19 Sleeping clients | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.19-3 | 4.19 Sleeping clients | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.19-4 | 4.19 Sleeping clients | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.19-5 | 4.19 Sleeping clients | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.20-1 | 4.20 Authentication | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.20-2 | 4.20 Authentication | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.20-3 | 4.20 Authentication | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.20-4 | 4.20 Authentication | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.20-5 | 4.20 Authentication | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.21-1 | 4.21 Retained Packets | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.21-2 | 4.21 Retained Packets | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.21-3 | 4.21 Retained Packets | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-1 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-2 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-3 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-4 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-5 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-6 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-7 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-8 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-9 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-10 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
| SN20-4.22-11 | 4.22 Optional Features | RALMQTTSNClient, RALMQTTSNGateway | sn |
