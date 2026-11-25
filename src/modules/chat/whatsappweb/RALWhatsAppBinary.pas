{ The binary node format of WhatsApp Web (the "binary XML" of its socket): a node is a
  tag, attributes and either child nodes or binary content; strings travel as tokens of
  the dictionary version 3, packed nibbles or hex digits, JIDs or raw bytes. }
unit RALWhatsAppBinary;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs,
  RALTypes, RALCompress, RALCompressZLib, RALWhatsAppCrypto;

const
  /// The server of user JIDs.
  cRALWAUserServer = 's.whatsapp.net';
  /// The server of group JIDs.
  cRALWAGroupServer = 'g.us';
  /// The server of hidden user ids (LID).
  cRALWALIDServer = 'lid';
  /// The server of broadcast lists and status.
  cRALWABroadcastServer = 'broadcast';

type
  /// One attribute of a node.
  TRALWhatsAppAttr = record
    /// The name.
    Name: StringRAL;
    /// The value (a JID is written as text: user, server and maybe device).
    Value: StringRAL;
  end;

  /// What a node carries besides its attributes.
  TRALWhatsAppContentKind = (
    /// Nothing.
    wckNone,
    /// Bytes (Content).
    wckBytes,
    /// Child nodes.
    wckNodes
  );

  { A node of the WhatsApp socket. A node owns its children; adding a child or setting
    Content changes ContentKind. }
  TRALWhatsAppNode = class
  private
    /// The attributes.
    FAttrs: array of TRALWhatsAppAttr;
    /// The child nodes, owned.
    FChildren: TObjectList;
    FContent: TBytes;
    FContentKind: TRALWhatsAppContentKind;
    FTag: StringRAL;

    function GetAttrCount: IntegerRAL;
    function GetAttrs(AIndex: IntegerRAL): TRALWhatsAppAttr;
    function GetChildCount: IntegerRAL;
    function GetChildren(AIndex: IntegerRAL): TRALWhatsAppNode;
    procedure SetContent(const AValue: TBytes);
  public
    /// A node of tag ATag, with nothing in it.
    constructor Create(const ATag: StringRAL);
    destructor Destroy; override;

    /// Adds ANode as the last child (the node now owns it) and returns it.
    function Add(ANode: TRALWhatsAppNode): TRALWhatsAppNode;
    /// Adds a new child of tag ATag and returns it.
    function AddChild(const ATag: StringRAL): TRALWhatsAppNode;
    /// Adds a child of tag ATag whose content is AContent, and returns it.
    function AddContent(const ATag: StringRAL; const AContent: TBytes): TRALWhatsAppNode;
    /// The value of the attribute AName; empty when it is missing.
    function Attr(const AName: StringRAL): StringRAL;
    /// The first child of tag ATag, or nil.
    function Child(const ATag: StringRAL): TRALWhatsAppNode;
    /// The content of the first child of tag ATag; empty when there is none.
    function ChildContent(const ATag: StringRAL): TBytes;
    /// The content as text (the bytes as they are).
    function ContentText: StringRAL;
    /// Whether the attribute AName is there.
    function HasAttr(const AName: StringRAL): Boolean;
    /// Sets the attribute AName (added when missing) and returns the node.
    function SetAttr(const AName, AValue: StringRAL): TRALWhatsAppNode;
    /// An XML-like text of the node, for logs (binary content in hex).
    function ToText: StringRAL;

    /// How many attributes there are.
    property AttrCount: IntegerRAL read GetAttrCount;
    /// The attribute at AIndex.
    property Attrs[AIndex: IntegerRAL]: TRALWhatsAppAttr read GetAttrs;
    /// How many children there are.
    property ChildCount: IntegerRAL read GetChildCount;
    /// The child at AIndex.
    property Children[AIndex: IntegerRAL]: TRALWhatsAppNode read GetChildren;
    /// The binary content; setting it drops the children.
    property Content: TBytes read FContent write SetContent;
    /// What the node carries.
    property ContentKind: TRALWhatsAppContentKind read FContentKind;
    /// The tag.
    property Tag: StringRAL read FTag write FTag;
  end;

/// The bytes of ANode in the binary format (without the frame's flag byte).
function RALWhatsAppEncodeNode(ANode: TRALWhatsAppNode): TBytes;
{ The node in AData (without the frame's flag byte); the caller frees it. Raises
  ERALWhatsAppError when the data is not a valid node. }
function RALWhatsAppDecodeNode(const AData: TBytes): TRALWhatsAppNode;
/// The plaintext of a frame for ANode: the flag byte 0 and the node.
function RALWhatsAppPackNode(ANode: TRALWhatsAppNode): TBytes;
{ The node of a frame plaintext: reads the flag byte (2 = zlib) and decodes the rest;
  the caller frees it. }
function RALWhatsAppUnpackNode(const AData: TBytes): TRALWhatsAppNode;
{ Splits a JID into user, device and server (5511999, 3 and s.whatsapp.net for
  '5511999:3' at s.whatsapp.net); False when AJid has no at sign. }
function RALWhatsAppSplitJid(const AJid: StringRAL; out AUser: StringRAL;
  out ADevice: IntegerRAL; out AServer: StringRAL): Boolean;
/// The JID of AUser on AServer, with ADevice when above 0.
function RALWhatsAppJid(const AUser: StringRAL; ADevice: IntegerRAL;
  const AServer: StringRAL): StringRAL;
/// AJid without the device: the user's JID.
function RALWhatsAppUserJid(const AJid: StringRAL): StringRAL;

implementation

resourcestring
  /// A node that cannot be read.
  rsWABinaryBad = 'A WhatsApp binary node is not valid.';
  /// A node that ends before its data.
  rsWABinaryShort = 'A WhatsApp binary node ends too early.';

const
  /// An empty list (and an empty JID user).
  tagListEmpty = 0;
  /// The first double-byte dictionary.
  tagDictionary0 = 236;
  /// The last double-byte dictionary.
  tagDictionary3 = 239;
  /// A JID of an interop network.
  tagInteropJid = 245;
  /// A JID of Messenger.
  tagFBJid = 246;
  /// A JID with agent and device.
  tagADJid = 247;
  /// A list of up to 255 items.
  tagList8 = 248;
  /// A list of up to 65535 items.
  tagList16 = 249;
  /// A JID as user and server.
  tagJidPair = 250;
  /// Packed hex digits.
  tagHex8 = 251;
  /// Bytes, length in one byte.
  tagBinary8 = 252;
  /// Bytes, length in 20 bits.
  tagBinary20 = 253;
  /// Bytes, length in 32 bits.
  tagBinary32 = 254;
  /// Packed nibbles (digits, '-' and '.').
  tagNibble8 = 255;
  /// The longest text packed in nibbles or hex.
  cPackedMax = 127;
  /// The single-byte tokens of the dictionary version 3 (index = byte).
  cSingleTokens: array[0..235] of StringRAL = (
    '', 'xmlstreamstart', 'xmlstreamend', 's.whatsapp.net', 'type', 'participant',
    'from', 'receipt', 'id', 'notification', 'disappearing_mode', 'status', 'jid',
    'broadcast', 'user', 'devices', 'device_hash', 'to', 'offline', 'message',
    'result', 'class', 'xmlns', 'duration', 'notify', 'iq', 't', 'ack', 'g.us', 'enc',
    'urn:xmpp:whatsapp:push', 'presence', 'config_value', 'picture', 'verified_name',
    'config_code', 'key-index-list', 'contact', 'mediatype', 'routing_info',
    'edge_routing', 'get', 'read', 'urn:xmpp:ping', 'fallback_hostname', '0',
    'chatstate', 'business_hours_config', 'unavailable', 'download_buckets', 'skmsg',
    'verified_level', 'composing', 'handshake', 'device-list', 'media', 'text',
    'fallback_ip4', 'media_conn', 'device', 'creation', 'location', 'config', 'item',
    'fallback_ip6', 'count', 'w:profile:picture', 'image', 'business', '2', 'hostname',
    'call-creator', 'display_name', 'relaylatency', 'platform', 'abprops', 'success',
    'msg', 'offline_preview', 'prop', 'key-index', 'v', 'day_of_week', 'pkmsg',
    'version', '1', 'ping', 'w:p', 'download', 'video', 'set', 'specific_hours',
    'props', 'primary', 'unknown', 'hash', 'commerce_experience', 'last', 'subscribe',
    'max_buckets', 'call', 'profile', 'member_since_text', 'close_time', 'call-id',
    'sticker', 'mode', 'participants', 'value', 'query', 'profile_options',
    'open_time', 'code', 'list', 'host', 'ts', 'contacts', 'upload', 'lid', 'preview',
    'update', 'usync', 'w:stats', 'delivery', 'auth_ttl', 'context', 'fail',
    'cart_enabled', 'appdata', 'category', 'atn', 'direct_connection', 'decrypt-fail',
    'relay_id', 'mmg-fallback.whatsapp.net', 'target', 'available', 'name', 'last_id',
    'mmg.whatsapp.net', 'categories', '401', 'is_new', 'index', 'tctoken', 'ip4',
    'token_id', 'latency', 'recipient', 'edit', 'ip6', 'add', 'thumbnail-document',
    '26', 'paused', 'true', 'identity', 'stream:error', 'key', 'sidelist',
    'background', 'audio', '3', 'thumbnail-image', 'biz-cover-photo', 'cat', 'gcm',
    'thumbnail-video', 'error', 'auth', 'deny', 'serial', 'in', 'registration',
    'thumbnail-link', 'remove', '00', 'gif', 'thumbnail-gif', 'tag', 'capability',
    'multicast', 'item-not-found', 'description', 'business_hours', 'config_expo_key',
    'md-app-state', 'expiration', 'fallback', 'ttl', '300', 'md-msg-hist',
    'device_orientation', 'out', 'w:m', 'open_24h', 'side_list', 'token', 'inactive',
    '01', 'document', 'te2', 'played', 'encrypt', 'msgr', 'hide', 'direct_path', '12',
    'state', 'not-authorized', 'url', 'terminate', 'signature', 'status-revoke-delay',
    '02', 'te', 'linked_accounts', 'trusted_contact', 'timezone', 'ptt', 'kyc-id',
    'privacy_token', 'readreceipts', 'appointment_only', 'address', 'expected_ts',
    'privacy', '7', 'android', 'interactive', 'device-identity', 'enabled',
    'attribute_padding', '1080', '03', 'screen_height');
  /// The double-byte tokens: four dictionaries of 256.
  cDoubleTokens: array[0..3, 0..255] of StringRAL = (
    (
      'read-self', 'active', 'fbns', 'protocol', 'reaction', 'screen_width',
      'heartbeat', 'deviceid', '2:47DEQpj8', 'uploadfieldstat', 'voip_settings',
      'retry', 'priority', 'longitude', 'conflict', 'false', 'ig_professional',
      'replaced', 'preaccept', 'cover_photo', 'uncompressed', 'encopt', 'ppic', '04',
      'passive', 'status-revoke-drop', 'keygen', '540', 'offer', 'rate', 'opus',
      'latitude', 'w:gp2', 'ver', '4', 'business_profile', 'medium', 'sender',
      'prev_v_id', 'email', 'website', 'invited', 'sign_credential', '05', 'transport',
      'skey', 'reason', 'peer_abtest_bucket', 'America/Sao_Paulo', 'appid', 'refresh',
      '100', '06', '404', '101', '104', '107', '102', '109', '103', 'member_add_mode',
      '105', 'transaction-id', '110', '106', 'outgoing', '108', '111', 'tokens',
      'followers', 'ig_handle', 'self_pid', 'tue', 'dec', 'thu', 'joinable',
      'peer_pid', 'mon', 'features', 'wed', 'peer_device_presence', 'pn', 'delete',
      '07', 'fri', 'audio_duration', 'admin', 'connected', 'delta', 'rcat', 'disable',
      'collection', '08', '480', 'sat', 'phash', 'all', 'invite', 'accept',
      'critical_unblock_low', 'group_update', 'signed_credential',
      'blinded_credential', 'eph_setting', 'net', '09', 'background_location',
      'refresh_id', 'Asia/Kolkata', 'privacy_mode_ts', 'account_sync',
      'voip_payload_type', 'service_areas', 'acs_public_key', 'v_id', '0a',
      'fallback_class', 'relay', 'actual_actors', 'metadata', 'w:biz', '5',
      'connected-limit', 'notice', '0b', 'host_storage', 'fb_page', 'subject',
      'privatestats', 'invis', 'groupadd', '010', 'note.m4r', 'uuid', '0c', '8000',
      'sun', '372', '1020', 'stage', '1200', '720', 'canonical', 'fb', '011',
      'video_duration', '0d', '1140', 'superadmin', '012', 'Opening.m4r',
      'keystore_attestation', 'dleq_proof', '013', 'timestamp', 'ab_key',
      'w:sync:app:state', '0e', 'vertical', '600', 'p_v_id', '6', 'likes', '014',
      '500', '1260', 'creator', '0f', 'rte', 'destination', 'group', 'group_info',
      'syncd_anti_tampering_fatal_exception_enabled', '015', 'dl_bw', 'Asia/Jakarta',
      'vp8/h.264', 'online', '1320', 'fb:multiway', '10', 'timeout', '016',
      'nse_retry', 'urn:xmpp:whatsapp:dirty', '017', 'a_v_id',
      'web_shops_chat_header_button_enabled', 'nse_call', 'inactive-upgrade', 'none',
      'web', 'groups', '2250', 'mms_hot_content_timespan_in_seconds',
      'contact_blacklist', 'nse_read', 'suspended_group_deletion_notification',
      'binary_version', '018', 'https://www.whatsapp.com/otp/copy/', 'reg_push',
      'shops_hide_catalog_attachment_entrypoint', 'server_sync', '.',
      'ephemeral_messages_allowed_values', '019', 'mms_vcache_aggregation_enabled',
      'iphone', 'America/Argentina/Buenos_Aires', '01a',
      'mms_vcard_autodownload_size_kb', 'nse_ver', 'shops_header_dropdown_menu_item',
      'dhash', 'catalog_status', 'communities_mvp_new_iqs_serverprop', 'blocklist',
      'default', '11', 'ephemeral_messages_enabled', '01b', 'original_dimensions', '8',
      'mms4_media_retry_notification_encryption_enabled',
      'mms4_server_error_receipt_encryption_enabled', 'original_image_url', 'sync',
      'multiway', '420', 'companion_enc_static', 'shops_profile_drawer_entrypoint',
      '01c', 'vcard_as_document_size_kb', 'status_video_max_duration',
      'request_image_url', '01d', 'regular_high', 's_t', 'abt',
      'share_ext_min_preliminary_image_quality', '01e', '32',
      'syncd_key_rotation_enabled', 'data_namespace', 'md_downgrade_read_receipts2',
      'patch', 'polltype', 'ephemeral_messages_setting', 'userrate', '15',
      'partial_pjpeg_bw_threshold', 'played-self', 'catalog_exists', '01f', 'mute_v2'),
    (
      'reject', 'dirty', 'announcement', '020', '13', '9', 'status_video_max_bitrate',
      'fb:thrift_iq', 'offline_batch', '022', 'full',
      'ctwa_first_business_reply_logging', 'h.264', 'smax_id',
      'group_description_length', 'https://www.whatsapp.com/otp/code',
      'status_image_max_edge', 'smb_upsell_business_profile_enabled', '021',
      'web_upgrade_to_md_modal', '14', '023', 's_o',
      'smaller_video_thumbs_status_enabled', 'media_max_autodownload', '960',
      'blocking_status', 'peer_msg', 'joinable_group_call_client_version',
      'group_call_video_maximization_enabled', 'return_snapshot', 'high',
      'America/Mexico_City', 'entry_point_block_logging_enabled', 'pop', '024', '1050',
      '16', '1380', 'one_tap_calling_in_group_chat_size', 'regular_low',
      'inline_joinable_education_enabled', 'hq_image_max_edge', 'locked',
      'America/Bogota', 'smb_biztools_deeplink_enabled', 'status_image_quality',
      '1088', '025', 'payments_upi_intent_transaction_limit', 'voip', 'w:g2', '027',
      'md_pin_chat_enabled', '026', 'multi_scan_pjpeg_download_enabled',
      'shops_product_grid', 'transaction_id', 'ctwa_context_enabled', '20', 'fna',
      'hq_image_quality', 'alt_jpeg_doc_detection_quality',
      'group_call_max_participants', 'pkey', 'America/Belem', 'image_max_kbytes',
      'web_cart_v1_1_order_message_changes_enabled', 'ctwa_context_enterprise_enabled',
      'urn:xmpp:whatsapp:account', '840', 'Asia/Kuala_Lumpur', 'max_participants',
      'video_remux_after_repair_enabled', 'stella_addressbook_restriction_type', '660',
      '900', '780', 'context_menu_ios13_enabled', 'mute-state', 'ref',
      'payments_request_messages', '029', 'frskmsg', 'vcard_max_size_kb',
      'sample_buffer_gif_player_enabled', 'match_last_seen', '510', '4983',
      'video_max_bitrate', '028', 'w:comms:chat', '17', 'frequently_forwarded_max',
      'groups_privacy_blacklist', 'Asia/Karachi', '02a',
      'web_download_document_thumb_mms_enabled', '02b', 'hist_sync',
      'biz_block_reasons_version', '1024', '18',
      'web_is_direct_connection_for_plm_transparent', 'view_once_write',
      'file_max_size', 'paid_convo_id', 'online_privacy_setting', 'video_max_edge',
      'view_once_read', 'enhanced_storage_management',
      'multi_scan_pjpeg_encoding_enabled', 'ctwa_context_forward_enabled',
      'video_transcode_downgrade_enable', 'template_doc_mime_types',
      'hq_image_bw_threshold', '30', 'body', 'u_aud_limit_sil_restarts_ctrl', 'other',
      'participating', 'w:biz:directory', '1110', 'vp8', '4018', 'meta',
      'doc_detection_image_max_edge', 'image_quality', '1170', '02c',
      'smb_upsell_chat_banner_enabled', 'key_expiry_time_second', 'pid',
      'stella_interop_enabled', '19', 'linked_device_max_count',
      'md_device_sync_enabled', '02d', '02e', '360', 'enhanced_block_enabled',
      'ephemeral_icon_in_forwarding', 'paid_convo_status', 'gif_provider',
      'project_name', 'server-error', 'canonical_url_validation_enabled',
      'wallpapers_v2', 'syncd_clear_chat_delete_chat_enabled', 'medianotify', '02f',
      'shops_required_tos_version', 'vote', 'reset_skey_on_id_change', '030',
      'image_max_edge', 'multicast_limit_global', 'ul_bw', '21', '25', '5000', 'poll',
      '570', '22', '031', '1280', 'WhatsApp', '032', 'bloks_shops_enabled', '50',
      'upload_host_switching_enabled', 'web_ctwa_context_compose_enabled',
      'ptt_forwarded_features_enabled', 'unblocked', 'partial_pjpeg_enabled',
      'fbid:devices', 'height', 'ephemeral_group_query_ts', 'group_join_permissions',
      'order', '033', 'alt_jpeg_status_quality', 'migrate', 'popular-bank',
      'win_uwp_deprecation_killswitch_enabled',
      'web_download_status_thumb_mms_enabled', 'blocking', 'url_text', '035',
      'web_forwarding_limit_to_groups', '1600', 'val', '1000',
      'syncd_msg_date_enabled', 'bank-ref-id', 'max_subject', 'payments_web_enabled',
      'web_upload_document_thumb_mms_enabled', 'size', 'request', 'ephemeral', '24',
      'receipt_agg', 'ptt_remember_play_position', 'sampling_weight', 'enc_rekey',
      'mute_always', '037', '034', '23', '036', 'action', 'click_to_chat_qr_enabled',
      'width', 'disabled', '038', 'md_blocklist_v2', 'played_self_enabled',
      'web_buttons_message_enabled', 'flow_id', 'clear', '450', 'fbid:thread',
      'bloks_session_state', 'America/Lima', 'attachment_picker_refresh',
      'download_host_switching_enabled', '1792', 'u_aud_limit_sil_restarts_test2',
      'custom_urls', 'device_fanout', 'optimistic_upload', '2000', 'key_cipher_suite',
      'web_smb_upsell_in_biz_profile_enabled', 'e', '039', 'siri_post_status_shortcut',
      'pair-device', 'lg', 'lc', 'stream_attribution_url', 'model',
      'mspjpeg_phash_gen', 'catalog_send_all', 'new_multi_vcards_ui',
      'share_biz_vcard_enabled', '-', 'clean', '200', 'md_blocklist_v2_server', '03b',
      '03a', 'web_md_migration_experience', 'ptt_conversation_waveform',
      'u_aud_limit_sil_restarts_test1'),
    (
      '64', 'ptt_playback_speed_enabled', 'web_product_list_message_enabled',
      'paid_convo_ts', '27', 'manufacturer', 'psp-routing', 'grp_uii_cleanup',
      'ptt_draft_enabled', '03c', 'business_initiated', 'web_catalog_products_onoff',
      'web_upload_link_thumb_mms_enabled', '03e', 'mediaretry', '35',
      'hfm_string_changes', '28', 'America/Fortaleza', 'max_keys', 'md_mhfs_days',
      'streaming_upload_chunk_size', '5541', '040', '03d', '2675', '03f', '...', '512',
      'mute', '48', '041', 'alt_jpeg_quality', '60', '042', 'md_smb_quick_reply',
      '5183', 'c', '1343', '40', '1230', '043', '044',
      'mms_cat_v1_forward_hot_override_enabled', 'user_notice', 'ptt_waveform_send',
      '047', 'Asia/Calcutta', '250', 'md_privacy_v2', '31', '29', '128',
      'md_messaging_enabled', '046', 'crypto', '690', '045', 'enc_iv', '75', 'failure',
      'ptt_oot_playback', 'AIzaSyDR5yfaG7OG8sMTUj8kfQEb8T9pN8BM6Lk', 'w', '048',
      '2201', 'web_large_files_ui', 'Asia/Makassar', '812', 'status_collapse_muted',
      '1334', '257', '2HP4dm', '049', 'patches', '1290', '43cY6T', 'America/Caracas',
      'web_sticker_maker', 'campaign', 'ptt_pausable_enabled', '33', '42',
      'attestation', 'biz', '04b', 'query_linked', 's', '125', '04a', '810',
      'availability', '1411', 'responsiveness_v2_m1', 'catalog_not_created', '34',
      'America/Santiago', '1465', 'enc_p', '04d', 'status_info', '04f', 'key_version',
      '..', '04c', '04e', 'md_group_notification', '1598', '1215', 'web_cart_enabled',
      '37', '630', '1920', '2394', '-1', 'vcard', '38', 'elapsed', '36', '828', 'peer',
      'pricing_category', '1245', 'invalid', 'stella_ios_enabled', '2687', '45',
      '1528', '39', 'u_is_redial_audio_1104_ctrl', '1025', '1455', '58', '2524',
      '2603', '054', 'bsp_system_message_enabled', 'web_pip_redesign', '051',
      'verify_apps', '1974', '1272', '1322', '1755', '052', '70', '050', '1063',
      '1135', '1361', '80', '1096', '1828', '1851', '1251', '1921', 'key_config_id',
      '1254', '1566', '1252', '2525', 'critical_block', '1669', 'max_available',
      'w:auth:backup:token', 'product', '2530', '870', '1022', 'participant_uuid',
      'web_cart_on_off', '1255', '1432', '1867', '41', '1415', '1440', '240', '1204',
      '1608', '1690', '1846', '1483', '1687', '1749', '69', 'url_number', '053',
      '1325', '1040', '365', '59', 'Asia/Riyadh', '1177', 'test_recommended', '057',
      '1612', '43', '1061', '1518', '1635', '055', '1034', '1375', '750', '1430',
      'event_code', '1682', '503', '55', '865', '78', '1309', '1365', '44',
      'America/Guayaquil', '535', 'LIMITED', '1377', '1613', '1420', '1599', '1822',
      '05a', '1681', 'password', '1111', '1214', '1376', '1478', '47', '1082', '4282',
      'Europe/Istanbul', '1307', '46', '058', '1124', '256', 'rate-overlimit',
      'retail', 'u_a_socket_err_fix_succ_test', '1292', '1370', '1388', '520', '861',
      'psa', 'regular', '1181', '1766', '05b', '1183', '1213', '1304', '1537'),
    (
      '1724', 'profile_picture', '1071', '1314', '1605', '407', '990', '1710', '746',
      'pricing_model', '056', '059', '061', '1119', '6027', '65', '877', '1607', '05d',
      '917', 'seen', '1516', '49', '470', '973', '1037', '1350', '1394', '1480',
      '1796', 'keys', '794', '1536', '1594', '2378', '1333', '1524', '1825', '116',
      '309', '52', '808', '827', '909', '495', '1660', '361', '957', 'google', '1357',
      '1565', '1967', '996', '1775', '586', '736', '1052', '1670', 'bank', '177',
      '1416', '2194', '2222', '1454', '1839', '1275', '53', '997', '1629', '6028',
      'smba', '1378', '1410', '05c', '1849', '727', 'create', '1559', '536', '1106',
      '1310', '1944', '670', '1297', '1316', '1762', 'en', '1148', '1295', '1551',
      '1853', '1890', '1208', '1784', '7200', '05f', '178', '1283', '1332', '381',
      '643', '1056', '1238', '2024', '2387', '179', '981', '1547', '1705', '05e',
      '290', '903', '1069', '1285', '2436', '062', '251', '560', '582', '719', '56',
      '1700', '2321', '325', '448', '613', '777', '791', '51', '488', '902',
      'Asia/Almaty', 'is_hidden', '1398', '1527', '1893', '1999', '2367', '2642',
      '237', 'busy', '065', '067', '233', '590', '993', '1511', '54', '723', '860',
      '363', '487', '522', '605', '995', '1321', '1691', '1865', '2447', '2462',
      'NON_TRANSACTIONAL', '433', '871', '432', '1004', '1207', '2032', '2050', '2379',
      '2446', '279', '636', '703', '904', '248', '370', '691', '700', '1068', '1655',
      '2334', '060', '063', '364', '533', '534', '567', '1191', '1210', '1473', '1827',
      '069', '701', '2531', '514', 'prev_dhash', '064', '496', '790', '1046', '1139',
      '1505', '1521', '1108', '207', '544', '637', 'final', '1173', '1293', '1694',
      '1939', '1951', '1993', '2353', '2515', '504', '601', '857', 'modify',
      'spam_request', 'p_121_aa_1101_test4', '866', '1427', '1502', '1638', '1744',
      '2153', '068', '382', '725', '1704', '1864', '1990', '2003', 'Asia/Dubai', '508',
      '531', '1387', '1474', '1632', '2307', '2386', '819', '2014', '066', '387',
      '1468', '1706', '2186', '2261', '471', '728', '1147', '1372', '1961'));

type
  /// Writes the binary format.
  TRALWAWriter = class
  private
    /// The bytes written.
    FData: TBytes;
    /// How many bytes of FData are used.
    FLen: IntegerRAL;
  public
    /// The bytes written.
    function Bytes: TBytes;
    /// Writes one byte.
    procedure PutByte(AValue: Byte);
    /// Writes ACount bytes.
    procedure PutBytes(AData: PByte; ACount: IntegerRAL);
    /// Writes a JID.
    procedure WriteJid(const AUser: StringRAL; ADevice: IntegerRAL; const AServer: StringRAL);
    /// Writes the start of a list of ASize items.
    procedure WriteListStart(ASize: IntegerRAL);
    /// Writes a node.
    procedure WriteNode(ANode: TRALWhatsAppNode);
    /// Writes a text packed as nibbles or hex.
    procedure WritePacked(const AText: StringRAL; ATag: Byte);
    /// Writes bytes with their length.
    procedure WriteRaw(AData: PByte; ACount: IntegerRAL);
    /// Writes a text the shortest way the format has.
    procedure WriteString(const AText: StringRAL);
  end;

  /// Reads the binary format.
  TRALWAReader = class
  private
    /// The bytes being read.
    FData: TBytes;
    /// The next byte to read.
    FPos: IntegerRAL;
  public
    /// A reader of AData.
    constructor Create(const AData: TBytes);

    /// Reads one byte.
    function GetByte: Byte;
    /// Reads ACount bytes.
    function GetBytes(ACount: IntegerRAL): TBytes;
    /// Reads AWidth bytes as a big-endian number.
    function GetInt(AWidth: IntegerRAL): Int64RAL;
    /// Reads a value of tag ATag as bytes (a text gives its bytes).
    function ReadBytesOf(ATag: Byte): TBytes;
    /// Reads a JID of tag ATag.
    function ReadJid(ATag: Byte): StringRAL;
    /// The size of a list of tag ATag.
    function ReadListSize(ATag: Byte): IntegerRAL;
    /// Reads a node.
    function ReadNode: TRALWhatsAppNode;
    /// Reads a packed text of tag ATag.
    function ReadPacked(ATag: Byte): StringRAL;
    /// Reads a value as text.
    function ReadString: StringRAL;
    /// Reads a value of tag ATag as text.
    function ReadStringOf(ATag: Byte): StringRAL;
  end;

var
  { The token of every text, sorted: the object is the code (a byte, or
    256 + dictionary * 256 + index). }
  gTokenIndex: TStringList;

/// Fills gTokenIndex.
procedure BuildTokenIndex;
var
  vInt, vDict: IntegerRAL;
begin
  gTokenIndex := TStringList.Create;
  for vInt := 1 to High(cSingleTokens) do
    gTokenIndex.AddObject(string(cSingleTokens[vInt]), TObject(NativeInt(vInt)));
  for vDict := 0 to 3 do
    for vInt := 0 to 255 do
      gTokenIndex.AddObject(string(cDoubleTokens[vDict, vInt]),
        TObject(NativeInt(256 + vDict * 256 + vInt)));
  gTokenIndex.CaseSensitive := True;
  gTokenIndex.Duplicates := dupIgnore;
  gTokenIndex.Sorted := True;
end;

/// The code of a token (see gTokenIndex), -1 when AText is none.
function TokenOf(const AText: StringRAL): IntegerRAL;
var
  vIdx: Integer;
begin
  if gTokenIndex.Find(string(AText), vIdx) then
    Result := IntegerRAL(NativeInt(gTokenIndex.Objects[vIdx]))
  else
    Result := -1;
end;

/// Whether AText packs as nibbles.
function IsNibble(const AText: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  Result := (AText <> '') and (Length(AText) <= cPackedMax);
  if Result then
    for vInt := 1 to Length(AText) do
      if not (AText[vInt] in ['0'..'9', '-', '.']) then
      begin
        Result := False;
        Exit;
      end;
end;

/// Whether AText packs as hex digits.
function IsHex(const AText: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  Result := (AText <> '') and (Length(AText) <= cPackedMax);
  if Result then
    for vInt := 1 to Length(AText) do
      if not (AText[vInt] in ['0'..'9', 'A'..'F']) then
      begin
        Result := False;
        Exit;
      end;
end;

/// Whether AServer is a server the format writes as a JID.
function IsJidServer(const AServer: StringRAL): Boolean;
begin
  Result := (AServer = cRALWAUserServer) or (AServer = cRALWAGroupServer) or
    (AServer = cRALWALIDServer) or (AServer = cRALWABroadcastServer) or
    (AServer = 'c.us') or (AServer = 'newsletter') or (AServer = 'call') or
    (AServer = 'hosted') or (AServer = 'hosted.lid');
end;

function RALWhatsAppSplitJid(const AJid: StringRAL; out AUser: StringRAL;
  out ADevice: IntegerRAL; out AServer: StringRAL): Boolean;
var
  vAt, vColon: IntegerRAL;
begin
  AUser := '';
  AServer := '';
  ADevice := 0;
  vAt := Pos(StringRAL('@'), AJid);
  Result := vAt > 0;
  if not Result then
    Exit;
  AServer := Copy(AJid, vAt + 1, MaxInt);
  AUser := Copy(AJid, 1, vAt - 1);
  vColon := Pos(StringRAL(':'), AUser);
  if vColon > 0 then
  begin
    ADevice := StrToIntDef(string(Copy(AUser, vColon + 1, MaxInt)), 0);
    AUser := Copy(AUser, 1, vColon - 1);
  end;
end;

function RALWhatsAppJid(const AUser: StringRAL; ADevice: IntegerRAL;
  const AServer: StringRAL): StringRAL;
begin
  if ADevice > 0 then
    Result := AUser + ':' + StringRAL(IntToStr(ADevice)) + '@' + AServer
  else if AUser = '' then
    Result := AServer
  else
    Result := AUser + '@' + AServer;
end;

function RALWhatsAppUserJid(const AJid: StringRAL): StringRAL;
var
  vUser, vServer: StringRAL;
  vDevice: IntegerRAL;
begin
  if RALWhatsAppSplitJid(AJid, vUser, vDevice, vServer) then
    Result := RALWhatsAppJid(vUser, 0, vServer)
  else
    Result := AJid;
end;

{ TRALWhatsAppNode }

constructor TRALWhatsAppNode.Create(const ATag: StringRAL);
begin
  inherited Create;
  FTag := ATag;
  FChildren := TObjectList.Create(True);
end;

destructor TRALWhatsAppNode.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

function TRALWhatsAppNode.Add(ANode: TRALWhatsAppNode): TRALWhatsAppNode;
begin
  FChildren.Add(ANode);
  FContent := nil;
  FContentKind := wckNodes;
  Result := ANode;
end;

function TRALWhatsAppNode.AddChild(const ATag: StringRAL): TRALWhatsAppNode;
begin
  Result := Add(TRALWhatsAppNode.Create(ATag));
end;

function TRALWhatsAppNode.AddContent(const ATag: StringRAL;
  const AContent: TBytes): TRALWhatsAppNode;
begin
  Result := AddChild(ATag);
  Result.Content := AContent;
end;

function TRALWhatsAppNode.Attr(const AName: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 0 to Length(FAttrs) - 1 do
    if FAttrs[vInt].Name = AName then
    begin
      Result := FAttrs[vInt].Value;
      Exit;
    end;
end;

function TRALWhatsAppNode.Child(const ATag: StringRAL): TRALWhatsAppNode;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to FChildren.Count - 1 do
    if TRALWhatsAppNode(FChildren[vInt]).Tag = ATag then
    begin
      Result := TRALWhatsAppNode(FChildren[vInt]);
      Exit;
    end;
end;

function TRALWhatsAppNode.ChildContent(const ATag: StringRAL): TBytes;
var
  vNode: TRALWhatsAppNode;
begin
  vNode := Child(ATag);
  if vNode <> nil then
    Result := vNode.Content
  else
    SetLength(Result, 0);
end;

function TRALWhatsAppNode.ContentText: StringRAL;
begin
  SetLength(Result, Length(FContent));
  if Length(FContent) > 0 then
    Move(FContent[0], Result[1], Length(FContent));
end;

function TRALWhatsAppNode.GetAttrCount: IntegerRAL;
begin
  Result := Length(FAttrs);
end;

function TRALWhatsAppNode.GetAttrs(AIndex: IntegerRAL): TRALWhatsAppAttr;
begin
  Result := FAttrs[AIndex];
end;

function TRALWhatsAppNode.GetChildCount: IntegerRAL;
begin
  Result := FChildren.Count;
end;

function TRALWhatsAppNode.GetChildren(AIndex: IntegerRAL): TRALWhatsAppNode;
begin
  Result := TRALWhatsAppNode(FChildren[AIndex]);
end;

function TRALWhatsAppNode.HasAttr(const AName: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to Length(FAttrs) - 1 do
    if FAttrs[vInt].Name = AName then
    begin
      Result := True;
      Exit;
    end;
end;

function TRALWhatsAppNode.SetAttr(const AName, AValue: StringRAL): TRALWhatsAppNode;
var
  vInt: IntegerRAL;
begin
  Result := Self;
  for vInt := 0 to Length(FAttrs) - 1 do
    if FAttrs[vInt].Name = AName then
    begin
      FAttrs[vInt].Value := AValue;
      Exit;
    end;
  vInt := Length(FAttrs);
  SetLength(FAttrs, vInt + 1);
  FAttrs[vInt].Name := AName;
  FAttrs[vInt].Value := AValue;
end;

procedure TRALWhatsAppNode.SetContent(const AValue: TBytes);
begin
  FChildren.Clear;
  FContent := AValue;
  FContentKind := wckBytes;
end;

function TRALWhatsAppNode.ToText: StringRAL;
var
  vInt: IntegerRAL;
  vPrintable: Boolean;
begin
  Result := '<' + FTag;
  for vInt := 0 to Length(FAttrs) - 1 do
    Result := Result + ' ' + FAttrs[vInt].Name + '="' + FAttrs[vInt].Value + '"';
  case FContentKind of
    wckNone:
      Result := Result + '/>';
    wckBytes:
    begin
      vPrintable := True;
      for vInt := 0 to Length(FContent) - 1 do
        if (FContent[vInt] < 32) or (FContent[vInt] > 126) then
          vPrintable := False;
      if vPrintable then
        Result := Result + '>' + ContentText + '</' + FTag + '>'
      else
        Result := Result + '>[' + RALWAHex(FContent) + ']</' + FTag + '>';
    end;
  else
    Result := Result + '>';
    for vInt := 0 to FChildren.Count - 1 do
      Result := Result + TRALWhatsAppNode(FChildren[vInt]).ToText;
    Result := Result + '</' + FTag + '>';
  end;
end;

{ TRALWAWriter }

function TRALWAWriter.Bytes: TBytes;
begin
  Result := Copy(FData, 0, FLen);
end;

procedure TRALWAWriter.PutByte(AValue: Byte);
begin
  PutBytes(@AValue, 1);
end;

procedure TRALWAWriter.PutBytes(AData: PByte; ACount: IntegerRAL);
begin
  if ACount <= 0 then
    Exit;
  if FLen + ACount > Length(FData) then
    SetLength(FData, 2 * (FLen + ACount) + 64);
  Move(AData^, FData[FLen], ACount);
  Inc(FLen, ACount);
end;

procedure TRALWAWriter.WriteJid(const AUser: StringRAL; ADevice: IntegerRAL;
  const AServer: StringRAL);
begin
  if ADevice > 0 then
  begin
    PutByte(tagADJid);
    if AServer = cRALWALIDServer then
      PutByte(1)
    else if AServer = 'hosted' then
      PutByte(128)
    else if AServer = 'hosted.lid' then
      PutByte(129)
    else
      PutByte(0);
    PutByte(Byte(ADevice));
    WriteString(AUser);
  end
  else
  begin
    PutByte(tagJidPair);
    if AUser = '' then
      PutByte(tagListEmpty)
    else
      WriteString(AUser);
    WriteString(AServer);
  end;
end;

procedure TRALWAWriter.WriteListStart(ASize: IntegerRAL);
begin
  if ASize = 0 then
    PutByte(tagListEmpty)
  else if ASize < 256 then
  begin
    PutByte(tagList8);
    PutByte(Byte(ASize));
  end
  else
  begin
    PutByte(tagList16);
    PutByte(Byte(ASize shr 8));
    PutByte(Byte(ASize));
  end;
end;

procedure TRALWAWriter.WriteNode(ANode: TRALWhatsAppNode);
var
  vInt, vSize: IntegerRAL;
begin
  vSize := 1 + 2 * ANode.AttrCount;
  if ANode.ContentKind <> wckNone then
    Inc(vSize);
  WriteListStart(vSize);
  WriteString(ANode.Tag);
  for vInt := 0 to ANode.AttrCount - 1 do
  begin
    WriteString(ANode.Attrs[vInt].Name);
    WriteString(ANode.Attrs[vInt].Value);
  end;
  case ANode.ContentKind of
    wckBytes:
      if Length(ANode.Content) = 0 then
        WriteRaw(nil, 0)
      else
        WriteRaw(@ANode.Content[0], Length(ANode.Content));
    wckNodes:
    begin
      WriteListStart(ANode.ChildCount);
      for vInt := 0 to ANode.ChildCount - 1 do
        WriteNode(ANode.Children[vInt]);
    end;
  end;
end;

/// The 4-bit code of a character in a packed text of kind ATag.
function PackChar(AChar: AnsiChar; ATag: Byte): Byte;
begin
  if AChar = #0 then
    Result := 15
  else if AChar in ['0'..'9'] then
    Result := Ord(AChar) - Ord('0')
  else if ATag = tagNibble8 then
  begin
    if AChar = '-' then
      Result := 10
    else
      Result := 11;
  end
  else
    Result := Ord(AChar) - Ord('A') + 10;
end;

procedure TRALWAWriter.WritePacked(const AText: StringRAL; ATag: Byte);
var
  vInt, vRounded: IntegerRAL;
  vLow: AnsiChar;
begin
  PutByte(ATag);
  vRounded := (Length(AText) + 1) div 2;
  if Odd(Length(AText)) then
    vRounded := vRounded or 128;
  PutByte(Byte(vRounded));
  for vInt := 0 to (Length(AText) + 1) div 2 - 1 do
  begin
    if 2 * vInt + 2 <= Length(AText) then
      vLow := AnsiChar(AText[2 * vInt + 2])
    else
      vLow := #0;
    PutByte((PackChar(AnsiChar(AText[2 * vInt + 1]), ATag) shl 4) or PackChar(vLow, ATag));
  end;
end;

procedure TRALWAWriter.WriteRaw(AData: PByte; ACount: IntegerRAL);
begin
  if ACount < 256 then
  begin
    PutByte(tagBinary8);
    PutByte(Byte(ACount));
  end
  else if ACount < 1 shl 20 then
  begin
    PutByte(tagBinary20);
    PutByte(Byte((ACount shr 16) and $0F));
    PutByte(Byte(ACount shr 8));
    PutByte(Byte(ACount));
  end
  else
  begin
    PutByte(tagBinary32);
    PutByte(Byte(ACount shr 24));
    PutByte(Byte(ACount shr 16));
    PutByte(Byte(ACount shr 8));
    PutByte(Byte(ACount));
  end;
  PutBytes(AData, ACount);
end;

procedure TRALWAWriter.WriteString(const AText: StringRAL);
var
  vCode, vDevice: IntegerRAL;
  vUser, vServer: StringRAL;
begin
  vCode := TokenOf(AText);
  if vCode > 0 then
  begin
    if vCode < 256 then
      PutByte(Byte(vCode))
    else
    begin
      PutByte(Byte(tagDictionary0 + (vCode - 256) div 256));
      PutByte(Byte((vCode - 256) mod 256));
    end;
  end
  else if IsNibble(AText) then
    WritePacked(AText, tagNibble8)
  else if IsHex(AText) then
    WritePacked(AText, tagHex8)
  else if RALWhatsAppSplitJid(AText, vUser, vDevice, vServer) and IsJidServer(vServer) then
    WriteJid(vUser, vDevice, vServer)
  else if AText = '' then
    WriteRaw(nil, 0)
  else
    WriteRaw(@AText[1], Length(AText));
end;

{ TRALWAReader }

constructor TRALWAReader.Create(const AData: TBytes);
begin
  inherited Create;
  FData := AData;
end;

function TRALWAReader.GetByte: Byte;
begin
  if FPos >= Length(FData) then
    raise ERALWhatsAppError.Create(rsWABinaryShort);
  Result := FData[FPos];
  Inc(FPos);
end;

function TRALWAReader.GetBytes(ACount: IntegerRAL): TBytes;
begin
  if (ACount < 0) or (FPos + ACount > Length(FData)) then
    raise ERALWhatsAppError.Create(rsWABinaryShort);
  Result := Copy(FData, FPos, ACount);
  Inc(FPos, ACount);
end;

function TRALWAReader.GetInt(AWidth: IntegerRAL): Int64RAL;
begin
  Result := RALWAReadBigEndian(GetBytes(AWidth));
end;

function TRALWAReader.ReadBytesOf(ATag: Byte): TBytes;
var
  vLen: Int64RAL;
begin
  case ATag of
    tagBinary8:
      Result := GetBytes(GetByte);
    tagBinary20:
    begin
      vLen := GetInt(3) and $FFFFF;
      Result := GetBytes(IntegerRAL(vLen));
    end;
    tagBinary32:
    begin
      vLen := GetInt(4);
      if vLen > MaxInt then
        raise ERALWhatsAppError.Create(rsWABinaryBad);
      Result := GetBytes(IntegerRAL(vLen));
    end;
  else
    Result := RALWAText(ReadStringOf(ATag));
  end;
end;

function TRALWAReader.ReadJid(ATag: Byte): StringRAL;
var
  vUser, vServer: StringRAL;
  vAgent, vDevice: IntegerRAL;
begin
  case ATag of
    tagJidPair:
    begin
      vUser := ReadString;
      vServer := ReadString;
      Result := RALWhatsAppJid(vUser, 0, vServer);
    end;
    tagADJid:
    begin
      vAgent := GetByte;
      vDevice := GetByte;
      vUser := ReadString;
      case vAgent of
        1: vServer := cRALWALIDServer;
        128: vServer := 'hosted';
        129: vServer := 'hosted.lid';
      else
        vServer := cRALWAUserServer;
      end;
      Result := RALWhatsAppJid(vUser, vDevice, vServer);
    end;
    tagFBJid:
    begin
      vUser := ReadString;
      vDevice := IntegerRAL(GetInt(2));
      vServer := ReadString;
      Result := RALWhatsAppJid(vUser, vDevice, vServer);
    end;
  else
    // interop: user, device, integrator, server
    vUser := ReadString;
    vDevice := IntegerRAL(GetInt(2));
    vAgent := IntegerRAL(GetInt(2));
    vServer := ReadString;
    Result := RALWhatsAppJid(StringRAL(IntToStr(vAgent)) + '-' + vUser, vDevice, vServer);
  end;
end;

function TRALWAReader.ReadListSize(ATag: Byte): IntegerRAL;
begin
  case ATag of
    tagListEmpty: Result := 0;
    tagList8: Result := GetByte;
    tagList16: Result := IntegerRAL(GetInt(2));
  else
    raise ERALWhatsAppError.Create(rsWABinaryBad);
  end;
end;

function TRALWAReader.ReadNode: TRALWhatsAppNode;
var
  vSize, vInt, vCount: IntegerRAL;
  vTag: Byte;
  vName: StringRAL;
begin
  vSize := ReadListSize(GetByte);
  if vSize = 0 then
    raise ERALWhatsAppError.Create(rsWABinaryBad);
  vName := ReadString;
  if vName = '' then
    raise ERALWhatsAppError.Create(rsWABinaryBad);
  Result := TRALWhatsAppNode.Create(vName);
  try
    for vInt := 1 to (vSize - 1) div 2 do
    begin
      vName := ReadString;
      Result.SetAttr(vName, ReadString);
    end;
    if not Odd(vSize) then
    begin
      vTag := GetByte;
      if vTag in [tagListEmpty, tagList8, tagList16] then
      begin
        vCount := ReadListSize(vTag);
        if vCount = 0 then
          Result.Content := nil
        else
          for vInt := 1 to vCount do
            Result.Add(ReadNode);
      end
      else
        Result.Content := ReadBytesOf(vTag);
    end;
  except
    Result.Free;
    raise;
  end;
end;

/// The character of a 4-bit code in a packed text of kind ATag.
function UnpackChar(ACode: Byte; ATag: Byte): AnsiChar;
begin
  if ACode < 10 then
    Result := AnsiChar(Ord('0') + ACode)
  else if ATag = tagHex8 then
    Result := AnsiChar(Ord('A') + ACode - 10)
  else if ACode = 10 then
    Result := '-'
  else if ACode = 11 then
    Result := '.'
  else if ACode = 15 then
    Result := #0
  else
    raise ERALWhatsAppError.Create(rsWABinaryBad);
end;

function TRALWAReader.ReadPacked(ATag: Byte): StringRAL;
var
  vStart, vInt: IntegerRAL;
  vByte: Byte;
begin
  vStart := GetByte;
  SetLength(Result, 2 * (vStart and 127));
  for vInt := 0 to (vStart and 127) - 1 do
  begin
    vByte := GetByte;
    Result[2 * vInt + 1] := UnpackChar(vByte shr 4, ATag);
    Result[2 * vInt + 2] := UnpackChar(vByte and 15, ATag);
  end;
  if (vStart and 128) <> 0 then
    SetLength(Result, Length(Result) - 1);
end;

function TRALWAReader.ReadString: StringRAL;
begin
  Result := ReadStringOf(GetByte);
end;

function TRALWAReader.ReadStringOf(ATag: Byte): StringRAL;
var
  vBytes: TBytes;
begin
  case ATag of
    tagListEmpty:
      Result := '';
    tagDictionary0..tagDictionary3:
      Result := cDoubleTokens[ATag - tagDictionary0, GetByte];
    tagJidPair, tagADJid, tagFBJid, tagInteropJid:
      Result := ReadJid(ATag);
    tagNibble8, tagHex8:
      Result := ReadPacked(ATag);
    tagBinary8, tagBinary20, tagBinary32:
    begin
      vBytes := ReadBytesOf(ATag);
      SetLength(Result, Length(vBytes));
      if Length(vBytes) > 0 then
        Move(vBytes[0], Result[1], Length(vBytes));
    end;
  else
    if ATag <= High(cSingleTokens) then
      Result := cSingleTokens[ATag]
    else
      raise ERALWhatsAppError.Create(rsWABinaryBad);
  end;
end;

function RALWhatsAppEncodeNode(ANode: TRALWhatsAppNode): TBytes;
var
  vWriter: TRALWAWriter;
begin
  vWriter := TRALWAWriter.Create;
  try
    vWriter.WriteNode(ANode);
    Result := vWriter.Bytes;
  finally
    vWriter.Free;
  end;
end;

function RALWhatsAppDecodeNode(const AData: TBytes): TRALWhatsAppNode;
var
  vReader: TRALWAReader;
begin
  vReader := TRALWAReader.Create(AData);
  try
    Result := vReader.ReadNode;
  finally
    vReader.Free;
  end;
end;

function RALWhatsAppPackNode(ANode: TRALWhatsAppNode): TBytes;
var
  vFlag: TBytes;
begin
  SetLength(vFlag, 1);
  vFlag[0] := 0;
  Result := RALWABytes(vFlag, RALWhatsAppEncodeNode(ANode));
end;

/// AData inflated from zlib.
function Inflate(const AData: TBytes): TBytes;
var
  vZip: TRALCompressZLib;
  vIn, vOut: TMemoryStream;
begin
  vZip := TRALCompressZLib.Create;
  vIn := TMemoryStream.Create;
  vOut := TMemoryStream.Create;
  try
    if Length(AData) > 0 then
      vIn.WriteBuffer(AData[0], Length(AData));
    vIn.Position := 0;
    vZip.Format := ctZLib;
    vZip.DecompressTo(vIn, vOut);
    SetLength(Result, vOut.Size);
    if vOut.Size > 0 then
    begin
      vOut.Position := 0;
      vOut.ReadBuffer(Result[0], vOut.Size);
    end;
  finally
    vOut.Free;
    vIn.Free;
    vZip.Free;
  end;
end;

function RALWhatsAppUnpackNode(const AData: TBytes): TRALWhatsAppNode;
var
  vBody: TBytes;
begin
  if Length(AData) < 2 then
    raise ERALWhatsAppError.Create(rsWABinaryShort);
  vBody := RALWACopy(AData, 1, Length(AData) - 1);
  if (AData[0] and 2) <> 0 then
    vBody := Inflate(vBody);
  Result := RALWhatsAppDecodeNode(vBody);
end;

initialization
  BuildTokenIndex;

finalization
  gTokenIndex.Free;

end.
