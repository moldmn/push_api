-author("dmn").

%headers
-define(APNS_EXPIRY,        expiry).
-define(APNS_PRIORITY,      priority).
-define(APNS_TOPIC,         topic).
-define(APNS_ID,            'apns-id').
-define(APNS_AUTHORIZATION, authorization).

% aps
-define(APNS_APS,                   aps).
-define(APNS_APS_ALERT,             alert).
-define(APNS_APS_BADGE,             badge).
-define(APNS_APS_SOUND,             sound).
-define(APNS_APS_CONTENT_AVAILABLE, 'content-available').
-define(APNS_APS_CATEGORY,          category).
-define(APNS_APS_THREAD_ID,         'thread-id').
-define(APNS_APS_MUTABLE_CONTENT,   'mutable-content').

% alert
-define(APNS_ALERT_TITLE,          title).
-define(APNS_ALERT_BODY,           body).
-define(APNS_ALERT_TITLE_LOC_KEY,  'title-loc-key').
-define(APNS_ALERT_TITLE_LOC_ARGS, 'title-loc-args').
-define(APNS_ALERT_ACTION_LOC_KEY, 'action-loc-key').
-define(APNS_ALERT_LOC_KEY,        'loc-key').
-define(APNS_ALERT_LOC_ARGS,       'loc-args').
-define(APNS_ALERT_LAUNCH_IMAGE,   'launch-image').

-define(APNS_APS_FIELDS, #{
  ?APNS_APS_ALERT => true,
  ?APNS_APS_BADGE => true,
  ?APNS_APS_SOUND => true,
  ?APNS_APS_CONTENT_AVAILABLE => true,
  ?APNS_APS_CATEGORY => true,
  ?APNS_APS_THREAD_ID => true,
  ?APNS_APS_MUTABLE_CONTENT => true
}).

-define(APNS_ALERT_FIELDS, #{
  ?APNS_ALERT_TITLE => true,
  ?APNS_ALERT_BODY => true,
  ?APNS_ALERT_TITLE_LOC_KEY => true,
  ?APNS_ALERT_TITLE_LOC_ARGS => true,
  ?APNS_ALERT_ACTION_LOC_KEY => true,
  ?APNS_ALERT_LOC_ARGS => true,
  ?APNS_ALERT_LAUNCH_IMAGE => true
}).