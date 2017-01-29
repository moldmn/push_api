push_api
=======

This software provides an Erlang client for [`Google Cloud Messaging`](http://developer.android.com/google/gcm/index.html "Google Cloud Messaging for Android") and [`Apple Push Notification service`](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/APNSOverview.html "Apple Push Notification service").

### How to use with rebar:
You can use push_api as a dependency in your rebar.config:

    {deps , [
        {gcm, ".*", {git, "https://github.com/moldmn/push_api.git", {branch, "master"}}}
    ]}.

### How to start apns:
    Config = #{
      %pool
      pool_name  => test_apns,
      pool_size  => 5,

      %connection
      apple_host => "api.push.apple.com",
      apple_port => 443,
      certfile   => "test.pem",
      keyfile    => "test.pem",

      %funs
      report_result_fun => fun(ApnsId, Code)->
        error_logger:info_msg("apnsId : ~p, code: ~p~n", [ApnsId,Code]) end
      ,
      report_error_fun => fun(ApnsId, Code, Token, Error)->
        error_logger:error_msg("apnsId : ~p, code: ~p, token: ~p, error: ~p~n", [ApnsId,Code,Token,Error]) end
      ,
      info_logger_fun => fun(Format, Args)->
        error_logger:info_msg(Format, Args) end
    },
    push_api:start_apns(Config)

### How to send message via apns:
    -include("apns.hrl").

    Message = #{?APNS_APS_ALERT => <<"hello">>},

    push_api:send_apns(
      test_apns,
      <<"658a936c587f3645876358e9723a48733d587658c3658793b587937a598b3538">>,
      Message,
      #{?APNS_TOPIC => <<"my.apns.topic">>, ?APNS_ID => <<"11111111-2222-3333-4444-555555555555">>}
    )

### How to start gcm:
    Config = #{
      %pool
      pool_name  => test_gcm,
      pool_size  => 5,
      %connection
      key => "HPsvIyBK0N2S5_-L-ndeD=Zmu2l7SHnaIMfgwAn",
      %funs
      report_result_fun => fun(RegId, MessageId)-> error_logger:info_msg("regId: ~p, messageId: ~p", [RegId, MessageId]) end,
      report_error_fun => fun(RegId, Reason)-> error_logger:error_msg("regId: ~p, reason: ~p", [RegId, Reason]) end,
      info_logger_fun => fun(Format, Args)-> error_logger:info_msg(Format, Args) end
    },
    push_api:start_gcm(Config)

### How to send message via gcm:
    Message = #{data => #{message => <<"test">>}},

    push_api:send_gcm(
      test_gcm,
      [<<"nlwHB8v1ITE:APA91bHUMj7gENjpmw-n0mUPetL-Ge1tjcn_61FOU2eMt21bbEk48KAskt1-uUYYTat2gAErNneu9PHtk5mnsEgGdy__1RMv9joyormdAqiNnSdkbPnelo6BqVccggcocJTJsc5BcqSg">>],
      Message
    )