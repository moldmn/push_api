-module(push_api).

%% API
-export([ start/0,
          stop/0,
          send_apns/3,
          send_apns/4,
          start_apns/1,
          start_gcm/1,
          send_gcm/3
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(push_api),
  ok.

%% @doc Stops the Application
-spec stop() -> ok.
stop() ->
  ok = application:stop(push_api).


start_apns(#{pool_name := PoolName, pool_size := PoolSize}=Config)->
  push_api_sup:create_apns_pool(PoolName,Config, PoolSize)
.

start_gcm(#{pool_name := PoolName, pool_size := PoolSize}=Config)->
  push_api_sup:create_gcm_pool(PoolName, Config, PoolSize)
.

send_apns(PoolId,Token,Message)->
  send_apns(PoolId,Token,Message,#{})
.
send_apns(PoolId,Token,Message,Headers)->
  poolboy:transaction(PoolId, fun(Worker) ->  Worker ! {send, Token, Message, Headers}  end)
.

send_gcm(PoolId,RegIds,Message)->
  poolboy:transaction(PoolId, fun(Worker) ->  gen_server:cast(Worker, {send, RegIds, Message, 3}) end)
.



%%%===================================================================
%%% Internal Functions
%%%===================================================================
