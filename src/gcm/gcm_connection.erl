-module(gcm_connection).

-behaviour(gen_server).

-export([  start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-export([push/3, push/4, sync_push/3, sync_push/4]).
-export([web_push/3, web_push/4, sync_webpush/3, sync_webpush/4]).

-define(SERVER, ?MODULE).
-define(RETRY, 3).

-type publicKey()    :: string()|binary().
-type authTokeny()   :: string()|binary().
-type regid()        :: binary().
-type subscription() :: {regid(), publicKey(), authTokeny()}.

-export_type([subscription/0]).

-record(state, {key}).


push(Name, RegIds, Message) ->
  push(Name, RegIds, Message, ?RETRY).

push(Name, RegIds, Message, Retry) ->
  gen_server:cast(Name, {send, RegIds, Message, Retry}).

web_push(Name, Message, Subscription) ->
  web_push(Name, Message, Subscription, ?RETRY).

web_push(Name, Message, Subscription, Retry) ->
  gen_server:cast(Name, {send_webpush, Message, Subscription, Retry}).

sync_push(Name, RegIds, Message) ->
  sync_push(Name, RegIds, Message, ?RETRY).

sync_push(Name, RegIds, Message, Retry) ->
  gen_server:call(Name, {send, RegIds, Message, Retry}).

sync_webpush(Name, Message, Subscription) ->
  sync_webpush(Name, Message, Subscription, ?RETRY).

sync_webpush(Name, Message, Subscription, Retry) ->
  gen_server:call(Name, {send_webpush, Message, Subscription, Retry}).

%% OTP
start_link(Config) ->
  gen_server:start_link( ?MODULE, Config, []).

init(Config) ->
  {ok, get_config(Config)}.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call({send, RegIds, Message, Retry}, _From, State) ->
  Reply = do_push(RegIds, Message, State, Retry),
  {reply, Reply, State};

handle_call({send_webpush, Message, Subscription, Retry}, _From, State) ->
  Reply = do_web_push(Message, State, Subscription, 0, Retry),
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({send, RegIds, Message, Retry}, State) ->
  do_push(RegIds, Message, State, Retry),
  {noreply, State};

handle_cast({send_webpush, Message, Subscription, Retry}, #{key:=Key} = State) ->
  do_web_push(Message, Key, Subscription, 0, Retry),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal
do_push(_, _, _, 0) ->
  ok;

do_push(RegIds, Message, #{info_logger_fun:=LogFun}=State, Retry) ->
  LogFun("gcm: Sending message: ~p to reg ids: ~p retries: ~p.~n", [
    filter_message_params(Message,[<<"message">>, <<"chat_id">>, <<"text">>, <<"name">>]),
    RegIds,
    Retry
  ]),
  case gcm_api:push(RegIds, Message, State) of
    {ok, GCMResult} ->
      handle_result(GCMResult, RegIds,State);
    {error, {retry, RetryAfter}} ->
      do_backoff(RetryAfter, RegIds, Message, Retry,State),
      {error, retry};
    {error, Reason} ->
      {error, Reason}
  end.

do_web_push(_, _, _, _, 0) ->
  ok;

do_web_push(Message, #{info_logger_fun:=LogFun}=State, Subscription, PaddingLength, Retry) ->
  LogFun("gcm: Sending web push message: ~p to subscription: ~p retries: ~p.~n", [Message, Subscription, Retry]),
  case gcm_api:web_push(Message, State, Subscription, PaddingLength) of
    {ok, GCMResult} ->
      handle_result(GCMResult, [Subscription],State);
    {error, {retry, RetryAfter}} ->
      do_backoff(RetryAfter, Subscription, Message, Retry,State),
      {error, retry};
    {error, Reason} ->
      {error, Reason}
  end.

handle_result(#{<<"results">> := Results}, RegIds, State) ->
  lists:map(fun({Result, RegId}) -> {RegId, parse(Result,RegId,State)} end, lists:zip(Results, RegIds))
.

do_backoff(RetryAfter, {_,_,_} = Subscription, Message, Retry, #{info_logger_fun := InfoFun}) ->
  case RetryAfter of
    no_retry ->
      ok;
    _ ->
      InfoFun("Received retry-after. Will retry: ~p times~n", [Retry-1]),
      timer:apply_after(RetryAfter * 1000, ?MODULE, web_push, [self(), Message, Subscription, Retry - 1])
  end;

do_backoff(RetryAfter, RegIds, Message, Retry, #{info_logger_fun := InfoFun}) ->
  case RetryAfter of
    no_retry ->
      ok;
    _ ->
      InfoFun("Received retry-after. Will retry: ~p times~n", [Retry-1]),
      timer:apply_after(RetryAfter * 1000, ?MODULE, push, [self(), RegIds, Message, Retry - 1])
  end.

parse(#{<<"error">> := Error},RegId,#{report_error_fun := ErrFun}) ->
  ErrFun(RegId,Error),
  Error
;
parse(#{<<"message_id">> := MessageId},RegId,#{report_result_fun := ResFun}) ->
  ResFun(RegId,MessageId),
  ok
;
parse(#{<<"registration_id">> := NewRegId},_RegId,_State) ->
  {<<"NewRegistrationId">>, NewRegId}
.

%% @private
get_config(Config)->
  maps:merge(get_default_config(),Config)
.

%% @private
get_default_config()->
  #{
    report_result_fun =>  fun(RegId, MessageId)-> error_logger:info_msg("regId: ~p, messageId: ~p", [RegId, MessageId]) end,
    report_error_fun  =>  fun(RegId, Reason)-> error_logger:error_msg("regId: ~p, reason: ~p", [RegId, Reason]) end,
    info_logger_fun   =>  fun(Format, Args)-> error_logger:info_msg(Format, Args) end
  }
.

%% @private
filter_message_params(#{data := Data} = Message, ParamList) ->
  FilteredData = maps:filter(fun(K,_V) -> not lists:member(K, ParamList) end, Data),
  Message#{data => FilteredData};
filter_message_params(Message, _ParamList) ->
  Message.
