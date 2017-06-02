-module(gcm_api).


-export([push/3]).
-export([web_push/4]).

-define(BASEURL, "https://android.googleapis.com/gcm/send").
-define(TEMP_GCM_URL, "https://gcm-http.googleapis.com/gcm").

-type header()       :: {string(), string()}.
-type headers()      :: [header()].
-type regid()        :: binary().
-type regids()       :: [binary(),...].
-type message()      :: map() | binary().
-type result()       :: {number(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [any()]}.
-type subscription() :: {regid(), webpush_encryption:publicKey(), webpush_encryption:authTokeny()}.

-spec push(regids(),message(),map()) -> {'error',any()} | {'noreply','unknown'} | {'ok',result()}.
push(RegIds, Message, State) ->
  Request = jsone:encode(Message#{<<"registration_ids">> => RegIds}),
  push(Request, State, [], ?BASEURL).

-spec push(message(), map(), headers(), string()) -> {'error',any()} | {'noreply','unknown'} | {'ok',result()} | {'ok','ok'}.
push(Request, #{key:=Key, report_error_fun:=ErrorFun}, Headers, BaseUrl) ->
  ApiKey = string:concat("key=", Key),
  try httpc:request(post, {BaseUrl, [{"Authorization", ApiKey}|Headers], "application/json", Request}, [], []) of
    {ok, {{_, 200, _}, _Headers, Body}} ->
      Json = jsone:decode(response_to_binary(Body), [{object_format, map}]),
      {ok, Json};
    {ok, {{_, 201, _}, _Headers, _Body}} ->				% web push success result
      {ok, [{<<"multicast_id">>, <<"">>},{<<"success">>,1},{<<"failure">>,0},{<<"canonical_ids">>,0},{<<"results">>, []} ] };
    {ok, {{_, 400, _}, _, Body}} ->
      ErrorFun("Error in request. Reason was: Bad Request - ~p~n", [Body]),
      {error, Body};
    {ok, {{_, 401, _}, _, _}} ->
      ErrorFun("Error in request. Reason was: authorization error~n", []),
      {error, auth_error};
    {ok, {{_, Code, _}, _RespHeaders, _}} when Code >= 500 andalso Code =< 599 ->
      RetryTime = 3,
      ErrorFun("Error in request. Reason was: retry. Will retry in: ~p~n", [RetryTime]),
      {error, {retry, RetryTime}};
    {ok, {{StatusLine, RespHeaders, _}, _, _Body}} ->
      ErrorFun("Error in request. Reason was: timeout1 ~p ~p ~n", [StatusLine, RespHeaders]),
      {error, timeout};
    {error, Reason} ->
      ErrorFun("Error in request. Reason was: ~p~n", [Reason]),
      {error, Reason};
    OtherError ->
      ErrorFun("Error in request. Reason other was: ~p~n", [OtherError]),
      {noreply, unknown}
  catch
    Exception ->
      ErrorFun("Error in request. Exception ~p while calling URL: ~p~n", [Exception, BaseUrl]),
      {error, Exception}
  end.

-spec web_push(message(), map(), subscription(), non_neg_integer()) -> {'error',any()} | {'noreply','unknown'} | {'ok',result()}.
web_push(Message, State, {RegId, ClientPublicKey, ClientAuthToken} = _Subscription, PaddingLength) ->
  {Ciphertext, Salt, PublicKey} = webpush_encryption:encrypt(jsone:encode(Message), {ClientPublicKey, ClientAuthToken}, PaddingLength),
  Headers = [
    {"Content-Encoding", "aesgcm"}
    , {"Encryption", "salt=" ++ binary_to_list( base64url:encode(Salt)) }
    , {"Crypto-Key", "dh=" ++ binary_to_list( base64url:encode(PublicKey)) }
    , {"TTL", "0"}
  ],
  PushServerUri = ?TEMP_GCM_URL ++ "/" ++ binary_to_list(RegId),
  push(Ciphertext, State, Headers, PushServerUri).

-spec response_to_binary(binary() | list()) -> binary().
response_to_binary(Json) when is_binary(Json) ->
  Json;

response_to_binary(Json) when is_list(Json) ->
  list_to_binary(Json).
