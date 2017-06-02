%%% @doc This gen_server handles the APNs Connection.
%%%
%%% Copyright 2017 Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(apns_connection).
-author("Felipe Ripoll <ferigis@inakanetworks.com>").

-behaviour(gen_server).

-include("apns.hrl").

%% API
-export([ start_link/1
        , name/1
        , host/1
        , port/1
        , gun_connection/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-export_type([ name/0
             , host/0
             , port/0
             , path/0
             , connection/0
             ]).

-type name()         :: atom().
-type host()         :: string() | inet:ip_address().
-type path()         :: string().
-opaque connection() :: #{ name       => name()
                         , apple_host => host()
                         , apple_port => inet:port_number()
                         , certfile   => path()
                         , keyfile    => path()
                         }.

-type state()        :: #{ connection     => connection()
                         , gun_connection => pid()
                         , gun_monitor    => reference()
                         , last_token     => binary() | none
                         , last_message   => {binary(),integer()} | none
                         }.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc starts the gen_server
-spec start_link(connection()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Connection) ->

  gen_server:start_link(?MODULE, Connection, []).


%% @doc Returns the gun's connection PID. This function is only used in tests.
-spec gun_connection(name()) -> pid().
gun_connection(ConnectionName) ->
  gen_server:call(ConnectionName, gun_connection).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(connection()) -> {ok, State :: state()}.
init(Connection) ->
  {GunMonitor, GunConnectionPid} = open_gun_connection(Connection),
  {ok, #{ connection     => get_config(Connection)
        , gun_connection => GunConnectionPid
        , gun_monitor    => GunMonitor
        }}.

-spec handle_call( Request :: term()
                 , From    :: {pid()
                 , Tag     :: term()}
                 , State
                 ) -> {reply, ok, State}.
handle_call(gun_connection, _From, #{gun_connection := GunConn} = State) ->
  {reply, GunConn, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(Request :: term(), State) ->
  {noreply, State}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State) ->
  {noreply, State}.
handle_info({send, APNSID, Message, Headers}, #{gun_connection := GunConnection} = State) ->
  Path = <<"/3/device/", APNSID/binary>>,

  Expiry = maps:get(?APNS_EXPIRY,Headers,0),
  Priority = maps:get(?APNS_PRIORITY,Headers,10),

  BaseHeaders = [
    {<<"apns-expiration">>, integer_to_binary(Expiry)},
    {<<"apns-priority">>, integer_to_binary(Priority)}
  ],
  MsgHeaders = set_other_header(Headers,BaseHeaders),

  MsgData = apns_message:get_message(Message),
  gun:post(GunConnection,Path,MsgHeaders,MsgData),
  {noreply, State#{last_token => APNSID}}
;
handle_info( {'DOWN', GunMonitor, process, GunConnPid, _}
           , #{ gun_connection := GunConnPid
              , gun_monitor    := GunMonitor
              , connection     := Connection
              } = State) ->
  #{info_logger_fun := InfoFun, pool_name := PoolName} = Connection,
  InfoFun("apns worker from pool ~p: reconnecting ~p~n",[PoolName,GunConnPid]),
  {GunMonitor2, GunConnPid2} = open_gun_connection(Connection),
  {noreply, State#{gun_connection => GunConnPid2, gun_monitor => GunMonitor2}}
;
handle_info({gun_response,_,_,nofin,Code,[{<<"apns-id">>,ApnsId}]}, State) ->
  {noreply, State#{last_message => {ApnsId,Code}}}
;
handle_info({gun_response,_,_,_,Code,[{<<"apns-id">>,ApnsId}]}, #{connection := #{report_result_fun := ResultFun}}=State) ->
  ResultFun(ApnsId,Code),
  {noreply, State}
;
handle_info({gun_data,_,_,fin,Error}, #{connection := #{report_error_fun := ErrorFun}, last_message := {ApnsId,Code}, last_token := LastToken}=State) ->
  ErrorMsg = jsone:decode(Error,[{object_format, map}]),
  ErrorFun(ApnsId,Code,LastToken,ErrorMsg),
  {noreply, State}
;
handle_info({gun_up,Pid,http2}, #{connection := #{info_logger_fun := InfoFun, pool_name := PoolName}}=State) ->
  InfoFun("apns worker from pool ~p: connected ~p~n",[PoolName,Pid]),
  {noreply, State}
;
handle_info(Info, #{connection := #{info_logger_fun := InfoFun, pool_name := PoolName}}=State) ->
  InfoFun("apns worker from pool ~p: receive info ~p~n",[PoolName, Info]),
  {noreply, State}
.


-spec terminate( Reason :: (normal | shutdown | {shutdown, term()} | term())
               , State  :: map()
               ) -> term().
terminate(_Reason, _State) ->
  ok.

-spec code_change(OldVsn :: term() | {down, term()}
                 , State
                 , Extra :: term()
                 ) -> {ok, State}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Connection getters/setters Functions
%%%===================================================================

-spec name(connection()) -> name().
name(#{name := ConnectionName}) ->
  ConnectionName.

-spec host(connection()) -> host().
host(#{apple_host := Host}) ->
  Host.

-spec port(connection()) -> inet:port_number().
port(#{apple_port := Port}) ->
  Port.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec open_gun_connection(connection()) -> { GunMonitor       :: reference()
                                           , GunConnectionPid :: pid()
                                           }.
open_gun_connection(Connection) ->
  Host = host(Connection),
  Port = port(Connection),
  TransportOpts = get_transport_opts(Connection),
  {ok, GunConnectionPid} = gun:open( Host
                                   , Port
                                   , #{ protocols => [http2]
                                      , transport_opts => TransportOpts
                                      }),
  GunMonitor = monitor(process, GunConnectionPid),
  {GunMonitor, GunConnectionPid}.

%% @private
set_other_header(#{?APNS_TOPIC := Topic}=Map, Headers)->
  set_other_header(maps:remove(?APNS_TOPIC,Map),[{<<"apns-topic">>,Topic}|Headers])
;
set_other_header(#{?APNS_ID := ApnsId}=Map, Headers)->
  set_other_header(maps:remove(?APNS_ID,Map),[{<<"apns-id">>,ApnsId}|Headers])
;
set_other_header(#{?APNS_AUTHORIZATION := Authorization}=Map, Headers)->
  set_other_header(maps:remove(?APNS_AUTHORIZATION,Map),[{<<"authorization">>,Authorization}|Headers])
;
set_other_header(_, Headers)->
  Headers
.

%% @private
get_config(Config)->
  maps:merge(get_default_config(),Config)
.

%% @private
get_default_config()->
  #{
    report_result_fun => fun(ApnsId,Code)-> error_logger:info_msg("apnsId : ~p, code: ~p~n", [ApnsId,Code]) end,
    report_error_fun => fun(ApnsId,Code,Token,Error)-> error_logger:error_msg("apnsId : ~p, code: ~p, token: ~p, error: ~p~n", [ApnsId,Code,Token,Error]) end,
    info_logger_fun => fun(Format, Args)->error_logger:info_msg(Format, Args) end
  }
.

%% @private
get_transport_opts(Map)->
  get_transport_opts(Map,[])
.
get_transport_opts(#{certfile := CertFile}=Map,Result)->
  get_transport_opts(maps:remove(certfile,Map), [{certfile, CertFile}|Result])
;
get_transport_opts(#{cert := CertFile}=Map,Result)->
  get_transport_opts(maps:remove(cert,Map), [{cert, CertFile}|Result])
;
get_transport_opts(#{keyfile := CertFile}=Map,Result)->
  get_transport_opts(maps:remove(keyfile,Map), [{keyfile, CertFile}|Result])
;
get_transport_opts(#{key := CertFile}=Map,Result)->
  get_transport_opts(maps:remove(key,Map), [{key, CertFile}|Result])
;
get_transport_opts(_Map,Result)->
  Result
.