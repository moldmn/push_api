%%%-------------------------------------------------------------------
%%% @author dmn
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Янв. 2017 3:06
%%%-------------------------------------------------------------------
-module(push_api_sup).
-author("dmn").

-behaviour(supervisor).


-export([start_link/0, create_apns_pool/3,create_gcm_pool/3,delete_pool/1]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
%% @hidden
-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | shutdown | term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


create_apns_pool(PoolName, Connection, Size)->
  SizeArgs = [{size, Size}, {max_overflow, 0}],
  PoolArgs = [{name, {local, PoolName}}, {worker_module, apns_connection}],
  PoolSpec = poolboy:child_spec(PoolName, PoolArgs ++ SizeArgs, Connection),

  supervisor:start_child(?MODULE, PoolSpec)
.

create_gcm_pool(PoolName, Connection, Size)->
  SizeArgs = [{size, Size}, {max_overflow, 0}],
  PoolArgs = [{name, {local, PoolName}}, {worker_module, gcm_connection}],
  PoolSpec = poolboy:child_spec(PoolName, PoolArgs ++ SizeArgs, Connection),

  supervisor:start_child(?MODULE, PoolSpec)
.


-spec(delete_pool(PoolName::atom()) -> ok | {error,not_found}).
delete_pool(PoolName) ->
  supervisor:terminate_child(?MODULE, PoolName),
  supervisor:delete_child(?MODULE, PoolName).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
  {ok,
    {{one_for_one, 5, 10},
      [
      ]
    }
  }.
