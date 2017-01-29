-module(push_api_app).

-behaviour(application).

%% Application callbacks
-export([ start/2
        , stop/1
        ]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(term(), term()) -> {error, term()} | {ok, pid()}.
start(_StartType, _StartArgs) ->
  push_api_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
  ok.
