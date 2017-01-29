%%%-------------------------------------------------------------------
%%% @author dmn
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. янв 2017 17:19
%%%-------------------------------------------------------------------
-module(apns_message).
-author("dmn").

-include("apns.hrl").

%% API
-export([get_message/1]).


get_message(Message) when is_binary(Message)->
  Message
;

get_message(Message) when is_map(Message)->
  Keys = maps:keys(Message),
  get_message(Keys,Message,#{})
.

get_message([],_MessageMap,ResultMap)->
  jsone:encode(ResultMap)
;
get_message([Key|KeyTail]=Keys,MessageMap,ResultMap)->
  case maps:find(Key,?APNS_APS_FIELDS) of
  {ok, true} ->
    Map = #{?APNS_APS => #{Key => maps:get(Key,MessageMap ) }},
    get_message(KeyTail,MessageMap, recursive_merge(Map,ResultMap) )
  ;
  error ->
    get_message_alert(Keys,MessageMap,ResultMap)
  end
.

%% @private
get_message_alert([Key|KeyTail],MessageMap,ResultMap)->
  case maps:find(Key,?APNS_ALERT_FIELDS) of
  {ok, true} ->
    Map = #{?APNS_APS => #{ ?APNS_APS_ALERT => #{Key => maps:get(Key,MessageMap ) } }},
    get_message(KeyTail,MessageMap, recursive_merge(Map,ResultMap) )
  ;
    error -> get_message(KeyTail,MessageMap, ResultMap#{ Key => maps:get(Key,MessageMap ) })
  end
.



%% @private
recursive_merge(Map1, undefined) -> Map1;
recursive_merge(Map1, Map2) ->
  Keys = maps:keys(Map1),
  Merged = do_merge(Keys, Map1, Map2),
  maps:merge(Map2, Merged).


%% @private
do_merge([], Map, _) -> Map;
do_merge([Key | Keys], Map, Map2) ->
  Updated = case maps:find(Key, Map2) of
              error ->
                Map;
              {ok, Value} when is_map(Value) ->
                Merged = recursive_merge(maps:get(Key, Map), Value),
                Map#{Key => Merged};
              {ok, Value} ->
                Map#{Key => Value}
            end,
  do_merge(Keys, Updated, Map2).