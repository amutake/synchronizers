%%%-------------------------------------------------------------------
%% @doc alpha_traverse server.
%% @end
%%%-------------------------------------------------------------------

-module(alpha_traverse_server).

-behaviour(synchronizers_alpha).

%% API
-export([start_link/3]).

%% Synchronizer callbacks
-export([init/1, handle_pulse/2, handle_message/3]).

-define(SERVER, ?MODULE).
-define(STATE, ?SERVER).
-record(?STATE, {
           neighbors :: [synchronizers_alpha:neighbor()],
           parent = undefined :: 'undefined' | synchronizers_alpha:neighbor(),
           level :: pos_integer(),
           start = false :: boolean()
          }).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link(synchronizers_alpha:name(), [synchronizers_alpha:neighbor()], boolean()) -> Result when
  Result :: {ok, pid()} | ignore | {error, Reason::term()}.
start_link(Name, Neighbors, Start) ->
    synchronizers_alpha:start_link(Name, Neighbors, ?MODULE, {Neighbors, Start}, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init({Neighbors, Start}) ->
    State = #?STATE{neighbors = Neighbors, start = Start},
    {ok, State}.

handle_pulse(1, State = #?STATE{neighbors = Neighbors, start = true}) ->
    {ok, maps:from_list([{Neighbor, {level, 1}} || Neighbor <- Neighbors]), State};
handle_pulse(Level, State = #?STATE{neighbors = Neighbors, level = Level}) ->
    {ok, maps:from_list([{Neighbor, {level, Level}} || Neighbor <- Neighbors]), State};
handle_pulse(_Level, State) ->
    {ok, #{}, State}.

handle_message({level, Level}, Neighbor, State = #?STATE{parent = undefined}) ->
    State#?STATE{parent = Neighbor, level = Level + 1};
handle_message(_Msg, _Neighbor, State) -> State.

%%====================================================================
%% Internal functions
%%====================================================================
