%%%-------------------------------------------------------------------
%% @doc alpha_traverse top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(alpha_traverse_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    P1 = #{
      id => p1,
      start => {alpha_traverse_server, start_link, [p1, [p2, p3], false]}},
    P2 = #{
      id => p2,
      start => {alpha_traverse_server, start_link, [p2, [p1, p4], false]}},
    P3 = #{
      id => p3,
      start => {alpha_traverse_server, start_link, [p3, [p1, p4], false]}},
    P4 = #{
      id => p4,
      start => {alpha_traverse_server, start_link, [p4, [p2, p3], true]}},
    {ok, { {one_for_all, 0, 1}, [P1, P2, P3, P4]} }.

%%====================================================================
%% Internal functions
%%====================================================================
