-module(synchronizers_alpha).

-behavior(gen_server).

%% API exports
-export([start_link/5]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export_type([
              neighbor/0,
              clock/0,
              state/0
             ]).

-define(STATE, ?MODULE).

-record(?STATE,
        {
           module :: atom(),
           name :: atom(),
           clock = 0 :: clock(),
           neighbors = [] :: [neighbor()],
           expected_ack = 0 :: non_neg_integer(),
           neighbors_safe = [] :: [neighbor()], % as multiset
           next_messages = [] :: [{neighbor(), term()}]
          }).

-type name() :: atom().
-type neighbor() :: name().
-type clock() :: non_neg_integer().
-type app_state() :: term().
-type state() :: #?STATE{}.
-type message_defs() :: #{neighbor() => term()}.
%% #{Target::neighbor() => Msg::term()}

%%====================================================================
%% Callbacks
%%====================================================================

-callback init(Args::term()) -> {ok, app_state()}.

%% @doc handle pulse
%%
%% [message_def()] must be
-callback handle_pulse(clock(), app_state()) -> {ok, message_defs(), app_state()}.

%% @doc handle message
-callback handle_message(Msg::term(), neighbor(), app_state()) -> app_state().

%%====================================================================
%% API functions
%%====================================================================
-spec start_link(Name, Module, Args, Neighbors, Options) -> Result when
  Name :: name(),
  Module :: atom(),
  Args :: term(),
  Neighbors :: [neighbor()],
  Options :: term(),
  Result :: {ok, pid()} | ignore | {error, Reason::term()}.
start_link(Name, Neighbors, Mod, Args, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, {Mod, Name, Neighbors, Args}, Options).

%%====================================================================
%% gen_server Callbacks
%%====================================================================
init({Mod, Name, Neighbors, Args}) ->
    {ok, AppState} = Mod:init(Args),
    SyncState = #?STATE{
                    module = Mod,
                    name = Name,
                    clock = 0,
                    neighbors = Neighbors,
                    expected_ack = 0,
                    neighbors_safe = 0
                   },
    {ok, next(SyncState, AppState)}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({ack, _Name}, {SyncState = #?STATE{name = MyName,
                                               expected_ack = Ack,
                                               neighbors = Neighbors}, AppState}) ->
    NewAck = case Ack - 1 of
                 0 -> broadcast_safe(Neighbors, MyName), 0;
                 N when N > 0 -> N;
                 _ -> exit(implementation_error)
             end,
    NewSyncState = SyncState#?STATE{expected_ack = NewAck},
    {noreply, {NewSyncState, AppState}};
handle_cast({safe, Name}, {SyncState = #?STATE{neighbors = Neighbors,
                                               neighbors_safe = NeighborsSafe}, AppState}) ->
    NewNeighborsSafe = [Name|NeighborsSafe],
    NewSyncState = SyncState#?STATE{neighbors_safe = NewNeighborsSafe},
    %% TODO: optimize
    case lists:all(fun(Neighbor) -> lists:member(Neighbor, NewNeighborsSafe) end, Neighbors) of
        true -> {noreply, next(NewSyncState, AppState)};
        false -> {noreply, {NewSyncState, AppState}}
    end;
handle_cast({msg, Msg, Name}, {SyncState = #?STATE{module = Mod,
                                                   name = MyName,
                                                   neighbors_safe = NeighborsSafe,
                                                   next_messages = NextMessages}, AppState}) ->
    ok = send_ack(Name, MyName),
    case lists:member(Name, NeighborsSafe) of
        true -> % next pulse
            NewSyncState = SyncState#?STATE{next_messages = [{Name, Msg}|NextMessages]},
            {noreply, {NewSyncState, AppState}};
        false -> % current pulse
            NewAppState = Mod:handle_message(Msg, Name, AppState),
            {noreply, {SyncState, NewAppState}}
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec broadcast_safe([neighbor()], name()) -> ok.
broadcast_safe(Neighbors, Name) ->
    _ = [gen_server:cast(Neighbor, {safe, Name}) || Neighbor <- Neighbors],
    ok.

-spec send_ack(neighbor(), name()) -> ok.
send_ack(Neighbor, Name) ->
    gen_server:cast(Neighbor, {ack, Name}).

-spec next(state(), app_state()) -> {state(), app_state()}.
next(SyncState = #?STATE{module = Mod,
                         clock = Clock,
                         neighbors = Neighbors,
                         neighbors_safe = NeighborsSafe,
                         next_messages = NextMessages}, AppState) ->
    %% next pulse
    {ok, MsgDefs, AppState2} = Mod:handle_pulse(Clock + 1, AppState),
    %% receive messages
    NewAppState = lists:fold(fun({Target, Msg}, AccState) ->
                                     Mod:handle_message(Msg, Target, AccState)
                             end, AppState2, NextMessages),
    %% send messages to neighbors
    _ = maps:map(fun(Target, Msg) ->
                         gen_server:cast(Target, {msg, Msg})
                 end, MsgDefs),
    %% reset expected_ack
    ExpectedAck = maps:size(MsgDefs),
    %% reset neighbors_safe
    NewNeighborsSafe = lists:subtract(NeighborsSafe, Neighbors),

    NewSyncState = SyncState#?STATE{clock = Clock + 1,
                                    expected_ack = ExpectedAck,
                                    neighbors_safe = NewNeighborsSafe,
                                    next_messages = []},
    {NewSyncState, NewAppState}.
