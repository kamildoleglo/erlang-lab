%%%-------------------------------------------------------------------
%%% @author kamil
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2018 13:58
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("kamil").

-behaviour(gen_server).

%% API
-export([start_link/0, getAirQualityIndex/2, getDailyMean/2, getStationMean/2, getOneValue/3, findStation/1, removeValue/3, addValue/4, addStation/2, crash/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
addStation(Name, {X, Y}) ->
  gen_server:cast(?MODULE, {addStation, Name, {X, Y}}).

addValue(Name, Date, Type, Value) ->
  gen_server:cast(?MODULE, {addValue, Name, Date, Type, Value}).

removeValue(Name, Date, Type) ->
  gen_server:cast(?MODULE, {removeValue, Name, Date, Type}).

findStation(Name) ->
  gen_server:call(?MODULE, {findStation, Name}).

getOneValue(Type, Date, StationID) ->
  gen_server:call(?MODULE, {getOneValue, Type, Date, StationID}).

getStationMean(Type, StationID) ->
  gen_server:call(?MODULE, {getStationMean, Type, StationID}).

getDailyMean(Type, Day) ->
  gen_server:call(?MODULE, {getDailyMean, Type, Day}).

getAirQualityIndex(StationID, Date) ->
  gen_server:call(?MODULE, {getAirQualityIndex, StationID, Date}).

crash() ->
  1/0.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

handle_cast({addStation, Name, {X, Y}}, LoopData) ->
  Response = pollution:addStation(Name, {X, Y}, LoopData),
  parse_async_response(Response, LoopData);

handle_cast({addValue, Name, Date, Type, Value}, LoopData) ->
  Response = pollution:addValue(Name, Date, Type, Value, LoopData),
  parse_async_response(Response, LoopData);

handle_cast({removeValue, Name, Date, Type}, LoopData) ->
  Response = pollution:removeValue(Name, Date, Type, LoopData),
  parse_async_response(Response, LoopData).

handle_call({findStation, Name}, _, LoopData) ->
  Response = pollution:findStation(Name, LoopData),
  parse_sync_response(Response, LoopData);

handle_call({getOneValue, Type, Date, StationID}, _, LoopData) ->
  Response = pollution:getOneValue(Type, Date, StationID, LoopData),
  parse_sync_response(Response, LoopData);

handle_call({getStationMean, Type, StationID}, _, LoopData) ->
  Response = pollution:getStationMean(Type, StationID, LoopData),
  parse_sync_response(Response, LoopData);

handle_call({getDailyMean, Type, Day}, _, LoopData) ->
  Response = pollution:getDailyMean(Type, Day, LoopData),
  parse_sync_response(Response, LoopData);

handle_call({getAirQualityIndex, StationID, Date}, _, LoopData) ->
  Response = pollution:getAirQualityIndex(StationID, Date, LoopData),
  parse_sync_response(Response, LoopData).



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([]) ->
  {monitor, Monitor} = pollution:createMonitor(),
  {ok, Monitor}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


parse_async_response({monitor, NewMonitor}, _) ->
  {noreply, NewMonitor};

parse_async_response({error, _}, LoopData) ->
  {noreply, LoopData}.

parse_sync_response({ok, Value}, LoopData) ->
  {reply, Value, LoopData};

parse_sync_response({error, Error}, LoopData) ->
  {reply, Error, LoopData}.
