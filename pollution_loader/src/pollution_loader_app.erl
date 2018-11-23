%%%-------------------------------------------------------------------
%% @doc pollution_loader public API
%% @end
%%%-------------------------------------------------------------------

-module(pollution_loader_app).

-behaviour(application).
-include("data.hrl").


%% Application callbacks
-export([start/2, stop/1]).
-export([parse/0, parse/2, load/1, parse_and_load/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    pollution_server_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================


parse_and_load() ->
    load(parse()).


parse() ->
    parse("data.xsd", "data.xml").

parse(XsdName, XmlName) ->
    XsdFile = filename:join([codeDir(), XsdName]),
    {ok, Model} = erlsom:compile_xsd_file(XsdFile, [{strict, true}]),
    XmlFile = filename:join([codeDir(), XmlName]),
    {ok, PollutionData, _} = erlsom:scan_file(XmlFile, Model),
    PollutionData.

load(Data) ->
    Elements = Data#measurements.'element',
    Stations_list = [{X#element.location#location.latitude, X#element.location#location.longitude} || X <- Elements],
    Stations = sets:to_list(sets:from_list(Stations_list)),
    addStations(Stations),
    addValues(Elements).

addValues(Elements) ->
    lists:foreach(fun(Element) -> addValue(Element) end, Elements).

addValue(Element) ->
    Datetime = iso8601:parse(Element#element.time),
    Value = Element#element.data#data.no2#no2.value,
    Location = {Element#element.location#location.latitude, Element#element.location#location.longitude},
    Type = no2,
    pollution_gen_server:addValue(Location, Datetime, Type, Value).

addStations(Locations) ->
    lists:foreach(fun({Lat, Long}) ->
        pollution_gen_server:addStation(float_to_list(Lat) ++ "_" ++ float_to_list(Long),
            {Lat, Long}) end,
        Locations).


codeDir() -> filename:dirname(code:which(?MODULE)).
