%%%-------------------------------------------------------------------
%%% @author kamil
%%% @copyright (C) 2018, Kamil Doległo
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2018 14:13
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("kamil").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-record(monitor, {stations = #{}, locationsNames = #{}}).
-record(station, {name = "", coordinates = {}, measurement_types = #{}}).
-record(measurement_type, {measurements = #{}}).

% #monitor {name => #station}
% #station {type => #measurement_type}
% #measurement_type {date => value}

create_monitor_test() ->
  {monitor, #{}, #{}} = pollution:createMonitor().

add_station_test() ->
  Monitor = pollution:createMonitor(),
  StationA = #station{name = "A", coordinates = {0, 1}},
  StationB = #station{name = "B", coordinates = {2, 3}},
  MonitorA = pollution:addStation("A", {0, 1}, Monitor),

  ?assertEqual(maps:get("A", MonitorA#monitor.stations), StationA),

  MonitorB = pollution:addStation("B", {2, 3}, MonitorA),

  ?assertEqual(
    #monitor{
      locationsNames = #{{0, 1} => "A", {2, 3} => "B"},
      stations = #{
        "A" => StationA,
        "B" => StationB
      }},
    MonitorB),

  ?assertMatch({error, "Station already exists"}, pollution:addStation("A", {0, 1}, MonitorB)).

add_value_test() ->
  Time = calendar:local_time(),
  Monitor = pollution:createMonitor(),
  MonitorA = pollution:addStation("A", {0, 1}, Monitor),
  MonitorB = pollution:addValue({0, 1}, Time, "PM10", 100, MonitorA),

  ?assertEqual(
    #monitor{
      locationsNames = #{{0, 1} => "A"},
      stations = #{
        "A" => #station{
          name = "A",
          coordinates = {0, 1},
          measurement_types = #{
            "PM10" => #measurement_type{
              measurements = #{
                Time => 100
              }
            }
          }
        }
      }
    }, MonitorB),

  ?assertMatch({error, "Cannot add measurement"}, pollution:addValue({0, 1}, Time, "PM10", 72, MonitorB)),

  ?assertMatch({error, "Cannot find station"},
    pollution:addValue({-1000, 0}, calendar:local_time(), "Temperature", 21.5, MonitorB)).



remove_value_test() ->
  Time = calendar:local_time(),
  Monitor = pollution:createMonitor(),
  MonitorA = pollution:addStation("A", {0, 1}, Monitor),
  MonitorB = pollution:addValue({0, 1}, Time, "PM10", 100, MonitorA),

  ?assertEqual(
    #monitor{
      locationsNames = #{{0, 1} => "A"},
      stations = #{
        "A" => #station{
          name = "A",
          coordinates = {0, 1},
          measurement_types = #{
            "PM10" => #measurement_type{
              measurements = #{
                Time => 100
              }
            }
          }
        }
      }
    }, MonitorB),

  MonitorC = pollution:removeValue("A", Time, "PM10", MonitorB),

  ?assertEqual(
    #monitor{
      locationsNames = #{{0, 1} => "A"},
      stations = #{
        "A" => #station{
          name = "A",
          coordinates = {0, 1},
          measurement_types = #{
            "PM10" => #measurement_type{
              measurements = #{}
            }
          }
        }
      }
    }, MonitorC).

get_one_value_test() ->
  Time = calendar:local_time(),
  Monitor = pollution:createMonitor(),
  MonitorA = pollution:addStation("A", {0, 1}, Monitor),
  MonitorB = pollution:addValue({0, 1}, Time, "PM10", 100, MonitorA),

  ?assertEqual(100, pollution:getOneValue("PM10", Time, "A", MonitorB)),

  ?assertMatch(
    {error, "Cannot find measurement type"},
    pollution:getOneValue("PM25", calendar:local_time(), "A", MonitorB)),

  ?assertMatch({error, "Cannot find measurement"},
    pollution:getOneValue("PM10", calendar:universal_time(), "A", MonitorB)).

get_station_mean_test() ->
  Monitor = pollution:createMonitor(),
  MonitorA = pollution:addStation("A", {0, 1}, Monitor),
  {Date, {Hour, Minute, Second}} = calendar:local_time(),

  MonitorB = pollution:addValue({0, 1}, {Date, {Hour, Minute, Second}}, "PM10", 100, MonitorA),
  MonitorC = pollution:addValue({0, 1}, {Date, {(Hour + 1) rem 24, Minute, Second}}, "PM10", 50, MonitorB),
  MonitorD = pollution:addValue({0, 1}, {Date, {(Hour + 2) rem 24, Minute, Second}}, "PM2,5", 10, MonitorC),

  ?assertEqual(75.0, pollution:getStationMean("PM10", "A", MonitorC)),
  ?assertEqual(75.0, pollution:getStationMean("PM10", "A", MonitorD)).

get_daily_mean_test() ->
  Monitor = pollution:createMonitor(),
  MonitorA = pollution:addStation("A", {0, 1}, Monitor),
  MonitorB = pollution:addStation("B", {2, 3}, MonitorA),
  Time = {Date, {Hour, Minute, Second}} = calendar:local_time(),

  MonitorC = pollution:addValue("A", Time, "PM10", 80, MonitorB),
  MonitorD = pollution:addValue("A", {Date, {(Hour + 1) rem 24, Minute, Second}}, "PM10", 120, MonitorC),
  MonitorE = pollution:addValue("B", {Date, {(Hour + 2) rem 24, Minute, Second}}, "PM10", 40, MonitorD),

  ?assertEqual(80.0, pollution:getDailyMean("PM10", Date, MonitorE)).

get_air_quality_test() ->
  Monitor = pollution:createMonitor(),
  MonitorA = pollution:addStation("A", {0, 1}, Monitor),
  Time = {Date, {Hour, Minute, Second}} = calendar:local_time(),

  MonitorB = pollution:addValue("A", Time, "PM10", 80, MonitorA),
  MonitorC = pollution:addValue("A", {Date, {Hour, (Minute + 10) rem 60, Second}}, "PM10", 120, MonitorB),
  MonitorD = pollution:addValue("A", {Date, {Hour, Minute, Second}}, "PM25", 40, MonitorC),

  ?assertEqual(240.0, pollution:getAirQualityIndex("A", Time, MonitorD)).

