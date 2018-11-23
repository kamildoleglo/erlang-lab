defmodule PollutionData do
  @moduledoc false
  def importLinesFromCSV do
    File.read!("pollution.csv") |> String.split("\r\n")
  end

  def parse_line(line) do
    [date, time, lat, long, val] = line |> String.split(",")
    date = date |> String.split("-") |> Enum.reverse |> Enum.join("-") |> Date.from_iso8601! |> Date.to_erl
    time = time |> String.split(":")
    time = time ++ ["00"] |> Enum.map(fn(x) -> String.to_integer(x) end)
    datetime = {date, :erlang.list_to_tuple(time)}
    {lat, _} = Float.parse(lat); {long, _} = Float.parse(long)
    val = String.to_integer(val)
    %{:datetime => datetime, :location => {lat, long}, :pollutionLevel => val}
  end

  def identifyStations(structs) do
    stations = Enum.reduce(structs, %{}, fn x, acc -> Map.put(acc, x[:location], nil) end)
  end
end

#Float.to_string(elem(x[:location], 0)) <> "_" <> Float.to_string(elem(x[:location], 1))