-module(nmea_utils).

-export([calc_speed/2, calc_distance/2]).

-define(Earth_radius, 6372795).

%=======================================================================================================================
%  public exports
%=======================================================================================================================

calc_speed({Lat1, Lon1, Timestamp1}, {Lat2, Lon2, Timestamp2}) ->
   Distance = calc_distance({Lat1, Lon1}, {Lat2, Lon2}),
   TimeDiff = calendar:datetime_to_gregorian_seconds(Timestamp2) -
         calendar:datetime_to_gregorian_seconds(Timestamp1), % in seconds
   case TimeDiff of
      0 -> % if 0, then decides TimeDiff = 1
         Speed = Distance * 3.6,
         erlang:trunc(Speed * 100) / 100;
      _ ->
         Speed = Distance / TimeDiff * 3.6,
         erlang:trunc(Speed * 100) / 100
   end.

calc_distance({Lat1, Lon1}, {Lat2, Lon2}) ->
   Pnt1 = {Lat1, Lon1},
   Pnt2 = {Lat2, Lon2},
   RadPnt1 = to_radians(Pnt1),
   RadPnt2 = to_radians(Pnt2),
   dist(to_cossin(RadPnt1, RadPnt2)). % in meters

%=======================================================================================================================
%  pivate
%=======================================================================================================================
lat({Lat, _}) ->
   Lat.

lon({_, Lon}) ->
   Lon.

to_radians(Pnt) ->
   {lat(Pnt)*math:pi()/180, lon(Pnt)*math:pi()/180}.

to_cossin(RadPnt1, RadPnt2) ->
   Delta = lon(RadPnt2) - lon(RadPnt1),
   {{math:cos(lat(RadPnt1)), math:cos(lat(RadPnt2))},
    {math:sin(lat(RadPnt1)), math:sin(lat(RadPnt2))},
    {math:cos(Delta), math:sin(Delta)}}.

dist({{CosLat1, CosLat2}, {SinLat1, SinLat2}, {CosDelta, SinDelta}}) ->
   P1 = math:pow(CosLat2 * SinDelta, 2),
   P2 = math:pow(CosLat1 * SinLat2 - SinLat1 * CosLat2 * CosDelta, 2),
   P3 = math:sqrt(P1 + P2),
   P4 = SinLat1 * SinLat2,
   P5 = CosLat1 * CosLat2 * CosDelta,
   P6 = P4 + P5,
   P7 = P3 / P6,
   ?Earth_radius * math:atan(P7).

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

calc_speed_test() ->
   Coord1 = {55.726102, 37.615437, {{2010, 6, 1}, {12, 10, 10}}},
   Coord2 = {55.726398, 37.61531,  {{2010, 6, 1}, {12, 10, 13}}},
   ?assertEqual(40.64, calc_speed(Coord1, Coord2)).
-endif.
