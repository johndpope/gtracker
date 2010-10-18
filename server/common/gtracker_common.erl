-module(gtracker_common).

-include("fields.hrl").

-export([
      gen_dev_name/0
      ,ints_to_float/2
      ,unix_seconds_to_datetime/1
      ,datetime_to_unix_seconds/1
      ,datetime_to_string/1
      ,join_pg/2
      ,leave_pg/2
      ,send_pg/2
      ,list_to_urlencoded/1
      ,bin_to_urlencoded/1
      ,urlencoded_to_list/1
      ,urlencoded_to_bin/1
      ,binary_to_hex/1
      ,fill_binary/3
      ,get_best_process/2
   ]).

-define(SWAMP, "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").
-define(EpochSeconds, 62167219200). % seconds since 0/0/0 0:0:0 to unix epoch time 1/1/1970 0:0:0

%=======================================================================================================================
%  public exports
%=======================================================================================================================

gen_dev_name() ->
   gen_seq(?DEV_NAME_LEN).

ints_to_float(Mantissa, Exponent) when Mantissa > 0 ->
   Mantissa + Exponent/1000000;
ints_to_float(Mantissa, Exponent) ->
   -(erlang:abs(Mantissa) + Exponent / 1000000).

unix_seconds_to_datetime(Sec) ->
   calendar:gregorian_seconds_to_datetime(?EpochSeconds + Sec).

datetime_to_unix_seconds(DateTime) ->
   calendar:datetime_to_gregorian_seconds(DateTime) - ?EpochSeconds.

datetime_to_string({{YYYY, MM, DD},{HH, M, SS}}) ->
   lists:flatten(io_lib:format("~2.10.0B.~2.10.0B.~4.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
         [DD, MM, YYYY, HH, M, SS])).

join_pg(Name, Pid) ->
   pg2:create(Name),
   case pg2:join(Name, Pid) of
      {error, _Reason} = Err ->
         erlang:error(Err);
      _ ->
         ok
   end.

leave_pg(Name, Pid) ->
   case pg2:leave(Name, Pid) of
      {error, _Reason} = Err ->
         erlang:error(Err);
      _ ->
         ok
   end.

send_pg(Name, Msg) ->
   lists:map(fun(Pid) -> Pid ! {pg_message, self(), Name, Msg} end, pg2:get_members(Name)).

list_to_urlencoded([]) ->
    [];
list_to_urlencoded([H|T]) ->
   [A, B] = to_hex(H),
   [$%, A, B | list_to_urlencoded(T)].

bin_to_urlencoded(Bin) ->
    list_to_urlencoded(binary_to_list(Bin)).

urlencoded_to_bin(S) ->
    list_to_binary(urlencoded_to_list(S)).

urlencoded_to_list([$%,X,Y|T]) ->
    [int(X)*16 + int(Y) | urlencoded_to_list(T)];
urlencoded_to_list([]) ->
    [].

binary_to_hex(<<>>) ->
   [];
binary_to_hex(<<I:8, Rest/binary>>) ->
   [A, B] = to_hex(I),
   [A, B | binary_to_hex(Rest)].

fill_binary(Bin, Size, _Val) when size(Bin) >= Size ->
   <<Ret:Size/binary, _Rest/binary>> = Bin,
   Ret;
fill_binary(Bin, Size, Val) ->
   fill_binary_aux(Bin, Size - size(Bin), Val).

get_best_process(ProcGroup, Criteria) ->
   case pg2:get_members(ProcGroup) of
      {error, _} ->
         undef;
      Pids ->
         hd(lists:sort(fun({_Pid1, Size1}, {_Pid2, Size2}) -> Size1 < Size2 end,
               [ (fun(P) -> [{_, Size}] = process_info(P, [Criteria]), {P, Size} end)(Pid) || Pid <- Pids ]))
   end.

%=======================================================================================================================
%  pivate
%=======================================================================================================================
gen_seq(0) ->
   [];
gen_seq(Len) ->
   [lists:nth(crypto:rand_uniform(1, length(?SWAMP)), ?SWAMP) | gen_seq(Len - 1)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a+(N-10).

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.

to_hex(N) when N < 256 ->
   [hex(N div 16), hex(N rem 16)].

fill_binary_aux(Bin, 0, _Val) ->
   Bin;
fill_binary_aux(Bin, TailSize, Val) ->
   fill_binary_aux(<<Bin/binary, Val/binary>>, TailSize - 1, Val).

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

list_to_urlencoded_test() ->
   SampleList="Hello World",
   UrlEncodedSample = "%48%65%6c%6c%6f%20%57%6f%72%6c%64",
   ?assertEqual(UrlEncodedSample, list_to_urlencoded(SampleList)),
   ?assertEqual(SampleList, urlencoded_to_list(UrlEncodedSample)).

fill_binary_test() ->
   Bin = <<1,2,3,4,5,6>>,
   ?assertEqual(<<1,2,3>>, fill_binary(Bin, 3, <<0:8>>)),
   ?assertEqual(Bin, fill_binary(Bin, 6, <<0:8>>)),
   ?assertEqual(<<Bin/binary, 0,0,0,0>>, fill_binary(Bin, 10, <<0:8>>)).

datetime_to_string_test() ->
   ?assertEqual("13.07.2010 14:14:00", datetime_to_string({{2010, 7, 13},{14,14,00}})).
-endif.
