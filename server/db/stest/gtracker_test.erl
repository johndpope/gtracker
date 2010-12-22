-module(gtracker_test).

-include_lib("eunit/include/eunit.hrl").

-include("common_recs.hrl").

start_test() ->
   application:start(gtracker_db).

device_get_devices_test() ->
  ?assertEqual([], gtracker_pub:get_devices()).

device_register_device_test() ->
   F = fun(Owner) ->
      Device = gtracker_pub:register(),
      ?assertEqual(true, Device#device.online),
      ?assertEqual(self(), Device#device.owner),
      ?assertEqual(Device, gtracker_pub:register(Device#device.name)),
      Device1 = gtracker_pub:unregister(Device#device.name),
      ?assertEqual(false, Device1#device.online),
      ?assertEqual(undef, Device1#device.owner),
      ?assertEqual(no_such_device, gtracker_pub:register("QWERTYASDFQW")),
      Owner ! {Device, done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         timer:sleep(1000)
   end.

device_get_devices2_test() ->
   F = fun(Owner) ->
      Devices = gtracker_pub:get_devices(),
      ?assertEqual(true, is_list(Devices)),
      ?assertEqual(1, length(Devices)),
      ?assertEqual(true, is_record(hd(Devices), device)),
      Owner ! {[], done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         timer:sleep(1000)
   end.

get_device_test() ->
   F = fun(Owner) ->
      [Device = #device{name = Name, online = O}] = gtracker_pub:get_devices(),
      Device1 = gtracker_pub:get_device(Name),
      ?assertEqual(Device, Device1),
      ?assertEqual(false, O),
      ?assertMatch(#device{online = true}, gtracker_pub:register(Name)),
      ?assertMatch(#device{online = false}, gtracker_pub:unregister(Name)),
      Owner ! {[], done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         timer:sleep(1000)
   end.

update_device_test() ->
   F = fun(Owner) ->
      [#device{name = Name}] = gtracker_pub:get_devices(),
      Device = gtracker_pub:get_device(Name),
      gtracker_pub:update(Device#device{alias = "TEST_ALIAS"}),
      Owner ! {Device, done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         timer:sleep(1000)
   end.

device_user_test() ->
   F = fun(Owner) ->
      User = gtracker_pub:new_user("dmitryme@gmail.com", "123"),
      ?assertMatch(#user{name = "dmitryme@gmail.com", online = false, map_type = 0, is_admin = false, devices = []}, User),
      ?assertEqual(already_exists, gtracker_pub:new_user("dmitryme@gmail.com", "321")),
      User1 = gtracker_pub:update(User#user{is_admin = true, map_type = 1, devices = ["DMITRYME_!@#"]}),
      ?assertMatch(#user{name = "dmitryme@gmail.com", map_type = 1, is_admin = true, devices = ["DMITRYME_!@#"]}, User1),
      ?assertEqual(User1, gtracker_pub:get_user("dmitryme@gmail.com")),
      ?assertEqual(not_found, gtracker_pub:get_user("dmitryme")),
      ?assertMatch(#user{name = "dmitryme@gmail.com", online = true}, gtracker_pub:authenticate("dmitryme@gmail.com", "123")),
      ?assertEqual(rejected, gtracker_pub:authenticate("dmitryme", "123")),
      ?assertEqual(rejected, gtracker_pub:authenticate("dmitryme@gmail.com", "1234")),
      Owner ! {[], done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         timer:sleep(1000)
   end.

news_test() ->
   F = fun(Owner) ->
      EmptyNews = gtracker_pub:get_news(),
      ?assertEqual([], EmptyNews),
      ?assertEqual(invalid_date, gtracker_pub:insert_news({10,10,2010}, "BLA-BLA-BLA")),
      ?assertEqual([], gtracker_pub:get_news()),
      Ref = gtracker_pub:insert_news({2010,10,10}, "BLA-BLA-BLA"),
      ?assertEqual(true, erlang:is_reference(Ref)),
      ?assertEqual(1, length(gtracker_pub:get_news())),
      ?assertEqual(0, length(gtracker_pub:get_news({2010,10,9}))),
      ?assertEqual(1, length(gtracker_pub:get_news({2010,10,10}))),
      ?assertEqual(1, length(gtracker_pub:get_news({2010,10,11}))),
      Owner ! {[], done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         timer:sleep(1000)
   end.

new_track_test() ->
   F = fun(Owner) ->
      Device = gtracker_pub:register(),
      Track = gtracker_pub:new_track(Device, true),
      ?assertEqual(is_record(Track, track), true),
      gtracker_track_pub:close(Track),
      Owner ! {Device, done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         timer:sleep(1000)
   end.

subscribe_test() ->
   F = fun(Owner) ->
      Device = gtracker_pub:register(),
      SubDevice = gtracker_pub:subscribe(Device),
      ?assertEqual(SubDevice#device.subs, [self()]),
      UbDevice = gtracker_pub:unsubscribe(SubDevice),
      ?assertEqual(UbDevice#device.subs, []),
      Owner ! {Device, done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         timer:sleep(1000)
   end.

stop_test() ->
   application:stop(gtracker_db).
