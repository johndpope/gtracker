-module(gtracker_test).

-include_lib("eunit/include/eunit.hrl").

-include("common_recs.hrl").

start_test() ->
   application:start(gtracker_db).

device_get_devices_test() ->
   ?assertEqual({ok, []}, gtracker_pub:get_devices()).

device_register_device_test() ->
  F = fun(Owner) ->
        {ok, Device} = gtracker_pub:register(),
        ?assertEqual(online, Device#device.status),
        ?assertEqual(self(), Device#device.owner),
        ?assertEqual({ok, Device}, gtracker_pub:register(Device#device.name)),
        {ok, Device1} = gtracker_pub:unregister(Device#device.name),
        ?assertEqual(offline, Device1#device.status),
        ?assertEqual(undef, Device1#device.owner),
        ?assertEqual({error, no_such_device, ["QWERTYASDFQW"]}, gtracker_pub:register("QWERTYASDFQW")),
        Owner ! {Device, done}
  end,
  Owner = self(),
  spawn(fun() -> F(Owner) end),
  receive
     {_, done} ->
        ok
  end.

device_get_devices2_test() ->
   F = fun(Owner) ->
         {ok, Devices} = gtracker_pub:get_devices(),
         ?assertEqual(true, is_list(Devices)),
         ?assertEqual(1, length(Devices)),
         ?assertEqual(true, is_record(hd(Devices), device)),
         Owner ! {[], done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         ok
   end.

get_device_test() ->
   F = fun(Owner) ->
         {ok, [Device = #device{name = Name, status = O}]} = gtracker_pub:get_devices(),
         {ok, Device1} = gtracker_pub:get_device(Name),
         ?assertEqual(Device, Device1),
         ?assertEqual(offline, O),
         ?assertMatch({ok, #device{status = online}}, gtracker_pub:register(Name)),
         ?assertMatch({ok, #device{status = offline}}, gtracker_pub:unregister(Name)),
         Owner ! {[], done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         ok
   end.

update_device_test() ->
   F = fun(Owner) ->
         {ok, [#device{name = Name}]} = gtracker_pub:get_devices(),
         {ok, Device} = gtracker_pub:get_device(Name),
         gtracker_pub:update(Device#device{alias = "TEST_ALIAS"}, [alias]),
         Owner ! {Device, done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         ok
   end.

device_user_test() ->
   F = fun(Owner) ->
         {ok, User} = gtracker_pub:new_user("dmitryme@gmail.com", "123"),
         ?assertMatch(#user{name = "dmitryme@gmail.com", online = false, map_type = 0, is_admin = false, devices = []}, User),
         ?assertEqual({error, already_exists, ["dmitryme@gmail.com"]}, gtracker_pub:new_user("dmitryme@gmail.com", "321")),
         {ok, User1} = gtracker_pub:update(User#user{is_admin = true, map_type = 1, devices = ["DMITRYME_!@#"]}, [is_admin,
            map_type, devices]),
         ?assertMatch(#user{name = "dmitryme@gmail.com", map_type = 1, is_admin = true, devices = ["DMITRYME_!@#"]}, User1),
         ?assertEqual({ok, User1}, gtracker_pub:get_user("dmitryme@gmail.com")),
         ?assertEqual({error, not_found, ["dmitryme"]}, gtracker_pub:get_user("dmitryme")),
         ?assertMatch({ok, #user{name = "dmitryme@gmail.com", online = true}}, gtracker_pub:authenticate("dmitryme@gmail.com", "123")),
         ?assertEqual({error, rejected, ["dmitryme", "123"]}, gtracker_pub:authenticate("dmitryme", "123")),
         ?assertEqual({error, rejected, ["dmitryme@gmail.com", "1234"]}, gtracker_pub:authenticate("dmitryme@gmail.com", "1234")),
         Owner ! {[], done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         ok
   end.

news_test() ->
   F = fun(Owner) ->
         {ok, EmptyNews} = gtracker_pub:get_news(),
         ?assertEqual([], EmptyNews),
         ?assertEqual({error, invalid_date, [{10,10,2010}]}, gtracker_pub:insert_news({10,10,2010}, "BLA-BLA-BLA")),
         ?assertEqual({ok, []}, gtracker_pub:get_news()),
         {ok, Ref} = gtracker_pub:insert_news({2010,10,10}, "BLA-BLA-BLA"),
         ?assertEqual(true, erlang:is_reference(Ref)),
         ?assertEqual(1, length(element(2, gtracker_pub:get_news()))),
         ?assertEqual(0, length(element(2, gtracker_pub:get_news({2010,10,9})))),
         ?assertEqual(1, length(element(2, gtracker_pub:get_news({2010,10,10})))),
         ?assertEqual(1, length(element(2, gtracker_pub:get_news({2010,10,11})))),
         Owner ! {[], done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         ok
   end.

subscribe_test() ->
   F = fun(Owner) ->
         {ok, Device} = gtracker_pub:register(),
         {ok, SubDevice} = gtracker_pub:subscribe(Device#device.name),
         ?assertEqual(SubDevice#device.subs, [self()]),
         {ok, UbDevice} = gtracker_pub:unsubscribe(SubDevice#device.name),
         ?assertEqual(UbDevice#device.subs, []),
         Owner ! {Device, done}
   end,
   Owner = self(),
   spawn(fun() -> F(Owner) end),
   receive
      {_, done} ->
         ok
   end.

stop_test() ->
   application:stop(gtracker_db).
