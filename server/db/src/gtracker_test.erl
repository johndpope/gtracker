-module(gtracker_test).

-include_lib("eunit/include/eunit.hrl").

-include("common_recs.hrl").

start_test() ->
   application:start(gtracker_db).

device_register_test() ->
   Device = gtracker_db_pub:register(),
   ?assertEqual(true, Device#device.online),
   ?assertEqual(self(), Device#device.links#links.owner),
   ?assertEqual(Device, gtracker_db_pub:register(Device#device.name)),
   Device1 = gtracker_db_pub:unregister(Device#device.name),
   ?assertEqual(false, Device1#device.online),
   ?assertEqual(undef, Device1#device.links#links.owner),
   ?assertEqual(no_such_device, gtracker_db_pub:register("QWERTYASDFQW")).

stop_test() ->
   application:stop(gtracker_db).
