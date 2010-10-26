%% mysql entities
-define(LINK, mysql_link).
-define(TIMEOUT, 5000).

%% query helpers
-define(RESULT(Var), {data, {mysql_result, _, Var, _, _}}).
-define(ERROR(Var), {error, {mysql_result, _, _, _, Var}}).

-define(USER_AUTHENTICATE, "SELECT id,password FROM user WHERE name='~s';"). %% TODO: fix me, make separate query for map_id (settings)
-define(USER_EXISTS, "SELECT id FROM user WHERE name='~s';").
-define(USER_CREATE, "INSERT INTO user (name,password) VALUES('~s', '~s');").
-define(USER_ADD_DEVICE, "INSERT INTO user2dev (user_id,device_id) VALUES('~w','~w');").
-define(USER_REMOVE_DEVICE, "DELETE FROM user2dev WHERE user_id='~w' AND device_id='~w';").
-define(USER_LOAD_SETTINGS, "SELECT admin,map_id FROM user WHERE id='~w';").
-define(USER_SAVE_SETTINGS, "UPDATE user SET map_id='~s' WHERE id='~w';").

-define(DEVICE_EXISTS, "SELECT id FROM device WHERE name='~s';").
-define(DEVICE_LAYER_SETTIGS, "SELECT color,weight,name,alias FROM device WHERE id='~w';").
-define(DEVICE_SETTINGS, "SELECT color,weight,name,alias,twitter_auth FROM device WHERE id='~w';").
-define(DEVICE_LIST, "SELECT device_id FROM user2dev WHERE user_id='~w';").
-define(DEVICE_NAME_LIST, "SELECT device_id,name FROM user2dev,device WHERE user_id='~w' AND device.id=user2dev.device_id;").
-define(DEVICE_CONFIG_LIST,
   "SELECT device_id,name,alias,color,weight FROM user2dev,device WHERE device.id=user2dev.device_id AND user2dev.user_id='~w';").
-define(DEVICE_UPDATE_INFO, "UPDATE device SET color='~s',weight='~s',alias='~s' WHERE id='~w';").

-define(SHOW_ALL_NEWS, "SELECT id,date,post FROM news;").
-define(SHOW_NEWS, "SELECT date,post FROM news ORDER BY id DESC LIMIT 10;").
-define(CREATE_NEWS, "INSERT INTO news (date,post) VALUES('~s', '~s');").
-define(REMOVE_NEWS, "DELETE FROM news WHERE id='~w';").
