%% mysql entities
-define(LINK, mysql_link).
-define(TIMEOUT, 5000).

%% query helpers
-define(RESULT(Var), {data, {mysql_result, _, Var, _, _}}).
-define(ERROR(Var), {error, {mysql_result, _, _, _, Var}}).

-define(AUTHENTICATE, "SELECT id,password FROM user WHERE name='~s';").
-define(USER_EXISTS, "SELECT id FROM user WHERE name='~s';").
-define(CREATE_USER, "INSERT INTO user (name, password) VALUES('~s', '~s');").

-define(DEVICE_INFO, "SELECT color,weight,name,alias FROM device WHERE id='~w';").

-define(SHOW_ALL_NEWS, "SELECT id,date,post FROM news;").
-define(SHOW_NEWS, "SELECT date,post FROM news ORDER BY id DESC LIMIT 10;").
-define(CREATE_NEWS, "INSERT INTO news (date,post) VALUES('~s', '~s');").
-define(REMOVE_NEWS, "DELETE FROM news WHERE id='~w';").
