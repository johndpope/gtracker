%% mysql entities
-define(LINK, mysql_link).
-define(TIMEOUT, 5000).

%% query helpers
-define(RESULT(Var), {data, {mysql_result, _, Var, _, _}}).
-define(ERROR(Var), {error, {mysql_result, _, _, _, Var}}).

-define(AUTHENTICATE, "SELECT id,password FROM user WHERE name='~s';").
-define(USER_EXISTS, "SELECT id FROM user WHERE name='~s';").
-define(CREATE_USER, "INSERT INTO user (name, password) VALUES('~s', '~s');").
