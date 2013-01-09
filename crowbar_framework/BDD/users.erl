% Copyright 2012, Dell 
% 
% Licensed under the Apache License, Version 2.0 (the "License"); 
% you may not use this file except in compliance with the License. 
% You may obtain a copy of the License at 
% 
%  http://www.apache.org/licenses/LICENSE-2.0 
% 
% Unless required by applicable law or agreed to in writing, software 
% distributed under the License is distributed on an "AS IS" BASIS, 
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
% See the License for the specific language governing permissions and 
% limitations under the License. 
% 
% 
-module(users).
-export([step/3, json_update/4, json/6, validate/1, inspector/1, g/1]).

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "/2.0/crowbar/2.0/users";
    path_reset_pw -> "/2.0/crowbar/2.0/user_reset_password";
    path_lock   -> "/2.0/crowbar/2.0/user_lock"; 
    path_unlock -> "/2.0/crowbar/2.0/user_unlock"; 
    path_make_admin -> "/2.0/crowbar/2.0/user_make_admin"; 
    path_remove_admin -> "/2.0/crowbar/2.0/user_remove_admin"; 
    natural_key -> username; % unlike most crowbar objects, this uses username as the natural key
    username -> "oscar";
	email -> "oscar@grouch.com";
    test_email -> "test@test.com";
    password -> "password";
    password_confirmation -> "password";
    remember_me -> "false";
    is_admin -> "false";
    _ -> crowbar:g(Item)
  end.

% Common Routine
% Only need chck id, username and email are parseable for now
% Should really have email validator

validate(List) ->
  Is_Valid = true,
  try 
    _Id = element(2,lists:keyfind("id", 1, List)),
    _Username = element(2,lists:keyfind("username", 1, List)),
    _Email = element(2,lists:keyfind("email", 1, List)),
    bdd_utils:log(debug, "User validation: ID: ~p, Username: ~p, Email: ~p", [_Id, _Username,_Email]),
    Is_Valid
  catch
    X: Y -> io:format("ERROR: cannot parse user: ~p:~p", [X, Y]),
            io:format("Stacktrace: ~p", [erlang:get_stacktrace()]),
    false
  end.


% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  crowbar_rest:inspector(Config, users).  % shared inspector works here, but may not always
  
% Common Routine
% Creates JSON used for POST/PUT requests
json(Username, Email, Password, Password_Confirmation, Remember_Me, Is_Admin) ->
  json:output([{"username",Username},{"email", Email},{"password", Password}, {"password_confirmation", Password_Confirmation},{"remember_me", Remember_Me},{"is_admin", Is_Admin}]).

json_update(Username, Email, Remember_Me, Is_Admin) ->
  json:output([{"username",Username},{"email", Email},{"remember_me", Remember_Me},{"is_admin", Is_Admin}]).

json_reset_password(Username, Password) ->
  json:output([{"username",Username},{"password", Password},{"password_confirmation", Password}]).

json_lock_unlock(Username) ->
  json:output([{"username",Username}]).

fetch_user(Config, Result, N, Username) ->
  {_Atom, List, _Path} = bdd_restrat:step(Config, Result, {step_when, N, ["REST requests the", eurl:path(g(path),Username),"page"]}),
  bdd_utils:log(Config, puts, "Fetch User: ~p", [List]),
  List.


% GIVEN STEP =======================================================

 step(_Config, _Global, {step_given, _N, ["there is not a user", Username]}) ->   
  bdd_utils:log(_Config, puts, "there is not a user: ~p", [Username]),
  R = bdd_restrat:step(_Config, _Global, {step_when, _N, ["REST cannot find the", eurl:path(g(path),Username),"page"]}),
  bdd_utils:log(_Config, puts, "there is not a user: ~p, returning: ~p", [Username,R]),
  R;

step(_Config, _Global, {step_given, _N, ["there is a user", Username]}) -> 
  bdd_utils:log(_Config, puts, "there is a user: ~p", [Username]),
  User = json(Username, g(test_email), g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  R = bdd_restrat:create(_Config, g(path),username,User),
  bdd_utils:log(_Config, puts, "Created user: ~p", [User]),
  R;

step(_Config, _Global, {step_given, _N, ["there is an admin user", Username]}) -> 
  bdd_utils:log(_Config, puts, "there is an admin user: ~p", [Username]),
  User = json(Username, g(test_email), g(password), g(password_confirmation), g(remember_me), true),
  R = bdd_restrat:create(_Config, g(path),username,User),
  bdd_utils:log(_Config, puts, "Created admin user: ~p", [User]),
  R;

% WHEN STEP =======================================================

step(_Config, _Given, {step_when, _N, ["REST requests the list of users"]}) ->
  bdd_restrat:step(_Config, _Given, {step_when, _N, ["REST requests the", g(path),"page"]});


step(_Config, _Given, {step_when, _N, ["REST adds the user",  Username]}) -> 
  User = json(Username,"blah@test.com", g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  bdd_utils:log(_Config, info, "REST adds the user, adding user: ~p",[User]),
  bdd_restrat:create(_Config, g(path), username, User),
  _Config;

step(_Config, _Given, {step_when, _N, ["REST elevates user", Username, "to administrator"]}) -> 
   bdd_utils:log(_Config, puts, "Elevating user: ~p, to administrator", [Username]),
   User = json_update(Username, g(test_email), g(remember_me), true),
   R = bdd_restrat:update(_Config, g(path_make_admin)++"/"++Username, update ,username, User),
   bdd_utils:log(_Config, trace, "Updating user returned: ~p", [R]),
   R;

step(_Config, _Given, {step_when, _N, ["REST removes admin privilege for user", Username]}) -> 
   bdd_utils:log(_Config, puts, "Removing admin privilege for user: ~p", [Username]),
   User = json_update(Username, g(test_email), g(remember_me), false),
   R = bdd_restrat:update(_Config, g(path_remove_admin)++"/"++Username, update ,username, User),
   bdd_utils:log(_Config, trace, "Updating user returned: ~p", [R]),
   R;

step(_Config, _Given, {step_when, _N, ["REST modifies user", Username, "setting email to", Email]}) -> 
   bdd_utils:log(_Config, puts, "Updating user: ~p, setting email to: ~p", [Username,Email]),
   User = json_update(Username, Email, g(remember_me), g(is_admin)),
   R = bdd_restrat:update(_Config, g(path)++"/"++Username, update ,username, User),
   bdd_utils:log(_Config, trace, "Updating user returned: ~p", [R]),
   R;

step(_Config, _Given, {step_when, _N, ["REST modifies user", Username, "setting password and password_confirmation to", Password]}) -> 
   bdd_utils:log(_Config, puts, "Updating user: ~p, resetting password to: ~p", [Username,Password]),
   User = json_reset_password(Username, Password),
   R = bdd_restrat:update(_Config, g(path_reset_pw)++"/"++Username, update ,username, User),
   bdd_utils:log(_Config, puts, "Updating user returned: ~p", [R]),
   R;

step(_Config, _Given, {step_when, _N, ["REST locks user", Username]}) -> 
   bdd_utils:log(_Config, puts, "Locking user: ~p", [Username]),
   User = json_lock_unlock(Username),
   R = bdd_restrat:update(_Config, g(path_lock)++"/"++Username, update ,username, User),
   bdd_utils:log(_Config, trace, "Lock user returned: ~p", [R]),
   R;

step(_Config, _Given, {step_when, _N, ["REST unlocks user", Username]}) -> 
   bdd_utils:log(_Config, puts, "Unlocking user: ~p", [Username]),
   User = json_lock_unlock(Username),
   R = bdd_restrat:update(_Config, g(path_unlock)++"/"++Username, update ,username, User),
   bdd_utils:log(_Config, trace, "Lock user returned: ~p", [R]),
   R;

% THEN STEP  =======================================================
% ADD MISSING THEN STEP:
step(_Config, _Result, {step_then, _N, ["there should be a valid user", Username]}) -> 
  bdd_utils:log(_Config, puts, "Fetching user: ~p", [Username]),
  User_JSON = fetch_user(_Config, _Result, _N, Username),
  IsValid = validate(User_JSON),
  IsValid;


step(_Config, _Result, {step_then, _N, ["the user",Username, "email should be", Email]}) -> 
   bdd_utils:log(_Config, puts, "Checking user: ~p email has been set to: ~p", [Username,Email]),
   User_JSON = fetch_user(_Config, _Result, _N, Username),
   _Email = element(2,lists:keyfind("email", 1, User_JSON)), 
   bdd_utils:log(_Config, puts, "Checking user _Email: ~p ", [_Email]), 
   bdd_utils:log(_Config, puts, "Checking user (_Email == Email): ~p ", [(_Email == Email)]), 
   (_Email == Email);

step(_Config, _Result, {step_then, _N, ["the user",Username, "is_admin should be", Is_Admin]}) -> 
   bdd_utils:log(_Config, puts, "Checking user: ~p is_admin has been set to: ~p", [Username,Is_Admin]),
   User_JSON = fetch_user(_Config, _Result, _N, Username),
   _Is_Admin = element(2,lists:keyfind("is_admin", 1, User_JSON)), 
   bdd_utils:log(_Config, puts, "Checking user _Is_Admin: ~p ", [_Is_Admin]), 
   bdd_utils:log(_Config, puts, "Checking user (_Is_Admin == Is_Admin): ~p ", [(_Is_Admin == Is_Admin)]), 
   (_Is_Admin == Is_Admin);


% FINALLY STEP  =======================================================
% ADD MISSING FINALLY STEP:
step(_Config, _Given, {step_finally, _N, ["REST removes the user", Username]}) -> 
  bdd_utils:log(_Config, puts, "REST removes the user: ~p", [Username]),
  bdd_restrat:destroy(_Config, g(path), Username);

% Setup/Teardown

%setup, takes care of create               
step(Config, _Global, {step_setup, _N, _}) -> 
  User = json(g(username), g(email), g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  bdd_utils:log(Config, info, "Setup users tests, creating user: ~p",[User]),
  bdd_restrat:create(Config, g(path), setup_user, username, User),
  Config;

%teardown, takes care of delete test.
step(Config, _Global, {step_teardown, _N, _}) -> 
   bdd_utils:log(Config, info, "step-teardown deleting: ~p",[g(username)] ),
   bdd_restrat:destroy(Config, g(path), g(username)).
