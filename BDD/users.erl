% Copyright 2013, Dell 
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
-export([step/2, json_update/4, json/6, validate/1, inspector/1, g/1]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "/api/v2/users";
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


% validates List of objects in a generic way common to all objects.
validate_list(List) ->
  bdd_utils:assert([is_list(List)], debug).

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

fetch_user(Config, Result, N, Username) ->
  {_Atom, List, _Path} = bdd_restrat:step(Config, Result, {step_when, N, ["REST requests the", eurl:path(g(path),Username),"page"]}),
  bdd_utils:log(Config, debug, "users:step Fetch User: ~p", [List]),
  List.


% GIVEN STEP =======================================================
% TEMPORARY REMAPPING
step(In, Out) -> step([], In, Out).

step(Config, _Global, {step_given, _N, ["there is not a user", Username]}) -> 
  bdd_utils:log(trace, "users:step there is not a user: ~p", [Username]),
  R = eurl:delete(Config,g(path),Username,all),
  bdd_utils:log(debug, users, step, "there is not a user: ~p, returning: ~p", [Username,R]),
  R;

step(_Config, _Global, {step_given, _N, ["there is a user", Username, "with email", Email]}) -> 
  bdd_utils:log(_Config, trace, "users:step there is a user: ~p", [Username]),
  User = json(Username, Email, g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  R = bdd_restrat:create(_Config, g(path),username,User),
  bdd_utils:log(_Config, debug, "users:step Created user: ~p", [User]),
  R;

step(_Config, _Global, {step_given, _N, ["there is a user", Username]}) -> 
  step(_Config, _Global, {step_given, _N, ["there is a user", Username, "with email", g(test_email)]});  

step(_Config, _Global, {step_given, _N, ["there is an admin user", Username]}) -> 
  bdd_utils:log(_Config, trace, "users:step there is an admin user: ~p", [Username]),
  User = json(Username, g(test_email), g(password), g(password_confirmation), g(remember_me), true),
  R = bdd_restrat:create(_Config, g(path),username,User),
  bdd_utils:log(_Config, debug, "users:step Created admin user: ~p", [User]),
  R;

% WHEN STEP =======================================================

step(_Config, _Given, {step_when, _N, ["REST requests the list of users"]}) ->
  bdd_restrat:step(_Config, _Given, {step_when, _N, ["REST requests the", g(path),"page"]});


step(_Config, _Given, {step_when, _N, ["REST adds the user",  Username]}) -> 
  User = json(Username,"blah@test.com", g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  bdd_utils:log(_Config, debug, "REST adds the user, adding user: ~p",[User]),
  bdd_restrat:create(_Config, g(path), username, User),
  _Config;

step(_Config, _Given, {step_when, _N, ["REST elevates user", Username, "to administrator"]}) -> 
   bdd_utils:log(_Config, debug, "Elevating user: ~p, to administrator", [Username]),
   R = json:parse(eurl:put_post(_Config,g(path)++"/"++Username++"/admin", [], post)),
   bdd_utils:log(_Config, trace, "Make user admin returned: ~p", [R]),
   R;

step(_Config, _Given, {step_when, _N, ["REST removes admin privilege for user", Username]}) -> 
   bdd_utils:log(_Config, debug, "Removing admin privilege for user: ~p", [Username]),
   R = eurl:delete(_Config, g(path)++"/"++Username++"/admin",[]),
   bdd_utils:log(_Config, trace, "Removed user admin returned: ~p", [R]),
   R;

step(_Config, _Given, {step_when, _N, ["REST modifies user", Username, "setting email to", Email]}) -> 
   bdd_utils:log(_Config, trace, "users:step Updating user: ~p, setting email to: ~p", [Username,Email]),
   User = json_update(Username, Email, g(remember_me), g(is_admin)),
   R = bdd_restrat:update(_Config, g(path)++"/"++Username, update ,username, User),
   bdd_utils:log(_Config, debug, "users:step Updating user returned: ~p", [R]),
   R;

step(_Config, _Given, {step_when, _N, ["REST modifies user", Username, "setting password and password_confirmation to", Password]}) -> 
   bdd_utils:log(_Config, trace, "users:step Updating user: ~p, resetting password to: ~p", [Username,Password]),
   User = json_reset_password(Username, Password),
   R = bdd_restrat:update(_Config, g(path)++"/"++Username++"/reset_password", update ,username, User),
   bdd_utils:log(_Config, debug, "users:step Updating user returned: ~p", [R]),
   R;

step(_Config, _Given, {step_when, _N, ["REST locks user", Username]}) -> 
   bdd_utils:log(_Config, debug, "Locking user: ~p", [Username]),
   R = json:parse(eurl:put_post(_Config, g(path)++"/"++Username++"/lock", [], post)),
   bdd_utils:log(_Config, trace, "Lock user returned: ~p", [R]),
   R;

step(_Config, _Given, {step_when, _N, ["REST unlocks user", Username]}) -> 
   bdd_utils:log(_Config, trace, "Unlocking user: ~p", [Username]),
   R = eurl:delete(_Config, g(path)++"/"++Username++"/lock",[]),
   bdd_utils:log(_Config, debug, "Unlock user returned: ~p", [R]),
   R;

% THEN STEP  =======================================================

% validate list based on basic rules for Crowbar
step(_Config, Result, {step_then, _N, ["the list of objects is properly formatted"]}) -> 
  {ajax, List, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  validate_list(List);

step(_Config, _Result, {step_then, _N, ["there should be a valid user", Username]}) -> 
  bdd_utils:log(_Config, trace, "users:step Fetching user: ~p", [Username]),
  User_JSON = fetch_user(_Config, _Result, _N, Username),
  IsValid = validate(User_JSON),
  IsValid;


step(_Config, _Result, {step_then, _N, ["the user",Username, "email should be", Email]}) -> 
   bdd_utils:log(_Config, trace, "users:step Checking user: ~p email has been set to: ~p", [Username,Email]),
   User_JSON = fetch_user(_Config, _Result, _N, Username),
   _Email = element(2,lists:keyfind("email", 1, User_JSON)), 
   bdd_utils:log(_Config, trace, "users:step Checking user _Email: ~p ", [_Email]), 
   bdd_utils:log(_Config, trace, "users:step Checking user (_Email == Email): ~p ", [(_Email == Email)]), 
   (_Email == Email);

step(Config, _Result, {step_then, _N, ["the user",Username, "is_admin should be", Is_Admin]}) -> 
   bdd_utils:log(Config, trace, "users:step Checking user: ~p is_admin has been set to: ~p", [Username,Is_Admin]),
   User_JSON = fetch_user(Config, _Result, _N, Username),
   _Is_Admin = element(2,lists:keyfind("is_admin", 1, User_JSON)), 
   bdd_utils:log(Config, trace, "users:step Checking user _Is_Admin: ~p ", [_Is_Admin]), 
   bdd_utils:log(Config, trace, "users:step Checking user (_Is_Admin == Is_Admin): ~p ", [(_Is_Admin == Is_Admin)]), 
   (_Is_Admin == Is_Admin);


% FINALLY STEP  =======================================================
% ADD MISSING FINALLY STEP:
step(Config, _Given, {step_finally, _N, ["REST removes the user", Username]}) -> 
  bdd_utils:log(Config, trace, "users:step REST removes the user: ~p", [Username]),
  bdd_restrat:destroy(Config, g(path), Username);

% Setup/Teardown

%setup, takes care of create               
step(Config, _Global, {step_setup, _N, _}) -> 
  User = json(g(username), g(email), g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  bdd_utils:log(Config, debug, "users:step Setup users tests, creating user: ~p",[User]),
  bdd_restrat:create(Config, g(path), setup_user, username, User),
  Config;

%teardown, takes care of delete test.
step(Config, _Global, {step_teardown, _N, _}) -> 
   bdd_utils:log(Config, debug, "users:step step-teardown deleting: ~p",[g(username)] ),
   bdd_restrat:destroy(Config, g(path), g(username)).
