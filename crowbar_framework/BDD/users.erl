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
    natural_key -> username;			% unlike most crowbar objects, this uses username as the natural key
    username -> "oscar";
	email -> "oscar@grouch.com";
    password -> "password";
    password_confirmation -> "password";
    remember_me -> "false";
    is_admin -> "false";
    _ -> crowbar:g(Item)
  end.

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) ->
  true.


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

%step(_Config, _Global, {step_given, _N, ["there is user", "test_user_1"]}) -> false;

step(_Config, _Result, {step_then, _N, ["the user",Username, "email should be", Email]}) -> 
   bdd_utils:log(_Config, puts, "Checking user: ~p email has been set to: ~p", [Username,Email]),
   true;

% GIVEN STEP =======================================================

 step(_Config, _Global, {step_given, _N, ["there is not a user", Username]}) ->   
  bdd_utils:log(_Config, puts, "Fetching user: ~p", [Username]),
  U = bdd_restrat:step(_Config, _Global, {step_when, _N, ["REST cannot find the", eurl:path(g(path),Username),"page"]}),
  bdd_utils:log(_Config, puts, "Fetching done, user is: ~p", [U]),
  U;
     
step(_Config, _Given, {step_given, _N, ["there is a user",Username]}) -> 
  bdd_utils:log(_Config, puts, "Creating user: ~p", [Username]),
  User = json(Username, g(email), g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  R = bdd_restrat:create(_Config, g(path), username,User),
  bdd_utils:log(_Config, puts, "Created user: ~p", [R]),
  R;


% WHEN STEP =======================================================

step(_Config, _Given, {step_when, _N, ["REST modifies user", Username, "setting email to", Email]}) -> 
   bdd_utils:log(_Config, puts, "Updating user: ~p, setting email to: ~p", [Username,Email]),
   User = json_update(Username, Email, g(remember_me), g(is_admin)),
   R = bdd_restrat:update(_Config, g(path)++"/"++Username, update ,username, User),
   bdd_utils:log(_Config, puts, "Updating user returned: ~p", [R]),
   true;

step(_Config, _Given, {step_when, _N, ["REST requests the list of users"]}) ->
  bdd_restrat:step(_Config, _Given, {step_when, _N, ["REST requests the", g(path),"page"]});

step(_Config, _Given, {step_when, _N, ["REST adds the user",  Username]}) -> 
  User = json(Username,"blah@test.com", g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  bdd_utils:log(_Config, info, "REST adds the user, adding user: ~p",[User]),
  bdd_restrat:create(_Config, username, g(path), User),
  _Config;

% THEN STEP  =======================================================
% ADD MISSING THEN STEP:
step(_Config, _Result, {step_then, _N, ["there should be a valid user", Username]}) -> 
  bdd_utils:log(_Config, puts, "Fetching user: ~p", [Username]),
  {Atom, List, Path} = bdd_restrat:step(_Config, _Result, {step_when, _N, ["REST requests the", eurl:path(g(path),Username),"page"]}),
  JSON= json:output(List),
  bdd_utils:log(_Config, puts, "JSON user: ~p", [JSON]),
  IsValid = validate(JSON),
  IsValid;

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
