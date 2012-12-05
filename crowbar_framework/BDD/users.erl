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
-export([step/3, json/6, validate/1, inspector/1, g/1]).

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "/2.0/crowbar/2.0/users";
    natural_key -> username;			% unlike most crowbar objects, this uses username as the natural key
    username -> "oscar";
	email -> "Oscar@grouch.com";
    password -> "password";
    password_confirmation -> "password";
    remember_me -> "false";
    is_admin -> "false";
    _ -> crowbar:g(Item)
  end.

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) ->
  try
    _Description = json:keyfind(JSON, description), % ADD CHECK!,
    R =[bdd_utils:is_a(number, json:keyfind(JSON, order)),
        bdd_utils:is_a(integer, json:keyfind(JSON, fingerprint)), 
        bdd_utils:is_a(boolean, json:keyfind(JSON, allocated)), 
        bdd_utils:is_a(string, json:keyfind(JSON, state)), 
        bdd_utils:is_a(boolean, json:keyfind(JSON,admin)), 
        bdd_utils:is_a(dbid, json:keyfind(JSON,os_id)), 
        crowbar_rest:validate(JSON)],
    bdd_utils:assert(R) 
  catch
    X: Y -> io:format("ERROR: parse error ~p:~p~n", [X, Y]),
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

% GIVEN STEP
%dp start
% List users

step(_Config, _Given, {step_when, _N, ["REST requests the list of users"]}) ->
  bdd_restrat:step(_Config, _Given, {step_when, _N, ["REST requests the", g(path),"page"]});

%dp end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
     
step(_Config, _Given, {step_given, _N, ["there is a user",Username]}) -> 
  bdd_utils:log(_Config, puts, "Fetching user: ~p", [Username]),
  bdd_restrat:step(_Config, _Given, {step_when, _N, ["REST requests the", eurl:path(g(path),Username),"page"]});

% WHEN STEP =======================================================

step(_Config, _Given, {step_when, _N, ["REST gets the user list"]}) -> false;


% Common Routine
% Validates the JSON returned by a test as part of general health tests
% Uses Feature validate, but through central routine     
step(_Config, Result, {step_then, _N, ["the node is properly formatted"]}) -> 
  crowbar_rest:step(_Config, Result, {step_then, _N, ["the", nodes, "object is properly formatted"]});

% Common Routine
<<<<<<< HEAD
% Cleans up users that are created during tests                         
% step(Config, _Given, {step_finally, _N, ["REST removes the user ",User]}) -> 
%   io:format("Deleting user:  ~p  finally finally finally finally finally finally finally finally finally", [User]),
%   crowbar_rest:destroy(Config, g(path), g(username));

%setup, takes care of create               
step(Config, _Global, {step_setup, _N, _}) -> 
  User = json(g(username), g(email), g(password), g(password_confirmation), g(remember_me), g(is_admin)),
  bdd_utils:log(Config, info, "Setup users tests, creating user: ~p",[User]),
  bdd_restrat:create(Config, username, g(path), User),
  Config;

%teardown, takes care of delete test.
step(Config, _Global, {step_teardown, _N, _}) -> 
io:format("Destroy step_teardownstep_teardownstep_teardownstep_teardownstep_teardown"),
   bdd_restrat:destroy(Config, g(path), g(username)).
=======
% Cleans up nodes that are created during tests                         
step(Config, _Given, {step_finally, _N, ["REST removes the node",Node]}) -> 
  crowbar_rest:destroy(Config, g(path), Node);
                   
step(Config, _Global, {step_setup, _N, _}) -> Config;

step(Config, _Global, {step_teardown, _N, _}) -> Config.
>>>>>>> c75ce1037e33eb20cb72e5f019001ca40f202116
