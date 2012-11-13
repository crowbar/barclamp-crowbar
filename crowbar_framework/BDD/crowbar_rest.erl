% Copyright 2011, Dell 
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
-module(crowbar_rest).
-export([step/3, g/1, validate/1, inspector/2, get_id/2, get_id/3]).
-export([create/3, create/4, create/5, create/6, destroy/3]).
-import(bdd_utils).
-import(json).

g(Item) ->
  case Item of
    _ -> crowbar:g(Item)
  end.

% validates JSON in a generic way common to all objects
validate(JSON) ->
  try
    _CreatedAt    = json:keyfind(JSON, created_at),   % ADD CHECK!
    Id            = json:keyfind(JSON, id),
    Name          = json:keyfind(JSON, name), 
    _UpdatedAt    = json:keyfind(JSON, updated_at),  % ADD CHECK!
    R = [bdd_utils:is_a(name, Name), 
         bdd_utils:is_a(dbid, Id)],
    case bdd_utils:assert(R)of
      true -> true;
      false -> io:format("FAIL: JSON did not comply with object format ~p~n", [JSON]), false
    end
  catch
    X: Y -> io:format("ERROR: parse error ~p:~p~n", [X, Y]),
		false
	end. 

% Common Routine - returns a list of items from the system, used for house keeping
inspector(Config, Feature) ->
  Raw = eurl:get(Config, apply(Feature, g, [path])),
  JSON = json:parse(Raw),
  [{Feature, ID, Name} || {ID, Name} <- JSON].
  
% given a path + key, returns the ID of the object
get_id(Config, Path, Key) ->
   Lookup = eurl:path(Path,Key),
   get_id(Config, Lookup).
   
% given a path, returns the ID of the object
get_id(Config, Path) ->
  R = eurl:get(Config, Path, not_found),
  bdd_utils:log(Config, debug, "crowbar_rest:get_id R: ~p~n", [R]),
  {"id", ID} = case R of
    "null"  -> {"id", "-1"};
    "undef" -> {"id", "-1"};
    not_found -> {"id", "-1"};
    Result  -> lists:keyfind("id", 1, json:parse(Result))
  end,  
  ID.

% helper common to all setups using REST
create(Config, Path, JSON)         -> create(Config, Path, JSON, post).
create(Config, Path, JSON, Action) ->
  % just in case - cleanup to prevent collision
  destroy(Config, Path, json:keyfind(json:parse(JSON), name)),
  % create node(s) for tests
  eurl:put_post(Config, Path, JSON, Action).
  
create(Config, Path, Atom, Name, JSON) ->
  create(Config, Path, Atom, Name, JSON, post).

create(Config, Path, Atom, Name, JSON, Action) ->
  bdd_utils:log(Config, debug, "Entering crowbar_rest:create Path: ~p, Name: ~p, JSON: ~p~n", [Path, Name, JSON]),
  Result = create(Config, Path, JSON, Action),
  % get the ID of the created object
  Key = json:keyfind(Result, id),
  % friendly message
  io:format("\tCreated ~s (key=~s & id=~s) for testing.~n", [Name, Atom, Key]),
  % add the new ID to the config list
  [{Atom, Key} | Config].

% helper common to all setups using REST
destroy(Config, Path, Atom) when is_atom(Atom) ->
  Item = lists:keyfind(Atom, 1, Config),
  {Atom, Key} = Item,
  destroy(Config, Path, Key),
  lists:delete(Item, Config);

% helper common to all setups using REST
destroy(Config, Path, Key) ->
  case get_id(Config, Path, Key) of
    "-1" -> bdd_utils:log(Config, debug, "\tRemoval of key ~s skipped: not found.~n", [Key]);
    ID   -> eurl:delete(Config, Path, ID),
            bdd_utils:log(Config, info, "\tRemoved key ~s & id ~s.~n", [Key, ID])
  end,
  Config.

% NODES 
step(Config, _Global, {step_given, _N, ["there is a node",Node]}) -> 
  JSON = nodes:json(Node, nodes:g(description), 200),
  create(Config, nodes:g(path), JSON);

% remove the node
step(Config, _Given, {step_finally, _N, ["throw away node",Node]}) -> 
  eurl:delete(Config, nodes:g(path), Node);

% GROUPS
step(Config, _Global, {step_given, _N, ["there is a",Category,"group",Group]}) -> 
  JSON = groups:json(Group, groups:g(description), 200, Category),
  create(Config, groups:g(path), JSON, post);

% remove the group
step(Config, _Given, {step_finally, _N, ["throw away group",Group]}) -> 
  destroy(Config, groups:g(path), Group);


% ============================  THEN STEPS =========================================

% validate object based on basic rules for Crowbar
step(_Config, Result, {step_then, _N, ["the object is properly formatted"]}) -> 
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  validate(JSON);
  
% validate object based on it the validate method in it's ERL file (if any)
% expects an ATOM for the file
step(_Config, Result, {step_then, _N, ["the", Feature, "object is properly formatted"]}) -> 
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  apply(Feature, validate, [JSON]);

% validates a list of object IDs
step(_Config, Result, {step_then, _N, ["the object id list is properly formatted"]}) ->
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  NumberTester = fun(Value) -> bdd_utils:is_a(integer, Value) end,
  lists:all(NumberTester, JSON);

% ============================  LAST RESORT =========================================
step(_Config, _Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;
                                    
step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.
