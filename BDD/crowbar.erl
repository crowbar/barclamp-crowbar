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
-module(crowbar).
-export([step/2, g/1, state/1, i18n/1, i18n/2, i18n/3, i18n/4, i18n/5, i18n/6, json/1, json/3, parse_object/1]).
-export([json_build/1]).
-include("bdd.hrl").

g(Item) ->
  case Item of
    "cli"   -> g(cli);
    i18n    -> "utils/i18n";
    version -> "v2";
    test_node_path -> "api/test/nodes";
    cli     -> bdd_utils:config(cli, "cd ../bin && ./crowbar");
    natural_key -> name;			% for most crowbar objects, this is the natural key.  override if not
    node_name -> "admin.bddtesting.com";
    node_atom -> admin_node;
    name    -> "bddtest";
    order   -> 9999;
    description -> "BDD Testing Only - should be automatically removed";
    error   -> -1;
    active  -> 0;
    todo    -> 1;
    transition -> 2;
    blocked -> 3;
    proposed -> 4;
    _ -> bdd_utils:log(warn, crowbar, g, "Could not resolve g request for ~p (fall through catch)", [Item]), false
  end.

state(Item) when is_atom(Item)  -> integer_to_list(g(Item));
state(Item)                     -> state(list_to_atom(Item)).

i18n(T1, T2, T3, T4, T5, T6) -> i18n_lookup([T1, T2, T3, T4, T5, T6]).
i18n(T1, T2, T3, T4, T5) -> i18n_lookup([T1, T2, T3, T4, T5]).
i18n(T1, T2, T3, T4) -> i18n_lookup([T1, T2, T3, T4]).
i18n(T1, T2, T3) -> i18n_lookup([T1, T2, T3]).
i18n(T1, T2) -> i18n_lookup([T1, T2]).
i18n(T) -> i18n_lookup([T]).
i18n_lookup(T) -> 
  Path = string:tokens(T, "+:/"),
  KeyList = case length(Path) of
    1 -> lists:nth(1, Path);
    _ -> Path
  end,
  Key = string:join(KeyList, "."),
  URI = eurl:path(["utils/i18n",Key]),
  bdd_utils:log(trace, crowbar, i18n, "looking up ~p", [URI]),
  R = eurl:get_http(URI),
  case R#http.code of
    200 -> R#http.data;
    _   -> bdd_utils:log(warn, crowbar, i18n, "Translation for ~p not found", [URI]), "!TRANSLATION MISSING!"
  end.

% rest response specific for crowbar API (only called when the vnd=crowbar)
parse_object(Results) ->
  [Type, Quantity | _] = Results#http.details,
  case Quantity of 
    "obj"   ->  JSON = json:parse(Results#http.data),
                ID = proplists:get_value("id", JSON),
                #obj{namespace = crowbar, data=JSON, type = Type, id = ID, url = Results#http.url};
    "array" ->  JSON = json:parse(Results#http.data),
                #array{namespace = crowbar, data=JSON, type = Type, url = Results#http.url, count = length(JSON) };
    "list"  ->  JSON = json:parse(Results#http.data),
                IDs = [proplists:get_value("id", I) || I <- JSON],
                #list{namespace = crowbar, data=JSON, type = Type, ids = IDs, url = Results#http.url, count = length(IDs) };
    "empty" ->  #obj{namespace = crowbar, data=none, type = Type, id = -1, url = Results#http.url };
    "error" ->  #obj{namespace = crowbar, data=error, type = Type, id = -1, url = Results#http.url };
    _       ->  bdd_utils:log(warn, "Crowbar API returned unexpected quantity flag (expected obj or list).  Returned ~p", [Results]),
                #item{namespace = crowbar, data=Results#http.data , url=Results#http.url}
  end.

json(Name, Description, Order)          -> json([{name, Name}, {description, Description}, {order, Order}]).

json(List) -> json:output(json_build(List)).

json_build([])                               -> [];
json_build({Key, Value}) when is_atom(Key)   -> json_build({atom_to_list(Key), Value});
json_build({Key, Value})                     -> {Key, Value};
json_build([Head | Tail])                    -> [ Head | json_build(Tail)].

%json(Part, JSON)  ->  
%  Key = atom_to_list(Part),
%  {Key, P} = lists:keyfind(Key,1,JSON), 
%  P.

% global setup
step(Global, {step_setup, {Scenario, _N}, Test}) -> 
  % setup the groups object override
  bdd_utils:log(debug, crowbar, step, "Global Setup alias: ~p",[get({scenario,alias_map})]),
  bdd_utils:alias(group, group_cb),
  bdd_utils:alias(user, user_cb),
  % turn off the delays in the test jig
  role:step(Global, {step_given, {Scenario, _N}, ["I set the",role, "test-admin", "property", "test", "to", "false"]}), 
  role:step(Global, {step_given, {Scenario, _N}, ["I set the",role, "test-server", "property", "test", "to", "false"]}), 
  role:step(Global, {step_given, {Scenario, _N}, ["I set the",role, "test-client", "property", "test", "to", "false"]}), 
  role:step(Global, {step_given, {Scenario, _N}, ["I set the",role, "test-library", "property", "test", "to", "false"]}), 
  role:step(Global, {step_given, {Scenario, _N}, ["I set the",role, "test-discovery", "property", "test", "to", "false"]}), 
  % create admin network
  network:make_admin(),
  % create node for testing
  bdd_utils:log(debug, crowbar, step, "Global Setup running (creating node ~p)",[g(node_name)]),
  Node = json([{name, g(node_name)}, {description, Test ++ g(description)}, {order, 100}, {alive, "true"}, {admin, "true"}]),
  bdd_crud:create(node:g(path), Node, g(node_atom));

% find the node from setup and remove it
step(_Global, {step_teardown, {_Scenario, _N}, _}) -> 
  bdd_utils:log(debug, crowbar, step, "Global Teardown running",[]),
  % remove node for testing
  bdd_crud:delete(g(node_atom));

% ============================  GIVEN STEPS =========================================

step(_Given, {step_when, _N, ["I18N checks",Key]}) ->
  URI = eurl:path(g(i18n),Key),
  eurl:get_http(URI);

step(Global, {step_given, {ScenarioID, _N}, ["there is a",role, Name]}) -> 
  step(Global, {step_given, {ScenarioID, _N}, ["there is a",role, Name, "in", barclamp, "crowbar", "for", jig, "test"]});

step(_Global, {step_given, {ScenarioID, _N}, ["there is a",role, Name, "in", barclamp, Barclamp, "for", jig, Jig]}) -> 
  bdd_utils:log(debug, crowbar, step, "REST creates the ~p ~p", [role, Name]),
  JSON = json([{name, Name}, {description, role:g(description)}, {order, role:g(order)}, {barclamp, Barclamp}, {jig_name, Jig}]),
  Path = role:g(path),
  bdd_restrat:create(Path, JSON, role, ScenarioID);

step(_Global, {step_given, {ScenarioID, _N}, [deployment,Deployment,"includes",role,Role]}) -> 
  step(_Global, {step_when, {ScenarioID, _N}, [deployment,Deployment,"includes",role,Role]});
step(_Given, {step_when, {ScenarioID, _N}, [deployment,Deployment,"includes",role,Role]}) -> 
  bdd_utils:log(debug, crowbar, step, "REST addes role ~p to deployment ~p", [Role, Deployment]),
  JSON = json([{role, Role}, {deployment, Deployment}]),
  Path = deployment_role:g(path),
  bdd_restrat:create(Path, JSON, deployment_role, ScenarioID);

 
step(_Global, {step_given, {_Scenario, _N}, ["test loads the",File,"data into",node, Node]}) -> 
  URL = eurl:path(g(test_node_path),Node),
  JSON = json([{source, File}]),
  bdd_crud:update(URL, JSON);

% ============================  WHEN STEPS =========================================

step(_Given, {step_when, {Scenario, _N}, ["I add",node, Node,"to",deployment, Deployment,"in",role,Role]}) -> 
  Path = node_role:g(path), 
  JSON = crowbar:json([{node, Node}, {role, Role}, {deployment, Deployment}]),
  bdd_utils:log(debug, annealer, step, "Add node_role ~p POST ~p",[Path, JSON]),
  bdd_restrat:create(Path, JSON, role, Scenario);


step(_Given, {step_when, _N, ["REST gets the",network,Network,range,"list"]})  -> 
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = range:path(Network,""),
  bdd_utils:log(debug, crowbar, step, "REST range get ~p list for ~p path", [Network, URI]),
  R = eurl:get_http(URI),
  [R, bdd_restrat:get_object(R)];

step(_Given, {step_when, _N, ["REST gets the",network,Network,range,Key]})  ->
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = range:path(Network,Key),
  bdd_utils:log(debug, crowbar, step, "REST range get the object ~p for ~p path", [Network, URI]),
  bdd_restrat:step(_Given, {step_when, _N, ["REST requests the",URI,"page"]});

% ============================  THEN STEPS =========================================


% helper for limiting checks to body
step(Result, {step_then, _N, ["I should see", Text, "in the body"]}) -> 
  bdd_webrat:step(Result, {step_then, _N, ["I should see", Text, "in section", "main_body"]});

% helper for limiting checks to body
step(Result, {step_then, _N, ["I should not see", Text, "in the body"]}) -> 
  bdd_webrat:step(Result, {step_then, _N, ["I should not see", Text, "in section", "main_body"]});

% ============================  LAST RESORT =========================================
step(_Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;
                                    
step(_Result, {step_then, _N, ["I should use my special step file"]}) -> true.
