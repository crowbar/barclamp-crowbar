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
-export([step/2, step/3, g/1, i18n/2, i18n/3, i18n/4, i18n/5, i18n/6, json/2, parse_object/1]).
-import(bdd_utils).
-import(json).
-include("bdd.hrl").

g(Item) ->
  case Item of
    "cli" -> g(cli);
    version -> "v2";
    cli -> bdd_utils:config(cli, "cd ../bin && ./crowbar");
    natural_key -> name;			% for most crowbar objects, this is the natural key.  override if not
    node_name -> "global-node.testing.com";
    node_atom -> global_node;
    name -> "bddtest";
    order -> 9999;
    description -> "BDD Testing Only - should be automatically removed";
    _ -> bdd_utils:log(warn, crowbar, g, "Could not resolve g request for ~p (fall through catch)", [Item]), false
  end.

i18n(_Config, T1, T2, T3, T4, T5) -> i18n_lookup([T1, T2, T3, T4, T5]).
i18n(_Config, T1, T2, T3, T4) -> i18n_lookup([T1, T2, T3, T4]).
i18n(_Config, T1, T2, T3) -> i18n_lookup([T1, T2, T3]).
i18n(_Config, T1, T2) -> i18n_lookup([T1, T2]).
i18n(_Config, T) -> i18n_lookup([T]).
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
    _   -> bdd_utils:log(warn, crowbar, i18n, "Translation for ~p not found", [URI]), "!TRANSLATON MISSING!"
  end.

% rest response specific for crowbar API (only called when the vnd=crowbar)
parse_object(Results) ->
  [Type, Quantity | _] = Results#http.details,
  case Quantity of 
    "obj"   ->  JSON = json:parse(Results#http.data),
                ID = proplists:get_value("id", JSON),
                #obj{namespace = crowbar, data=JSON, type = Type, id = ID, url = Results#http.url};
    "list"  ->  JSON = json:parse(Results#http.data),
                IDs = [proplists:get_value("id", I) || I <- JSON],
                #list{namespace = crowbar, data=JSON, type = Type, ids = IDs, url = Results#http.url, count = length(IDs) };
    "empty" ->  #obj{namespace = crowbar, data=none, type = Type, id = -1, url = Results#http.url };
    "error" ->  #obj{namespace = crowbar, data=error, type = Type, id = -1, url = Results#http.url };
    _       ->  bdd_utils:log(warn, "Crowbar API returned unexpected quantity flag (expected obj or list).  Returned ~p", [Results]),
                #item{namespace = crowbar, data=Results#http.data , url=Results#http.url}
  end.

json(Part, JSON)  ->  
  Key = atom_to_list(Part),
  {Key, P} = lists:keyfind(Key,1,JSON), 
  P.

% TEMPORARY REMAPPING
step(In, Out) -> step([], In, Out).

% global setup
step(Config, _Global, {step_setup, _N, Test}) -> 
  % setup the groups object override
  bdd_utils:config_set(alias_map,{group, group_cb}),
  bdd_utils:config_set(api_map,{"application/vnd.crowbar+json", crowbar_rest}),
  bdd_utils:log(debug, "crowbar:step Global Setup running (creating node ~p)",[g(node_name)]),
  Node = node:json(g(node_name), Test ++ g(description), 100),
  bdd_restrat:create(Config, node:g(path), g(node_atom), name, Node),
  Config;

% find the node from setup and remove it
step(Config, _Global, {step_teardown, _N, _}) -> 
  bdd_utils:log(debug, "crowbar:step Global Teardown running",[]),
  bdd_restrat:destroy(Config, node:g(path), g(node_atom)),
  Config;

% ============================  GIVEN STEPS =========================================

step(Config, _Given, {step_when, _N, ["I18N checks",Key]}) ->
  URI = eurl:path("utils/i18n",Key),
  R = eurl:get_page(Config, URI, all),
  bdd_utils:log(Config, trace, "crowbar:i18n get ~p gave ~p", [URI, R]),
  {Code, Details} = R,
  {ajax, Code, Details};

% ============================  THEN STEPS =========================================

% helper for limiting checks to body
step(_Config, Result, {step_then, _N, ["I should see", Text, "in the body"]}) -> 
  bdd_webrat:step(_Config, Result, {step_then, _N, ["I should see", Text, "in section", "main_body"]});

% helper for limiting checks to body
step(_Config, Result, {step_then, _N, ["I should not see", Text, "in the body"]}) -> 
  bdd_webrat:step(_Config, Result, {step_then, _N, ["I should not see", Text, "in section", "main_body"]});


% helper for checking to make sure the ID of the object your are using it the same as the one from setup
step(Config, Results, {step_then, _N, ["key",ID,"should match",Atom,"from setup"]}) -> 
  SetupID = bdd_utils:config(Config, list_to_atom(Atom)),
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Results),     % ASSUME, only 1 ajax result per feature
  SetupID =:= json:value(JSON, ID);

% ============================  LAST RESORT =========================================
step(_Config, _Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;
                                    
step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.
