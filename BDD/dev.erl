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

-module(dev).
-export([pop/0, pop/1, unpop/0]).  
-export([storename/2]).  
-import(bdd_utils).
-import(digest_auth).
-include("bdd.hrl").

% create a base system
pop()           -> pop(default).
pop(ConfigRaw) ->
  bdd:start(ConfigRaw),
  bdd_utils:config_set(global_setup, dev),
  bdd_utils:config_set(inspect, false),
  {ok, Build} = file:consult(bdd_utils:config(simulator, "dev.config")),
  [ add_group(G) || G <- buildlist(Build, groups) ],
  [ add_node(N) || N <- buildlist(Build, nodes) ],
  map_node_attribs(get({dev, node}),get({dev, attrib})),
  bdd_utils:config_unset(global_setup),
  bdd_utils:config_set(inspect, true),
  io:format("Done.~n").

% tear it down
unpop()       ->  
  {ok, Build} = file:consult(bdd_utils:config(simulator, "dev.config")),
  [ remove(nodes, N) || {N, _, _, _, _} <- buildlist(Build, nodes) ], 
  [ remove(groups, G) || {G, _, _, _} <- buildlist(Build, groups) ],
  bdd:stop([]). 

buildlist(Source, Type) ->
  {Type, R} = lists:keyfind(Type, 1, Source),
  R.

storename(Type, Name) ->
  List = get({dev, Type}),
  Add = case List of 
    undefined -> [Name];
    L         -> L ++ [Name]
  end,
  put({dev, Type}, Add).
  
remove(Type, Atom) ->
  crowbar_rest:destroy([], apply(Type, g, [path]), Atom),
  bdd_utils:config_unset(Atom),
  bdd_utils:log(info, "Removed ~p with tag ~p", [Type, Atom]).

add_group({Atom, Name, Descripton, Order}) ->
  JSON = group_cb:json(Name, Descripton, Order, 'ui'),
  Path = apply(group_cb, g, [path]),
  Result = json:parse(bdd_restrat:create([], Path, JSON)),
  Key = json:keyfind(Result, id),
  bdd_utils:config_set(Atom, Key),
  storename(group_cb, Name),
  bdd_utils:log(info, "Created Group ~p=~p named ~p", [Atom, Key, Name]).

add_node({Atom, Name, Description, Order, Group}) ->
  JSON = node:json(Name, Description, Order),
  Path = apply(node, g, [path]),
  Result = json:parse(bdd_restrat:create([], Path, JSON)),
  Key = json:keyfind(Result, id),
  group_cg:step([], [], {step_when, 0, ["REST adds the node",Name,"to",Group]}),
  bdd_utils:config_set(Atom, Key),
  storename(node, Name),
  bdd_utils:log(info, "Created Node ~p=~p named ~p in group ~p", [Atom, Key, Name, Group]).

map_node_attribs([], _Attribs)         -> done;
map_node_attribs([N | Nodes], Attribs) ->
  [attrib:node_add_attrib([], N, A) || A <- Attribs],
  map_node_attribs(Nodes, Attribs).
  