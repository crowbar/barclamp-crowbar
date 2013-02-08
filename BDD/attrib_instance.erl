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
-module(attrib_instance).
-export([json/1, validate/1, g/1]).
-export([node_add_attrib/3, path/2]).

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path    -> "attrib";    % MUST BE USED WITH NODE!!!
    attrib  -> "crowbar";
    value   -> "rocks";
    _       -> crowbar:g(Item)
  end.
  
% Common Routine
validate(JSON) ->
  R =[bdd_utils:is_a(JSON, dbid, node_id), 
      bdd_utils:is_a(JSON, dbid, attrib_id), 
      bdd_utils:is_a(JSON, str, value), 
      bdd_utils:is_a(JSON, "^(empty|unready|test|ready)$", state), 
      % standard checks (have to do because name is NOT standard)
      bdd_utils:is_a(JSON, string, created_at), % placeholder for createdat
      bdd_utils:is_a(JSON, string, updated_at), % placgit eholder for updatedat
      bdd_utils:is_a(JSON, "^([a-zA-Z0-9\\-\\_]*)@([a-zA-Z0-9\\-\\.]*$)", name), 
      bdd_utils:is_a(JSON, str, description),
      bdd_utils:is_a(JSON, integer, order),
      bdd_utils:is_a(JSON, dbid, id),
      length(JSON) =:= 10],
  bdd_utils:assert(R, debug). 

json(Value) ->  json:output([{"value",Value}]).
  
% helpers
path(Node, Attrib) ->
  NodePath = eurl:path(node:g(path), Node),
  AttribPath = eurl:path(NodePath, g(path)),
  eurl:path(AttribPath,Attrib).

node_add_attrib(Config, Node, Attribute) -> 
  Path = path(Node, Attribute),
  bdd_utils:log(debug, node, attrib, "Node connect node+attributes ~p", [Path]),
  eurl:put_post(Config, Path, json(g(value)), post).
     
  
