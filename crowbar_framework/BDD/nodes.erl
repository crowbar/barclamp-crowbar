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
-module(nodes).
-export([step/3, http_post/3]).
-import(bdd_utils).

node_json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}]).

http_post(Config, Path, JSON) ->
  URL = bdd_utils:uri(Config, Path),
  R = digest_auth:request(Config, post, {URL, [], "application/json", JSON}, [{timeout, 10000}], []),  
  {ok, {{"HTTP/1.1",_ReturnCode, State}, _Head, Body}} = R,
	{ok, StateMP} = re:compile("OK"),
	case re:run(State, StateMP) of
		{match, _} -> json:parse(Body);
		_ -> "ERROR, return of " ++ URL ++ " result was not 200 OK. " ++ Body
	end. 
	
step(Config, _Global, {step_setup, _N, _}) -> 
  Path = "node/2.0/new",
  Node = node_json("BDD1.example.com", "Testing Only", 100),
  Result = http_post(Config, Path, Node),
  {"id", Key} = lists:keyfind("id",1,Result),
  io:format("\tCreated Node BDD1 (id=~p) for testing.~n", [Key]),
  {"node1", Key};

step(Config, _Global, {step_teardown, _N, _}) -> 
  io:format("\tNo Nodes Tear Down Step.~n"),
  Config;
  
step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.
