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
-module(authenticate).
-export([step/3, g/1]).

g(Item) ->
  case Item of
    _ -> crowbar:g(Item)
  end.
  
                                                              
%step(Config, _Global, {step_setup, _N, _}) -> false;

%step(Config, _Global, {step_teardown, _N, _}) -> false;

step(Config, _Given, {step_when, _N, ["I go to the digest login page"]}) -> 
  URL = eurl:uri(Config, "digest"),
  {_Status,{{_Protocol,Code,_Comment}, _Fields, _Message}} = http:request(URL),
  {digest, Code};

step(Config, _Given, {step_when, _N, ["I go to home page"]}) ->
  URL = eurl:uri(Config, []),
  {_Status,{{_Protocol,_Code,_Comment}, _Fields, Message}} = http:request(URL),
  Message;

step(Config, _Given, {step_when, _N, ["I check the home page"]}) ->
  URL = eurl:uri(Config, []),
  {_Status,{{_Protocol,Code,_Comment}, _Fields, _Message}} = http:request(URL),
  {digest, Code};

step(Config, _Given, {step_when, _N, ["I go to node status page"]}) ->
  URL = eurl:uri(Config, "2.0/status/node"),
  {_Status,{{_Protocol,Code,_Comment}, _Fields, _Message}} = http:request(URL),
  {digest, Code};

step(Config, _Given, {step_when, _N, ["I login with",User,"and",Pass]}) -> 
  C = [{url, bdd_utils:config(Config, url)}, {user, User}, {password, Pass}],
  URL = eurl:uri(C, "digest"),
  {_Status,{{_Protocol,_Code,_Comment}, _Fields, R}} = digest_auth:request(C,URL),
  R;

step(_Config, Result, {step_then, _N, ["I should get a",Code,"error"]}) -> 
  {C, _} = string:to_integer(Code),
  {digest, R} = lists:keyfind(digest, 1, Result),
  R =:= C.

