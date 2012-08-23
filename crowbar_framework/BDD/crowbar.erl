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
% Author: RobHirschfeld 
% 
-module(crowbar).
-export([step/3]).
-import(bdd_utils).
-import(json).

% helper for checking to make sure the ID of the object your are using it the same as the one from setup
step(Config, Results, {step_then, _N, ["key",ID,"should match",Atom,"from setup"]}) -> 
  SetupID = bdd_utils:config(Config, list_to_atom(Atom)),
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Results),     % ASSUME, only 1 ajax result per feature
  SetupID =:= json:value(JSON, ID);
                                                              

step(_Config, _Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;
                                    
step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.

