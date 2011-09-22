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

%% SC = Short Cuts for very commonly used bdd_utils to improve readabilty
-module(sc).
-export([url/1, user/1, password/1, domain/1]).

url(Config) -> bdd_utils:config(Config,url).
user(Config) -> bdd_utils:config(Config,user).
password(Config) -> bdd_utils:config(Config,password).
domain(Config) -> bdd_utils:config(Config,domain).