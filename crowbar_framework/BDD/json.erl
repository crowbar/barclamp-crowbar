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
% 
-module(json).
-export([parse/1, value/2, output/1, keyfind/2]).
-export([json_array/3, json_value/2]).
-import(bdd_utils).

-record(json, {list=[], raw=[]}).
-record(jsonkv, {value=[], raw=[]}).

keyfind(JSON, Key) when is_atom(Key) -> keyfind(JSON, atom_to_list(Key));
keyfind(JSON, Key)                   ->
  case lists:keyfind(Key, 1, JSON) of
    {Key, R} -> R;
    false -> not_found;
    _ -> error
  end.

value_list(JSON, [Key | []]) ->  
  value_item(JSON, Key);
value_list(JSON, [Key | Tail]) ->  
  value_list(value_item(JSON, Key), Tail).
value_item(JSON, Key) ->           
  K = string:strip(Key, left, $[),
  {K, V} = lists:keyfind(K, 1, JSON),
  V.
value(JSON, Key) ->    
  List = string:tokens(Key, "]"),
  value_list(JSON, List).
  
% handles values that are quoted (this one ends the quote)
json_value_quoted(Value, [$" | T]) ->
  #jsonkv{value=Value, raw=T};
  
json_value_quoted(Value, [Next | T]) ->
  json_value_quoted(Value ++ [Next], T).
  
% returns JSON Key Values with remaining JSON
json_value(Value, RawJSON) ->
  [Next | T] = RawJSON, 
  case Next of
    $: -> throw('unexpected token : in value');
    ${ -> J = json(#json{raw=RawJSON}, []),                   % recurse to get list
            #jsonkv{value=J#json.list, raw=J#json.raw};  
    $[ -> J = json_array(0, [], T),                    % recurse to get array using 0++ as the Key
            #jsonkv{value=J, raw=J#json.raw};  
    $, -> #jsonkv{value=string:strip(Value), raw=RawJSON};    % terminator, return
    $} -> #jsonkv{value=string:strip(Value), raw=RawJSON};    % terminator, return
    $] -> #jsonkv{value=string:strip(Value), raw=RawJSON};    % terminator, return
    $" -> json_value_quoted(Value, T);                        % run to next quote,exit
    _ -> json_value(Value ++ [Next], T)                       % recurse
  end.

% parses an Array of values using numbers as the keys
json_array(Index, Value, RawJSON) ->
  [Next | T] = RawJSON, 
  case {Value, Next} of
    {_, $:} -> throw('unexpected token : in array');
    {_, $}} -> throw('unexpected token } in array');
    {_, $[} -> throw('unexpected token [ in array');
    {_, ${} -> J = json(#json{raw=RawJSON}, []),                   % recurse to get list
               lists:merge([{Index, J#json.list}], json_array(Index+1, [], J#json.raw)); 
    {[], $,} -> json_array(Index, [], T);      % no value, but recurse to get next element
    {V, $,} -> lists:merge([{Index, string:strip(V)}], json_array(Index+1, [], T));      % recurse to get next element
    {V, $]} -> [{Index, string:strip(V)}];                     % terminator, return
    {V, $"} -> json_value_quoted(V, T);                        % run to next quote,exit
    {V, _} -> json_array(Index, V ++ [Next], T)                % recurse
  end.
  
% parses the Key Value pairs (KVPs) based on , & } delimiters
json(JSON, Key) ->
  [Next | T] = JSON#json.raw,
  case {Next, T} of
    {$", _}  -> json(JSON#json{raw=T}, Key);        % ignore
    {$\n, _} -> json(JSON#json{raw=T}, Key);        % ignore
    {${, _}  -> json(#json{raw=T}, []);             % start new hash
    {$,, _}  -> json(JSON#json{raw=T}, []);         % add new value
    {$:, _}  -> KV = json_value([], T),  % get value for key
             List = lists:merge(JSON#json.list, [{string:strip(Key), KV#jsonkv.value}]),
             json(#json{list=List, raw=KV#jsonkv.raw}, []);  % add new KVP
    {$}, []} -> JSON#json.list;                    %DONE!
    {$}, _}  -> JSON#json{raw=T};                   %List parse, but more remains!
    {_, _}   -> json(JSON#json{raw=T}, Key ++ [Next])  % add to key
  end.

% entry point
parse(RawJSON) ->
  json(#json{raw=RawJSON}, []).

% CREATE JSON STRING FROM List: [{K, V}, {K, V}, {K, [{K, V}, ...]}, ...]
% create json from list
output({json, List}) -> 
  lists:concat(["{", output_inner(List), "}"]);
output(List) -> 
  lists:concat(["{", output_inner(List), "}"]).

atomize({K, V}) ->
  Value = case V of
    [{_, _} | _] -> output(V);
    _ -> V
  end,
  lists:concat(["\"", K, "\":\"", Value, "\""]).

output_inner([Head | []]) ->
  atomize(Head);
output_inner([Head | Tail]) ->
  atomize(Head) ++ ", " ++ output_inner(Tail).

