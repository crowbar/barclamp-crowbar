%% Author: bokner (boris.okner@gmail.com)
%% Created: Apr 10, 2010
%% Description: HTTP digest authentication
%% Note: the code follows particular explanation given on Wikipedia.
%% Disclaimer: Use on your own risk. The author disclaims any liability 
%% with regard to using this code.
%% Posted at git://gist.github.com/362131.git

-module(digest_auth).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([auth/5]).
-export([realm_key/2]).  % debug

%%
%% API Functions
%%

%% Does digest authentication. 
%% Callback function passes an authorization header and a URL,
%% to make it possible to construct consequent http request with proper authorization.
%% For example:
%% AuthCallback = fun(AuthHeader, URL) -> 
%%	http:request(post, {URL, [{"Authorization", AuthHeader}], "application/x-www-form-urlencoded",
%%			Body}, [], [])
%%		  end,
%% digest_auth:auth("http://hostname.com/protected_resource", "POST", "user_name", "password", AuthCallback).
%%
%% Note: I assume that initial request can be made using GET. 
%% It seems logical, because it doesn't have to submit any data. The service I was testing the code on does just this,
%% event though consequent requests must be made with POST. 
%% Hence second argument (i.e. Method) refers to a method for consequent request, not for initial request.
%% Note: HTTP method names are case sensitive. Because this code doesn't imply using any particular HTTP client
%% for consequent request, I don't see the way to enforce proper method name. So when you write your callback
%% function, you have to make sure that the method name used by your http client logically corresponds 
%% to the one passed to auth/5, which always have to be a capitalized name, like "POST" or "GET".
%% Looking at the example above, string "POST" is used for a method name, while http:request/4 specifies
%% method as atom 'post'.  
%% 
%%	
auth(URL, Method, User, Password, AuthCallback) ->
	{http, _, _, _, DigestURI, _} = http_uri:parse(URL),
	{ok, {{_, ResponseCode, _},
	        Fields,
		_Response}} = 	http:request(get, {URL, []}, [], []),
	case ResponseCode of
		401 ->
			AuthorizationHeader = buildAuthHeader(DigestURI, Method, User, Password, Fields),
			AuthCallback(AuthorizationHeader, URL);
		_ ->
			{error, noAuthentication}
	end.

buildAuthHeader(URI, Method, User, Password, Fields) ->
	{Realm, Nonce, Nc, CNonce, Response, Opaque} = 
        calcResponse(Fields, User, Password, URI, Method, "0000000000000000"),
	lists:flatten(io_lib:format("Digest username=\"~s\",realm=\"~s\",nonce=\"~s\",uri=\"~s\",qop=auth,nc=~s,cnonce=\"~s\",response=\"~s\",opaque=\"~s\"", 
	[User, Realm, Nonce, URI, Nc, CNonce, Response, Opaque])).

calcResponse(Fields, User, Password, URI, Method, Nc) ->
	random:seed(now()),
	DigestLine = proplists:get_value("www-authenticate", Fields),
	[$D, $i, $g, $e, $s, $t, $  | DigestParamsStr] = DigestLine,
	DigestParams = [ digest_auth:realm_key(R, []) || R <- string:tokens(DigestParamsStr,",")],
	%% Calculate digest
	Realm = proplists:get_value("realm", DigestParams),
	Opaque = proplists:get_value("opaque", DigestParams),
	Nonce = proplists:get_value("nonce", DigestParams),
	CNonce = hex(integer_to_list(erlang:trunc(random:uniform()*10000000000000000))),
	Qop = proplists:get_value("qop", DigestParams),
	Response = calc_response(Method, User, Password, URI, Realm, Opaque, Nonce, Nc, CNonce, Qop),
	{Realm, Nonce,  Nc, CNonce, Response, Opaque}.	

calc_response(Method, User, Password, URI, Realm, Opaque, Nonce, Nc, CNonce, Qop) ->	
	HA1 = 	hex(binary_to_list(crypto:md5( string:join([User, Realm, Password], ":")))),
	HA2 = 	hex(binary_to_list(crypto:md5( string:join([Method, URI], ":")))),
	io:format("HA1:~p~n", [HA1]),
	io:format("HA2:~p~n", [HA2]),	
	%HA1 result, server nonce (nonce), request counter (nc), client nonce (cnonce), quality of protection code (qop) and HA2 result is calculated.
	Step3Arg = HA1 ++ ":" ++ 
		Nonce ++ ":" ++
		Nc ++ ":" ++ 
		CNonce ++ ":" ++ 
		Qop ++  ":" ++
		HA2,
	io:format("3rd step:~p~n", [Step3Arg]),
	hex(binary_to_list(crypto:md5( Step3Arg))).

%% Implements example of digest response calculation from Wikipedia 
%% (http://en.wikipedia.org/wiki/Digest_access_authentication)
%% 
test_calc_response() ->
	crypto:start(),
	io:format("Proper response is: 6629fae49393a05397450978507c4ef1~n"),
	calc_response("GET", "Mufasa", "Circle Of Life", "/dir/index.html", "testrealm@host.com", "5ccc069c403ebaf9f0171e9517f40e41", "dcd98b7102dd2f0e8b11d0f600bfb0c093", "00000001","0a4f113b", "auth").

%%
%% Local Functions
%%

realm_key([$  | Realm], [])   -> realm_key( Realm, []);
realm_key([$= | Realm], Key)  -> {Key, string:strip(Realm, both, $")};
realm_key([H | Realm], Key)   -> realm_key( Realm, Key++[H] ).

%% @hidden

digit_to_xchar(D) when (D >= 0) and (D < 10) ->
	D + 48;
digit_to_xchar(D) ->
	D + 87.


hex(S) ->
	hex(S, []).



hex([], Res) ->
	lists:reverse(Res);
hex([N | Ns], Res) ->
	hex(Ns, [digit_to_xchar(N rem 16),
					 digit_to_xchar(N div 16) | Res]).



