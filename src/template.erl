-module(template).

-export([string/2]).

%% For tests
-export([tokens/1, parse/1]).

-type tok() :: at | {string, string()}.

-spec string(string(), {string(), string()}) -> string().
string(String, _Subs) -> String.

-spec tokens(string()) -> [tok()].
tokens([]) ->
    [];
tokens([$@ | T]) ->
    [at | tokens(T)];
tokens(S) ->
    {String, T} = lists:split(string:cspan(S, "@"), S),
    [{string, String} | tokens(T)].

-spec parse([tok()]) -> [{text, string()} | {var, string()}].
parse(Tokens) ->
    parse(Tokens, text).

parse([at| T], Terminal) ->
    parse(T, switch_terminal(Terminal));
parse([{string, S}| T], Terminal) ->
    [{Terminal, S}| parse(T, Terminal)];
parse([], _) ->
    [].

switch_terminal(text) ->
    var;
switch_terminal(var) ->
    text.
