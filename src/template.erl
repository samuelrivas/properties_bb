-module(template).

-export([string/2]).

%% For tests
-export([tokens/1, parse/1]).

-type tok() :: at | {string, string()}.

-spec string(string(), {string(), string()}) -> string().
string(String, Subs) ->
    apply_subs(parse(tokens(String)), Subs).

apply_subs(Parsed, Subs) ->
    lists:concat([to_string(X, Subs) || X <- Parsed]).

to_string({text, Text}, _Subs) ->
    Text;
to_string({var, Var}, Subs) ->
    keysearch(Var, Subs).

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

keysearch(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} ->
            Value;
        false ->
            throw({not_found, {Key, List}})
    end.
