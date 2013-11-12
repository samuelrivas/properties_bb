-module(template).

-export([string/2]).

%% For tests
-export([tokens/1, parse/1]).

-type tok() :: at | {string, string()}.

-spec string(string(), {string(), string()}) -> string().
string(_String, _Subs) -> "".

-spec tokens(string()) -> [tok()].
tokens(_String) -> [].

-spec parse([tok()]) -> [{text, string()} | {var, string()}].
parse(_) -> [].
