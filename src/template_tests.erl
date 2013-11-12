-module(template_tests).

-define(PROPER_NO_IMPORTS, true). %% I hate imported functions!
-include_lib("proper/include/proper.hrl").
-compile(export_all).

%% Generators ==========================================================
printable_char() ->
    proper_types:elements(lists:seq($ , $~)).

string() ->
    proper_types:list(printable_char()).

text() ->
    {text, string()}.

var() ->
    {var, string()}.

%% Generates the internal representation of a string with substitutions
template() ->
    proper_types:list(proper_types:elements([text(), var()])).

%% Properties ==========================================================
%% Test that no substitutions leave the string intact
prop_string_empty_list() ->
    ?FORALL(S, string(), proper:equals(template:string(S, []), S)).

prop_tokens() ->
    ?FORALL(
       T, template(),
       proper:equals(to_tokens(T), template:tokens(to_string(T)))).

%% Internals ============================================================

%% Returns the expected token list from the internal representation of a
%% template
to_tokens(Template) ->
    lists:concat([to_tokens_acc(X) || X <- Template]).

to_tokens_acc({var, S}) ->
    [at, {string, S}, at];
to_tokens_acc({text, S}) ->
    [{string, S}].

%% Returns the string form from the internal representation of a template
to_string(Template) ->
    lists:concat([to_string_acc(X) || X <- Template]).

to_string_acc({var, V}) ->
    lists:flatten(io_lib:format("@~s@", [V]));
to_string_acc({text, S}) ->
    S.

format_failure(Template, Substs, Expected, Result) ->
    io:format(
      "~nTemplate: ~p~n"
      "Substs  : ~p~n"
      "Expected: ~p~n"
      "Got     : ~p~n",
      [Template, Substs, Expected, Result]).
