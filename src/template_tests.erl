-module(template_tests).

-define(PROPER_NO_IMPORTS, true). %% I hate imported functions!
-include_lib("proper/include/proper.hrl").
-compile(export_all).

%% Generators ==========================================================

%% Properties ==========================================================
%% Test that no substitutions leave the string intact
prop_string_empty_list() -> false.

%% Internals ============================================================
format_failure(Template, Substs, Expected, Result) ->
    io:format(
      "~nTemplate: ~p~n"
      "Substs  : ~p~n"
      "Expected: ~p~n"
      "Got     : ~p~n",
      [Template, Substs, Expected, Result]).
