%%% @doc
-module(properties_1).

%%%_* Exports ==========================================================
-export([]).

%%%_* Includes =========================================================
-define(PROPER_NO_IMPORTS, true). %% I hate imported functions!
-include_lib("proper/include/proper.hrl").

%%%_* Properties =======================================================

prop_sum_1() ->
  ?FORALL(X, proper_types:pos_integer(), X >= 0).

prop_sum_2() ->
  ?FORALL({X, Y},
          {proper_types:pos_integer(),proper_types:pos_integer()},
          X + Y =:= Y + X).

prop_sum_3() ->
  ?FORALL(X, proper_types:pos_integer(),
          ?FORALL(Y, proper_types:pos_integer(),
                  X + Y =:= Y + X)).


%%%_* Private Functions ================================================

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
