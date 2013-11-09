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

%% Exercise: Write the basic properties to verify
%% * ++ is commutative
%% * ++ is associative
%% * [] is the identity element.
%%
%% Hint, instead of =:= you may use proper:equals/2 for better reporting
int_list() -> proper_types:list(proper_types:integer()).

prop_concat_commutative() ->
  false.

prop_concat_associative() ->
  false.

prop_concat_identity() ->
  false.

%% For the operation op/2, verify the same properties (one doesn't hold)
prop_op_commutative() ->
  false.

prop_op_associative() ->
  false.

prop_op_identity() ->
  false.

%%%_* Private Functions ================================================
op(A, B) when A < B -> A ++ B;
op(A, B)            -> B ++ A.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
