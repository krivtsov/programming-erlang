-module(lib_primes).
-export([make_prime/1]).

make_prime(1) ->
  lists:nth(rand:uniform(5), [1, 2, 3, 5, 7]);

make_prime(K) when K > 0 ->
  new_seed(),
  N = make_random_int(K),  
  if N > 3 ->
    io:format("Generating a ~w digit prime", [K]),
    MaxTries = N - 3,
    P1 = make_prime(MaxTries, N + 1),
    io:format("~n", []),
    P1;
  true ->
    make_prime(K)
  end.

make_prime(0, _) ->
  exit(impossible);

make_prime(K, P) ->
  io:format(".", []),
  case is_prime(P) of
    true -> P;
    false -> make_prime(K - 1, P + 1)
  end.

is_prime(D) ->
  new_seed(),
  is_prime(D, 100).

is_prime(D, Ntests) ->
  N = length(integer_to_list(D)) - 1,
  is_prime(Ntests, D, N).

is_prime(0, _, _) -> true;

is_prime(Ntest, N, Len) ->
  K = rand:uniform(Len),
  A = make_random_int(K),
  if A < N ->
    case lib_lin_pow(A, N, N) of
      A -> is_prime(Ntest - 1, N, Len);
      _ -> false
    end;
  true ->
    is_prime(Ntest, N, Len)
  end.

make_random_int(N) -> new_seed(), make_random_int(N, 0).

make_random_int(0, D) -> D;

make_random_int(N, D) ->
  make_random_int(N-1, D*10 + (rand:uniform(10)-1)).

new_seed() ->
  {_,_,X} = erlang:timestamp(),
  {H,M,S} = time(),
  H1 = H * X rem 32767,
  M1 = M * X rem 32767,
  S1 = S * X rem 32767,
  put(random_seed, {H1,M1,S1}).

%% pow(A, B, M) => (A^B) mod M

lib_lin_pow(A, 1, M) ->
  A rem M;

lib_lin_pow(A, 2, M) ->
  (A * A) rem M;

lib_lin_pow(A, B, M) ->
  B1 = B div 2,
  B2 = B - B1,
  %% B2 = B1 or B1+1
  P = lib_lin_pow(A, B1, M),
  case B2 of
    B1 -> (P *P) rem M;
    _ -> (P * P * A) rem M
  end.



