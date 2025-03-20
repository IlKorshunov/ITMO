init(MAX_N) :- eratosthenes(MAX_N, List), assert(get_List(List)).

has_divider(N, D) :- D * D =< N, 0 is mod(N, D), !. 
has_divider(N, D) :- D * D =< N, D1 is D+1, has_divider(N, D1).

sieve(X, Now, []) :- Now > X, !.
sieve(X, Now, [Now|List]) :- Now =< X, prime(Now), Next is Now + 1, sieve(X, Next, List), !.
sieve(X, Now, List) :- Now =< X, composite(Now), Next is Now + 1, sieve(X, Next, List), !.

eratosthenes(N, List) :- sieve(N, 2, List).

prime(N) :- N > 1, \+ has_divider(N, 2).

composite(N) :- N > 1, \+ prime(N).

prime_divisors(N, Divisors) :- get_List(List), help(N, 2, List, Divisors), !.
cube_divisors(1, []) :- !.
cube_divisors(N, D) :- get_List(List), N1 is N*N*N, help(N1, 2, List, D), !.

help(N, Now, List, []) :- N = 1.
help(N, Now, List, [Now | Divisors]) :-  0 is mod(N, Now), N1 is N/Now,  help(N1, Now, List, Divisors).
help(N, Now, [H|T], Divisors) :- \+ 0 is mod(N, Now), help(N, H, T, Divisors).
