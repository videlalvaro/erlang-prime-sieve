# Naive Distributed Prime Numbers Sieve #

This program calculates prime numbers using a new mehtod for building a sieve.

Usage:

```erlang
%% in this example the sieve of primes numbers will run from 1 to 1001 max.
{ok, Pid} = primes_coordinator:start_link(1000).

%% asynchrnously build the sieve:
primes_coordinator:run(Pid).

%% obtain the sieve:
{ok, Sieve} = primes_coordinator:collect(Pid).

%% print the sieve:
io:format("~p~n", [Sieve]).
```

The idea behind this program is that each number will be running on its own Erlang process and it will know how to eliminate itself from the sieve.

## The sieve method ##

The sieve method uses the following property:

If `2n + 1` is Prime, then `n` can't be congruent to `(k-1)/2 modulo k`, for some `k > n`.

More specifically:

for `n < k < sqrt(2n + 1), n % k != (k-1) / 2` where `%` stands for the modulus operation.

because if:

`n = kj + (k-1) / 2 => k divides n * 2 + 1 for some j > 0.`

Replacing `n` for `kj + (k-1) / 2` we get:

```
2(kj + ((k-1)/2)) + 1 =
2kj + 2((k - 1)/2) + 1 =
2kj + k - 1 + 1 =
2kj + k =
3kj
           QED
```

The numbers that pass this test are kept in the sieve, and then they are returned when collect is called by
multipling them by 2 and then adding 1.

Since I'm not a professional matematician we wary of this method, since so far I haven't seen a sieve produced
using these properties of numbers.
