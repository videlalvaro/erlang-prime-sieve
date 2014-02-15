# Naive Distributed Prime Numbers Sieve #

This program calculates prime numbers using a new* mehtod for building a sieve.

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

`n = kj + (k-1) / 2 => k divides 2n + 1 for some j > 0.`

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

## Example ##

Write the numbers from 2 to 100 and then scratch of those `n` that satisfy: `n % 3 == 1` and are greather than `3`.
So [4, 7, 10, 13 ... 100] will be removed.

Then add `2` to `3` getting `5` and then add `1` to `1` getting `2`. This means now you will scratch those `n`
that satisfy: `n % 5 == 2`.

Do the same with those `n` that satisfy: `n % 7 == 3` and so on.

The remaining numbers:

`[1, 2, 3, 5, 6, 8, 9, 11, 14, 15, 18 ... 99]` when multiplied by `2` and then adding `1` to the result, will give you a prime number.

This sequence is [A005097](http://oeis.org/A005097).

* I call this method __new__ because so far I haven't seen it in the various math books I've been consulting. I'm not claiming this is
some advanced Number Theory result or anything of the like. If you have seen this method before, please let me know by opening an issue.
