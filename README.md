# Naive Parallel Prime Numbers Sieve #

This program calculates prime numbers using a new* method for building a sieve.

Usage:
˘
```erlang
%% in this example the sieve of primes numbers will run from 1 to 1001 max.
primes_sieve:start(1000).
```

The idea behind this program is that each number will be running on its own Erlang process and it will know how to eliminate itself from the sieve.

All Erlang process will cooperate in creating the sieve and together they will arrive to the solution.

I got inspired by this paper: [Chemical Computing](http://users.minet.uni-jena.de/~dittrich//p/Dit2005upp.pdf),
tho I'm not necessarily claiming that I'm implementing that.

Yes, I know, one probably shouldn't be using Erlang for this anyway.

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
multiplying them by 2 and then adding 1.

Since I'm not a professional mathematician we wary of this method, since so far I haven't seen a sieve produced
using these properties of numbers.

## Algorithm ##

Algorithm to check if _n_ would be a candidate for making _n*2+1=p_ where _p_ stands for some prime number.
This uses the algorithm notation introduced by Knuth in TAOCP.

```
S1. [Initialize] Set k <- 3, res <- (k-1)/2, t <- n.
S2. [Is t lesser than k?] if t < k, terminate. n is candidate.
S3. [Is k greater than sqrt(2t+1)?] if k > sqrt(2*t+1), terminate, n is candidate.
S4. [Is t congruent to res?] if t % k == res, terminate. n is not a candidate.
S5. [Recycle] Set k <- k + 2, res <- res + 1, and go back to S2.
```

Or in another notation presented by Knuth in TAOCP:

Here _n_ is the number we want to test, _n > 0_, _k >= 3_, _r > 0_. The return value will be _n_ when _n_ is a candidate or
_-1_ when _n_ is not.

```
f((n)) = (n, 3, 1, 1);
f((n, k, r, 1)) = (n)  if n < k, (n, k, r, 2) otherwise;
f((n, k, r, 2)) = (n)  if k > sqrt(2*k+1), (n, k, r, 3) otherwise;
f((n, k, r, 3)) = (-1) if n % k == r, (n, k, r, 4) otherwise;
f((n, k, r, 4)) = (n, k+2, r+1, 1).
```

## Example ##

Write the numbers from 2 to 100 and then scratch of those `n` that satisfy: `n % 3 == 1` and are greater than `3`.
So `[4, 7, 10, 13 ... 100]` will be removed.

Then add `2` to `3` getting `5` and then add `1` to `1` getting `2`. This means now you will scratch those `n`
that satisfy: `n % 5 == 2`.

Do the same with those `n` that satisfy: `n % 7 == 3` and so on.

The remaining numbers:

`[1, 2, 3, 5, 6, 8, 9, 11, 14, 15, 18 ... 99]` when multiplied by `2` and then adding `1` to the result, will give you a prime number.

This sequence is [A005097](http://oeis.org/A005097).

* I call this method __new__ because so far I haven't seen it in the various math books I've been consulting. By new I mean the mathematical
properties outlined [here](https://github.com/videlalvaro/erlang-prime-sieve#the-sieve-method), not the _one process per number_ part. I'm not
claiming this is some advanced Number Theory result or anything of the like. If you have seen this method before, please let me know by opening
an issue.
