function retval = list_primes (n)

# usage: list_primes (n)
#
# List the first n primes.  If n is unspecified, the first 30 primes
# are listed.
#
# The algorithm used is from page 218 of the TeXbook.

  if (nargin > 0)
    if (! is_scalar (n))
      error ("list_primes: argument must be a scalar");
    endif
  endif

  if (nargin == 0)
    n = 30;
  endif

  if (n == 1)
    retval = 2;
    return;
  endif

  if (n == 2)
    retval = [2; 3];
    return;
  endif

  retval = zeros (1, n);
  retval (1) = 2;
  retval (2) = 3;

  n = n - 2;
  i = 3;
  p = 5;
  while (n > 0)

    is_prime = 1;
    is_unknown = 1;
    d = 3;
    while (is_unknown)
      a = fix (p / d);
      if (a <= d)
        is_unknown = 0;
      endif
      if (a * d == p)
        is_prime = 0;
        is_unknown = 0;
      endif
      d = d + 2;
    endwhile

    if (is_prime)
      retval (i++) = p;
      n--;
    endif
    p = p + 2;

  endwhile

endfunction