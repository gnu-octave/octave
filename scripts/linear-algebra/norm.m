function retval = norm (x, p)

# usage: norm (x, p)
#
# Compute the p-norm of x.
#
# If x is a matrix:
#
#   value of p     norm returns
#   ----------     ------------
#       1          1-norm, the largest column sum of x
#       2          largest singular value of x
#      Inf         infinity norm, the largest row sum of x
#     "fro"        Frobenius norm of x, sqrt (sum (diag (x' * x)))
#
# If x is a vector or a scalar:
#
#   value of p     norm returns
#   ----------     ------------
#      Inf         max (abs (x))
#     -Inf         min (abs (x))
#     other        p-norm of x, sum (abs (x) .^ p) ^ (1/p)
#
# If the second argument is missing, p = 2 is assumed.
#
# See also: cond, svd

  if (nargin < 1 || nargin > 2)
    error ("usage: norm (x [, p])")
  endif

# Do we have a vector or matrix as the first argument?

  if (rows (x) == 1 || columns (x) == 1)

    if (nargin == 2)
      if (isstr (p))
        if (strcmp (p, "fro"))
          retval = sqrt (sum (diag (x' * x)));
        else
          error ("norm: unrecognized norm");
        endif
      else
	if (p == Inf)
	  retval = max (abs (x));
	elseif (p == -Inf)
	  retval = min (abs (x));
	else
	  retval = sum (abs (x) .^ p) ^ (1/p);
	endif
      endif
    elseif (nargin == 1)
      retval = sum (abs (x) .^ 2) ^ 0.5;
    endif

  else

    if (nargin == 2)
      if (isstr (p))
        if (strcmp (p, "fro"))
          retval = sqrt (sum (diag (x' * x)));
        else
          error ("norm: unrecognized norm");
        endif
      else
        if (p == 1)
          retval = max (sum (abs (real (x)) + abs (imag (x))));
        elseif (p == 2)
          s = svd (x);
          retval = s (1);
        elseif (p == Inf)
          xp = x';
          retval = max (sum (abs (real (xp)) + abs (imag (xp))));
        endif
      endif
    elseif (nargin == 1)
      s = svd (x);
      retval = s (1);
    endif

  endif

endfunction
