## INLINE contstructor

%!shared fn
%! fn = inline ("x.^2 + 1");
%!assert (feval (fn, 6), 37)
%!assert (fn (6), 37)
%!assert (feval (inline ("sum (x(:))"), [1 2; 3 4]), 10)
%!assert (feval (inline ("sqrt (x^2 + y^2)", "x", "y"), 3, 4), 5)
%!assert (feval (inline ("exp (P1*x) + P2", 3), 3, 4, 5), exp (3*4) + 5)

## Test input validation
%!error inline ()
%!error <EXPR must be a string> inline (1)
%!error <N must be an integer> inline ("2", ones (2,2))
%!error <N must be a positive integer> inline ("2", -1)
%!error <additional arguments must be strings> inline ("2", "x", -1, "y")

## FORMULA

%!assert (formula (fn), "x.^2 + 1")
%!assert (formula (fn), char (fn))

## ARGNAMES

%!assert (argnames (fn), {"x"})
%!assert (argnames (inline ("1e-3*y + 2e4*z")), {"y"; "z"})
%!assert (argnames (inline ("2", 2)), {"x"; "P1"; "P2"})

## VECTORIZE

%!assert (formula (vectorize (fn)), "x.^2 + 1")
%!assert (formula (vectorize (inline ("1e-3*y + 2e4*z"))), "1e-3.*y + 2e4.*z")
%!assert (formula (vectorize (inline ("2^x^5"))), "2.^x.^5")
