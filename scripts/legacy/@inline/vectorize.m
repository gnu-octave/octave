## -*- texinfo -*-
## @deftypefn {} {} vectorize (@var{fun})
## Create a vectorized version of the inline function @var{fun} by
## replacing all occurrences of @code{*}, @code{/}, etc., with
## @code{.*}, @code{./}, etc.
##
## This may be useful, for example, when using inline functions with
## numerical integration or optimization where a vector-valued function
## is expected.
##
## @example
## @group
## fcn = vectorize (inline ("x^2 - 1"))
##    @result{} fcn = f(x) = x.^2 - 1
## quadv (fcn, 0, 3)
##    @result{} 6
## @end group
## @end example
## @seealso{inline, formula, argnames}
## @end deftypefn

## The following function was translated directly from the original C++
## version.  Yes, it will be slow, but the use of inline functions is
## strongly discouraged anyway, and most expressions will probably be
## short.  It may also be buggy.  Well, don't use this object!  Use
## function handles instead!

function fcn = vectorize (obj)

  if (nargin != 1)
    print_usage ();
  endif

  new_expr = "";

  expr = obj.expr;
  len = length (expr);
  i = 1;

  while (i <= len)
    c = expr(i);

    if (c == "*" || c == "/" || c == "\\" || c == "^")
      if (i > 1 && expr(i-1) != ".")
        new_expr(end+1) = ".";
      endif

      ## Special case for ** operator.
      if (c == '*' && i < (len - 1) && expr(i+1) == '*')
        new_expr(end+1) = "*";
        i++;
      endif
    endif

    new_expr(end+1) = c;
    i++;

  endwhile

  fcn = inline (new_expr);

endfunction
