## -*- texinfo -*-
## @deftypefn {} {} formula (@var{fun})
## Return a character string representing the inline function @var{fun}.
##
## Note that @code{char (@var{fun})} is equivalent to
## @code{formula (@var{fun})}.
## @seealso{char, argnames, inline, vectorize}
## @end deftypefn

function expr = formula (obj)

  if (nargin != 1)
    print_usage ();
  endif

  expr = obj.expr;

endfunction
