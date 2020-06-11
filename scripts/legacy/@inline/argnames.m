## -*- texinfo -*-
## @deftypefn {} {} argnames (@var{fun})
## Return a cell array of character strings containing the names of the
## arguments of the inline function @var{fun}.
## @seealso{inline, formula, vectorize}
## @end deftypefn

function args = argnames (obj)

  if (nargin != 1)
    print_usage ();
  endif

  args = obj.args;

endfunction
