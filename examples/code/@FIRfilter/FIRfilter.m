## -*- texinfo -*-
## @deftypefn  {Function File} {} FIRfilter ()
## @deftypefnx {Function File} {} FIRfilter (@var{p})
## Create a FIR filter with polynomial @var{p} as coefficient vector.
## @end deftypefn

function f = FIRfilter (p)

  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 0)
    p = @polynomial ([1]);
  elseif (! isa (p, "polynomial"))
    error ("@FIRfilter: expecting polynomial as input argument");
  endif

  f.polynomial = [];
  f = class (f, "FIRfilter", p);

endfunction
