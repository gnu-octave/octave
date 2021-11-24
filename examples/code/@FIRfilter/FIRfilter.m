## -*- texinfo -*-
## @deftypefn  {} {} FIRfilter ()
## @deftypefnx {} {} FIRfilter (@var{p})
## Create a FIR filter with polynomial @var{p} as coefficient vector.
## @end deftypefn

function f = FIRfilter (p)

  if (nargin == 0)
    p = @polynomial ([1]);
  elseif (! isa (p, "polynomial"))
    error ("@FIRfilter: P must be a polynomial object");
  endif

  f.polynomial = [];
  f = class (f, "FIRfilter", p);

endfunction
