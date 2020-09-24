## -*- texinfo -*-
## @deftypefn  {} {} FIRfilter ()
## @deftypefnx {} {} FIRfilter (@var{p})
## Create a FIR filter with polynomial @var{p} as coefficient vector.
## @end deftypefn

function f = FIRfilter (p)

  if (nargin == 0)
    f.polynomial = @polynomial ([1]);
  else
    if (! isa (p, "polynomial"))
      error ("@FIRfilter: P must be a polynomial object");
    endif

    f.polynomial = p;
  endif

  f = class (f, "FIRfilter");

endfunction
