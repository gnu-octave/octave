## -*- texinfo -*-
## @deftypefn  {} {} polynomial ()
## @deftypefnx {} {} polynomial (@var{a})
## Create a polynomial object representing the polynomial
##
## @example
## a0 + a1 * x + a2 * x^2 + @dots{} + an * x^n
## @end example
##
## @noindent
## from a vector of coefficients [a0 a1 a2 @dots{} an].
## @end deftypefn

function p = polynomial (a)

  if (nargin == 0)
    p.poly = [0];
    p = class (p, "polynomial");
  else
    if (strcmp (class (a), "polynomial"))
      p = a;
    elseif (isreal (a) && isvector (a))
      p.poly = a(:).';  # force row vector
      p = class (p, "polynomial");
    else
      error ("@polynomial: A must be a real vector");
    endif
  endif

  superiorto ("double");

endfunction
