function p = polyderiv(p)
#polyderiv(c)
#Returns the coefficients of the derivative of the polynomial whose
#coefficients are given by vector c.
#
#SEE ALSO: poly, polyinteg, polyreduce, roots, conv, deconv, residue,
#          filter, polyval, polyvalm

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  June 1994

  if(nargin != 1)
    error("usage: polyderiv(vector)");
  endif

  if(is_matrix(p))
    error("argument must be a vector");
  endif

  lp = length(p);
  if(lp == 1)
    p = 0;
    return;
  elseif (lp == 0)
    p = [];
    return;
  end

  p = p(1:(lp-1)) .* [(lp-1):-1:1];

endfunction
