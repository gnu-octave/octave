function p = polyinteg(p)

# polyinteg(c)
# Returns the coefficients of the integral the polynomial whose coefficients
# are represented by the vector c.
#
# The constant of integration is zero.
#
# SEE ALSO: poly, polyderiv, polyreduce, roots, conv, deconv, residue,
#           filter, polyval, polyvalm

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  June 1994

  if(nargin != 1)
    usage ("polyinteg(vector)");
  endif

  if(is_matrix(p))
    error("argument must be a vector");
  endif

  lp = length(p);

  if(lp == 0)
    p = [];
    return;
  end

  if(rows(p) > 1)
    # Convert to column vector
    p = p.';
  endif

  p = [ p 0 ] ./ [lp:-1:1 1];

endfunction
