function [b, r] = deconv (y, a)

  # Deconvolve two vectors.
  #
  # [b, r] = deconv (y, a) solves for b and r such that 
  #    y = conv(a,b) + r
  #
  # If y and a are polynomial coefficient vectors, b will contain the
  # coefficients of the polynomial quotient and r will be a remander
  # polynomial of lowest order.
  #
  # SEE ALSO: conv, poly, roots, residue, polyval, polyderiv,
  # polyinteg 

  # Author:
  #  Tony Richardson
  #  amr@mpl.ucsd.edu
  #  June 1994

  if (nargin != 2)
    error ("usage:  deconv (y,a)");
  endif

  if (is_matrix (y) || is_matrix (a))
    error("conv: both arguments must be vectors");
  endif

  la = length (a);
  ly = length (y);

  lb = ly - la + 1;

  if (ly > la)
    b = filter (y, a, [1 zeros (1, ly - la)]);
  elseif (ly == la)
    b = filter (y, a, 1);
  else
    b = 0;
  endif

  b = polyreduce (b);

  lc = la + length (b) - 1;
  if (ly == lc)
    r = y - conv (a, b);
  else
    r = [ zeros(1, lc - ly) y] - conv (a, b);
  endif

  r = polyreduce (r);

endfunction
