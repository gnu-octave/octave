function y = conv (a, b)
  
  # Convolve two vectors.
  # y = conv (a, b) returns a vector of length equal to length (a) +
  # length (b) -1.
  # If a and b are polynomial coefficient vectors, conv returns the
  # coefficients of the product polynomial.
  #
  # SEE ALSO: deconv, poly, roots, residue, polyval, polyderiv,
  # polyinteg 

  # Author:
  #  Tony Richardson
  #  amr@mpl.ucsd.edu
  #  June 1994

  if (nargin != 2)
    error ("usage:  conv(a,b)");
  endif

  if (is_matrix(a) || is_matrix(b))
    error("conv: both arguments must be vectors");
  endif

  la = length (a);
  lb = length (b);

  ly = la + lb - 1;

  # Ensure that both vectors are row vectors.
  if (rows (a) > 1)
    a = reshape (a, 1, la);
  endif
  if (rows (b) > 1)
    b = reshape (b, 1, lb);
  endif

  # Use the shortest vector as the coefficent vector to filter.
  if (la < lb)
    if (ly > lb)
      x = [b zeros (1, ly - lb)];
    else
      x = b;
    endif
    y = filter (a, 1, x);
  else
    if(ly > la)
      x = [a zeros (1, ly - la)];
    else
      x = a;
    endif
    y = filter (b, 1, x);
  endif

endfunction
