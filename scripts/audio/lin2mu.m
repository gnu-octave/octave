function y = lin2mu (x)
  
# usage:  y = lin2mu (x)
#
# x is a vector of an 8- or 16-bit linearly encoded audio sample,
# This is transformed into a mu-law encoded vector.

# Written by AW (Andreas.Weingessel@ci.tuwien.ac.at) on Oct 17, 1994
# Copyright Department of Probability Theory and Statistics TU Wien

  if (nargin != 1)
    usage ("y = lin2mu (x)");
  endif

  if (! is_vector (x))
    error ("lin2mu: x must be a vector");
  endif
  
  # transform 8-bit format to 16-bit
  if (max (abs (x)) <= 128)
    x = 256 .* x;
  endif

  # determine sign of x, set sign(0) = 1.
  sig = sign(x) + (x == 0);

  # take absolute value of x, but force it to be smaller than 32636;
  # add bias 
  x = min (abs (x), 32635 * ones (size (x))) + 132;

  # find exponent and fraction of bineary representation
  [f, e] = log2 (x);

  y = 64 * sig - 16 * e - fix (32 * f) + 335;

endfunction
