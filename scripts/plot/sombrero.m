function sombrero (n)

# usage: sombrero (n)
#
# Draw a `sombrero' in three dimensions using n grid lines.  The
# function plotted is
#
#   z = sin (x^2 + y^2) / (x^2 + y^2);

  if (nargin != 1)
    error ("usage: sombrero (n)");
  endif

  x = y = linspace (-8, 8, n)';
  [xx, yy] = meshdom (x, y);
  r = sqrt (xx .^ 2 + yy .^ 2) + eps;
  z = sin (r) ./ r;

  mesh (x, y, z);

endfunction
