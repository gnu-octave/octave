function x = imagesc(x, zoom)

# Scale and display a matrix as an image.
#
# imagesc(x) displays a scaled version of the matrix x.  The matrix is
# scaled so that its entries are indices into the current colormap.
# The scaled matrix is returned.
#
# imagesc(x,zoom) sets the magnification, the default value is 4.
#
# SEE ALSO: image, imshow

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  July 1994

  if (nargin < 1 || nargin > 2)
    usage ("image (matrix, [zoom])");
  endif

  if (nargin == 1)
    zoom = 4;
  endif

  [ high, wide ] = size(x);

  maxval = max(max(x));
  minval = min(min(x));

  # Rescale matrix so that all values are in the range 0 to
  # length(colormap) inclusive
  if (maxval == minval)
    x = ones(high, wide);
  else
    # Rescale values to between 1 and length(colormap) inclusive.
    x = fix((x - minval)/(maxval - minval) * (length(colormap)-1)) + 1;
  endif

  image(x,zoom);

endfunction
