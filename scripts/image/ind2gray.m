function Y = ind2gray(X,map)

# Convert an octave indexed image to a gray scale intensity image.
#
# Y = ind2gray(X) converts an indexed image to a gray scale intensity
# image.  The current colormap is used to determine the intensities.
# The intensity values lie between 0 and 1 inclusive.
#
# Y = ind2gray(X,map) uses the specified colormap instead of the current
# one in the conversion process.
#
# SEE ALSO: gray2ind, rgb2ntsc, image, colormap

  if (nargin == 1)
    map = colormap;
  endif

  # Convert colormap to intensity values.
  yiq = rgb2ntsc(map);
  y = yiq(:,1);

  # We need Fortran indexing capability, but be sure to save the user's
  # preference.
  pref = do_fortran_indexing;
  do_fortran_indexing = "true";

  # Replace indices in the input matrix with indexed values in the output
  # matrix.
  [rows, cols] = size(X);
  Y = y(X(:));
  Y = reshape(Y,rows,cols);

  # Restore the user's preference.
  do_fortran_indexing = pref;

endfunction
