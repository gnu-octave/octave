function imshow(a1,a2,a3)

# Display images.
#
# imshow(X) displays an indexed image using the current colormap.
#
# imshow(X,map) displays an indexed image using the specified colormap.
#
# imshow(I,n) displays a gray scale intensity image.
#
# imshow(R,G,B) displays an RGB image.
#
# SEE ALSO: image, imagesc, colormap, gray2ind, rgb2ind.

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  July 1994

  if (nargin == 0)
    usage ("imshow requires at least one argument.");
  elseif(nargin == 2)
    if(length(a2)==1)
      [a1 a2] = gray2ind(a1,a2);
    endif
    colormap(a2);
  elseif(nargin == 3)
    [a1 a2] = rgb2ind(a1,a2,a3);
    colormap(a2);
  endif

  image(a1);

endfunction
