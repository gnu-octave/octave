function [R G B] = ind2rgb(X,map)

# Convert an indexed image to red, green, and blue color components.
#
# [R G B] = ind2rgb(X) uses the current colormap for the conversion.
#
# [R G B] = ind2rgb(X,map) uses the specified colormap.
#
# SEE ALSO: rgb2ind, image, imshow, ind2gray, gray2ind.

  if(nargin == 1)
    map = colormap;
  endif

  [hi wi] = size(X);

  pref = do_fortran_indexing;

  unwind_protect

    do_fortran_indexing = "true";

    R = map(X(:),1);
    G = map(X(:),2);
    B = map(X(:),3);

    R = reshape(R,hi,wi);
    G = reshape(G,hi,wi);
    B = reshape(B,hi,wi);

  unwind_protect_cleanup
    do_fortran_indexing = pref;
  end_unwind_protect

endfunction
