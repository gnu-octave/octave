function [X map] = rgb2ind(R,G,B)
#Convert and RGB image to an octave indexed image.
#
#[X map] = rgb2ind(R,G,B)
#
#SEE ALSO: ind2rgb, rgb2ntsc.
#
#Bugs: The color map may have duplicate entries.

  if(nargin != 3)
    error("usage: [X, map] = rgb2ind(R,G,B)");
  endif

  [hi wi] = size(R);

  X = zeros(hi,wi);

  map = zeros(hi*wi,3);

  pref = do_fortran_indexing;
  do_fortran_indexing = "true";

  map(:,1) = R(:);
  map(:,2) = G(:);
  map(:,3) = B(:);

  X(:) = 1:(hi*wi);

  do_fortran_indexing = pref;

endfunction
