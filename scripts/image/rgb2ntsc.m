function yiq = rgb2ntsc(rgb)

  trans = [ 0.299 0.596 0.211; 0.587 -0.274 -0.523; 0.114 -0.322 0.312 ];
  yiq = rgb * trans;

endfunction
