function rgb = ntsc2rgb(yiq)

  trans = [ 1. 1. 1.; 0.95617 -0.27269 -1.10374; 0.62143 -0.64681 1.70062 ];

  rgb = yiq * trans;

endfunction
