function [X, map] = loadimage(filename)
#Load an image file.
#
#[X, map] = loadimage(img_file) loads an image and it's associated color
#map from file img_file.  The image must be in stored in octave's image
#format.
#
#SEE ALSO: saveimage, load, save

  eval(['load ', filename]);

endfunction
