function [X, map] = loadimage(filename)

# Load an image file.
#
# [X, map] = loadimage(img_file) loads an image and it's associated color
# map from file img_file.  The image must be in stored in octave's image
# format.
#
# SEE ALSO: saveimage, load, save

  if (nargin == 0)
    usage ("loadimage (filename)");
  endif

  file = file_in_path (IMAGEPATH, filename);

  if (isempty (file))
    error ("loadimage: unable to find image file");
  endif

# XXX FIXME XXX -- file is assumed to have variables X and map.
  
  eval(['load ', file]);

endfunction
