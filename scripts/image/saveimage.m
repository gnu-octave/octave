function saveimage(filename,X,img_form,map)

# Save a matrix to disk in image format.
# 
# saveimage(filename,x) saves matrix x to file filename in octave's image
# format.  The current colormap is saved in the file also.
# 
# saveimage(filename,x,"img") saves the image in the default format and
# is the same as saveimage(filename,x).
# 
# saveimage(filename,x,"ppm") saves the image in ppm format instead of
# the default octave image format.
# 
# saveimage(filename,x,"ps") saves the image in PostScript format instead
# of the default octave image format. (Note: images saved in PostScript format
# can not be read back into octave with loadimage.)
# 
# saveimage(filename,x,format,map) saves the image along with the specified
# colormap in the specified format.
# 
# Note: If the colormap contains only two entries and these entries are black
# and white, the bitmap ppm and PostScript formats are used.  If the image is
# a gray scale image (the entries within each row of the colormap are equal)
# the gray scale ppm and PostScript image formats are used, otherwise the full
# color formats are used.
# 
# SEE ALSO: loadimage, save, load, colormap

  if(nargin < 2)
    error("usage: saveimage(filename,matrix,[format, [colormap]])");
  elseif(nargin == 2)
    if(!isstr(filename))
      error("File name must be a string.");
    endif
    map = colormap;
    img_form = "img";
  elseif(nargin == 3)
    if(!isstr(img_form))
      error("Image format specification must be a string");
    endif
    map = colormap;
  endif

  if (strcmp(img_form,"img") == 1)
    oct_file = filename;
  elseif (strcmp(img_form,"ppm") == 1)
    # We need a temporary octave image file name.
    oct_file = sprintf("image.%s.img",num2str(fix(rand*10000)));
    ppm_file = filename;
  elseif (strcmp(img_form,"ps") == 1)
    # We need a temporary octave image file name.
    oct_file = sprintf("image.%s.img",num2str(fix(rand*10000)));
    ps_file = filename;
  endif

  # Save image in octave image file format
  eval(['save ', oct_file, ' map X']);

  # Convert to another format if requested.
  if (strcmp(img_form,"ppm") == 1)
    octtopnm = sprintf("octtopnm %s > %s",oct_file,filename);
    rm = sprintf("rm -f %s",oct_file);
    shell_cmd(octtopnm);
    shell_cmd(rm);
  elseif (strcmp(img_form,"ps") == 1)
    octtopnm = sprintf("octtopnm %s",oct_file);
    ppmtops = sprintf("pnmtops > %s 2> /dev/null", filename);
    octtops = [ octtopnm, " | ", ppmtops ];
    rm = sprintf("rm -f %s",oct_file);
    shell_cmd(octtops);
    shell_cmd(rm);
  endif

endfunction
