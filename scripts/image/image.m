function image(x, zoom)

# Display an octave image matrix.
#
# image(x) displays a matrix as a color image. The elements of x are indices
# into the current colormap and should have values between 1 and the length
# of the colormap.
#
# image(x,zoom) changes the zoom factor.  The default value is 4.
#
# SEE ALSO: imshow, imagesc, colormap.

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  July 1994

  if (nargin == 0)
    # Load Bobbie Jo Richardson (Born 3/16/94)
    x = loadimage("default.img");
    zoom = 2;
  elseif(nargin == 1)
    zoom = 4;
  elseif(nargin > 2)
    usage ("image (matrix, [zoom])");
  endif

  # Generate random file name
  rnd_str = num2str(fix(rand*10000));
  ppm_name = ["image.", rnd_str, ".ppm" ];

  saveimage(ppm_name,x,"ppm");

  # Start the viewer
  # Try xv, then xloadimage.

  xv = sprintf("xv -expand %f %s",zoom,ppm_name);
  xloadimage = sprintf("xloadimage -zoom %f %s",zoom*100, ppm_name);
  rm = sprintf("rm -f %s",ppm_name);

  command = sprintf("( %s || %s && %s ) > /dev/null 2>&1 &", ...
                    xv, xloadimage, rm);

  shell_cmd(command);

endfunction
