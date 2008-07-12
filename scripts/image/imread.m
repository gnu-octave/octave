## Copyright (C) 2002 Andy Adler
## Copyright (C) 2005 Stefan van der Walt <stefan@sun.ac.za>
## Copyright (C) 2006 Thomas Weber <thomas.weber.mail@gmail.com>
## Copyright (C) 2008 Kristian Rumberg <kristianrumberg@gmail.com>
## Copyright (C) 2008 Thomas L. Scofield <scofield@calvin.edu>
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{I} =} imread(@var{filename})
## Read images from various file formats.
##
## The size and numeric class of the output depends on the
## format of the image.  A colour image is returned as an
## MxNx3 matrix.  Grey-level and black-and-white images are
## of size MxN.
## The colour depth of the image determines the numeric
## class of the output: 'uint8' or 'uint16' for grey
## and colour, and 'logical' for black and white.
##
## Note: For image formats other than jpeg and png, the
## ImageMagick "convert" and "identify" utilities
## are needed. ImageMagick can be found at www.imagemagick.org
## @end deftypefn

## Author: Andy Adler
##
## Modified: Stefan van der Walt <stefan@sun.ac.za>
## Date: 24 January 2005
##
## Modified: Thomas Weber <thomas.weber.mail@gmail.com>
## Date: 20 December 2006
## Change parsing of imagemagick's output to get the 'color' depth for grayscale
## images
##
## Modified Kristian Rumberg <kristianrumberg@gmail.com>
## Date 2 April 2008
## Imread now works with BMP's created with "convert inputimage out.bmp"
## (tested with stable release Octave 3.0 in GNU/Linux and Windows XP),
## modified the calling parameters to identify and convert
##
## Modified Thomas Scofield <scofield 'at' calvin.edu
## Date 1 July 2008
## Imread now uses Magick++ API to GraphicsMagick libraries instead
## of ImageMagick libraries (tested with stable release Octave 3.0
## in GNU/Linux).  All images are read by GraphicsMagick routines;
## there is no longer exceptional handling of .png and .jpg files.

function varargout = imread ( filename, varargin )
  if (nargin < 1)
    usage ("I = imread(filename)")
  endif

  if (!ischar (filename))
    error ("imread: filename must be a string")
  endif

  filename = tilde_expand (filename);
  fn = file_in_path ( IMAGE_PATH, filename );
  if (isempty (fn))
    error ( "imread: cannot find %s", filename );
  endif

  [ig, ig, ext] = fileparts (fn);
  ext = upper (ext);    
    
  ## real work uses GraphicsMagick
  if ( file_in_loadpath ("__magick_read__.oct") )
    [varargout{1:nargout}] = __magick_read__ ( fn, varargin{:} );
    ## color .PNG formats (any others?) have 4 levels in the 3rd dimension
    if ( length (size (varargout{1}))==3 && length (varargout{1}(1,1,:)) > 3 )
      varargout{1} = varargout{1}(:,:,1:3);
    endif
    break
  endif
    
  ## The next line could be altered from "identify ..." to "gm identify ..."
  ## If we continue to carry this out with a system call, my guess is that
  ## more people will have ImageMagick than have GraphicsMagick.  But if
  ## we want this to work for people who have neither, then the system call
  ## needs to be scrapped and replaced with a call to a dynamically-linked
  ## routine that again employs the C++ API to GraphicsMagick.
  ## Note also the system call to ImageMagick's "convert" farther down.
  cmd = sprintf ('identify -verbose \"%s\" | grep -e "bit" -e Type', fn);
  [sys, ident] = system (cmd);
  if (sys != 0)
    error ( "imread: error running ImageMagick's 'identify' on %s", fn );
  endif
  depth = re_grab ( "([[:digit:]]{1,2})-bit", ident );
  imtype = re_grab ( "Type: ([[:alpha:]]*)", ident );

  depth = str2num (depth);
  if ( isempty (depth) || ( pow2 (nextpow2 (depth)) != depth ) )
    error ( "imread: invalid image depth %s", depth );
  endif

  if !( strcmp ( imtype, "Bilevel" )   || strcmp ( imtype, "Grayscale" )
        || strcmp ( imtype, "TrueColor" ) || strcmp ( imtype, "TrueColorMatte" )
        || strcmp ( imtype, "Palette" ) || strcmp ( imtype, "PaletteMatte" ) )
    # The 'PaletteMatte' option added by TLS to accomodate ImageMagick
    # on .png images.  It appears GraphicsMagick returns a different
    # string, so this will likely be only a temporary change.
    error ( "imread: unknown image type '%s'", imtype );
  endif

  switch (imtype)
    case {"Bilevel"} fmt = "pgm";
    case {"Grayscale"} fmt = "pgm";
    case {"TrueColor", "TrueColorMatte", "Palette", "PaletteMatte"}
      fmt = "ppm";
  endswitch
    
  ## Why are pipes so slow?
  ##    cmd = sprintf ( "convert -flatten -strip %s %s:-", fn, fmt );
    
  tmpf = [tmpnam(), ".", fmt];
  ##cmd = sprintf ( "convert -flatten -strip +compress '%s' '%s' 2>/dev/null",
  ##                fn, tmpf );
  cmd = sprintf ( "convert -strip \"%s\" \"%s\"", fn, tmpf );

  sys = system (cmd);    
  if (sys != 0)
    error ("imread: error running ImageMagick's 'convert'");
    unlink (tmpf);
  endif

  try
    fid = fopen ( tmpf, "rb" );
  catch
    unlink (tmpf);
    error ( "imread: could not open temporary file %s", tmpf )
  end_try_catch

  fgetl (fid); # P5 or P6 (pgm or ppm)
  [width, height] = sscanf ( fgetl (fid), "%d %d", "C" );
  fgetl (fid); # ignore max components

  if (depth == 16)
    ## PGM format has MSB first, i.e. big endian
    [data, count] = fread ( fid, "uint16", 0, "ieee-be" );
  else
    [data, count] = fread ( fid, "uint8" );
  endif
    
  fclose (fid);
  unlink (tmpf);

  if (any (strcmp ( imtype, {"TrueColor", "TrueColorMatte", ...
                             "Palette", "PaletteMatte"} ) ) )
    channels = 3;
  else
    channels = 1;
  endif
  if (count != width*height*channels)
    error ( "imread: image data chunk has invalid size %i != %i*%i*%i == %i",
            count, width, height, channels, width*height*channels );
  endif

  varargout = {};
  switch (imtype)
    case {"Bilevel"} varargout{1} = logical ( reshape (data, width, height)' );
    case {"Grayscale"} varargout{1} = uint8 ( reshape (data, width, height)' );
    case {"TrueColor", "TrueColorMatte", "Palette", "PaletteMatte"}
      varargout{1} = cat(3, reshape ( data(1:3:end), width, height )',
                         reshape ( data(2:3:end), width, height )',
                         reshape ( data(3:3:end), width, height )');
      eval( sprintf( "varargout{1} = uint%d(varargout{1});", depth ) );
  endswitch
endfunction

function value = re_grab ( re, str )
  T = regexp ( str, re, 'tokens' );
  if ( isempty (T) )
    value = "";
  else
    value = T{1}{1};
  endif
endfunction
