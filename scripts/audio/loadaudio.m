function X = loadaudio (name, ext, bit)
  
# usage:  X = loadaudio (name [, ext [, bit]])
#
# Loads audio data from the file "name.ext" into the data vector X. 
# Default value for the "ext" argument, which has to be written
# without the initial ".", is "lin".
# Currently, the following audio formats are supported:
# *) mu-law encoding with extension "mu", "au" or "snd"
# *) linear encoding with extension "lin" or "raw"
# 
# The `bit' argument can be either 8 (default) or 16.
# Depending on the value of bit, linearly encoded files are
# interpreted as being in 8 and 16 bit format, respectively, and
# mu-law encoded files are transformed to 8 and 16-bit linear
# format, respectively.

# Written by AW (Andreas.Weingessel@ci.tuwien.ac.at) on Apr 10, 1994
# Last modified by AW on Oct 29, 1994

  if (nargin == 0 || nargin > 3)
    usage ("loadaudio (name [, ext [, bit]])");
  endif

  if (nargin == 1)
    ext = "lin";
  endif

  if (nargin < 3)
    bit = 8;
  elseif (bit != 8 && bit != 16)
    error ("loadaudio: bit must be either 8 or 16");
  endif

  name = [name, ".", ext];
  num = fopen (name, "r");

  if (strcmp (ext, "lin") || strcmp (ext, "raw"))
    if (bit == 8)
      [Y, c] = fread (num, inf, "uchar");
      X = Y - 127;
    else
      [X, c] = fread (num, inf, "short");
    endif
  elseif (strcmp (ext, "mu") || strcmp (ext, "au") || strcmp (ext, "snd"))
    [Y, c] = fread (num, inf, "uchar");
    # remove file header
    m = max (find (Y(1:64) == 0));
    if (! isempty (m))
      Y(1:m) = [];
    endif
    X = mu2lin (Y, bit);
  else
    fclose (num);
    error ("loadaudio does not support given extension");
  endif

  fclose (num);
  
endfunction



