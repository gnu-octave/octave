### Copyright (C) 1996 John W. Eaton
###
### This file is part of Octave.
###
### Octave is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2, or (at your option)
### any later version.
###
### Octave is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with Octave; see the file COPYING.  If not, write to the Free
### Software Foundation, 59 Temple Place - Suite 330, Boston, MA
### 02111-1307, USA.

function playaudio (name, ext)
  
  ## usage: playaudio (name [, ext]) 
  ##        playaudio (X)
  ##
  ## `playaudio ("name" [, "ext"])' plays the audio file "name.ext". The
  ## default value for the "ext" argument, which has to be written
  ## without the initial ".", is "lin".
  ## Currently, the following audio formats are suppored:
  ## *) linear encoding with extension "lin" or "raw", played using
  ##    /dev/dsp 
  ## *) mu-law encoding with extension "mu", "au" or "snd", played
  ##    using /dev/audio 
  ##
  ## `playaudio (X)' plays the audio data contained in the vector X.

  ## Written by AW (Andreas.Weingessel@ci.tuwien.ac.at) on Apr 11, 1994
  ## Last modified by AW on Nov 7, 1994
  ## Copyright Dept of Probability Theory and Statistics TU Wien

  file = octave_tmp_file_name ();

  usage_msg = "playaudio (name [, ext])  or  playaudio (X)";
  
  if (nargin == 1 && is_vector (name) && ! isstr (name)) 
    ## play a vector
    [nr, nc] = size (name);
    if (nc != 1)
      if (nr == 1)
	name = name';
	nr = nc;
      else
	error ("playaudio: X must be a vector");
      endif
    endif
    X = name + 127;
    num = fopen (file, "w");
    c = fwrite (num, X, "uchar");
    fclose (num);
    system (sprintf ("cat %s > /dev/dsp", file));
    unlink (file);
  elseif (nargin >= 1 && isstr (name))
    ## play a file
    if (nargin == 1)
      name = [name, ".lin"];
    elseif (nargin == 2)
      name = [name, ".", ext];
    else
      usage (usage_msg);
    endif
    if (strcmp (ext, "lin") || strcmp (ext, "raw"))
      system (sprintf ("cat %s > /dev/dsp", name));
    elseif (strcmp (ext, "mu") || strcmp (ext, "au") || strcmp (ext, "snd"))
      system (sprintf ("cat %s > /dev/audio", name));
    else
      error ("playaudio does not support given extension");
    endif
  else
    usage (usage_msg);
  endif

endfunction
