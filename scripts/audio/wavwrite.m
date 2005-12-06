## Copyright (C) 2005 Michael Zeising
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} wavwrite(@var{filename}, @var{y})
## Write @var{y} to the canonical RIFF/WAVE sound file @var{filename}. A sample 
## rate of 8000 Hz and 16-bit samples are assumed. Each column of the data 
## represents a separate channel.
##
## @deftypefnx {Function File} {} wavwrite(@var{filename}, @var{y}, @var{fs})
## Set the sample rate to @var{fs} Hz.
##
## @deftypefnx {Function File} {} wavwrite(@var{filename}, @var{y}, @var{fs}, @var{bits})
## Set the sample rate to @var{fs} Hz and resolution to @var{bits} bits.
##
## @end deftypefn
##
## @seealso{wavread}

## Author: Michael Zeising <michael.zeising@stud.uni-erlangen.de>
## Created: 06 December 2005

function wavwrite (filename, y, samplesPerSec, bitsPerSample)
  BYTEORDER = "ieee-le";
  
  # parse arguments
  if (exist ("samplesPerSec","var") < 1)
    warning ("wavwrite: sample rate set to 8000 Hz")
    samplesPerSec = 8000;
  endif
  if (exist ("bitsPerSample","var") < 1)
    warning ("wavwrite: sample resolution set to 16-bit")
    bitsPerSample = 16;
  endif
  
  # determine sample format
  switch bitsPerSample
    case 8  
      format = "int8";
    case 16 
      format = "int16";
    case 32 
      format = "int32";
    otherwise
      fclose (fid);
      error ("wavread: sample resolution not supported");
  endswitch
  
  # calculate filesize
  channels = size(y)(2);
  n = size(y)(1);
  
  ckSize = n*channels*(bitsPerSample/8);       # size of data chunk
  
  # open file for writing binary
  [fid, msg] = fopen (filename, "wb");
  if (fid < 0)
    error ("wavwrite: %s", msg)
  endif
  
  # write RIFF/WAVE header
  c = 0;
  c += fwrite (fid, "RIFF",        "uchar");
  c += fwrite (fid, ckSize + 36,   "ulong", 0, BYTEORDER);   # file size - 8
  c += fwrite (fid, "WAVEfmt ",    "uchar");
  c += fwrite (fid, 16,            "ulong", 0, BYTEORDER);   # size of fmt chunk
  c += fwrite (fid, 0x0001,        "short", 0, BYTEORDER);   # sample format code (PCM)
  c += fwrite (fid, channels,      "short", 0, BYTEORDER);   # channels
  c += fwrite (fid, samplesPerSec, "ulong", 0, BYTEORDER);   # sample rate
  c += fwrite (fid, samplesPerSec*channels*bitsPerSample/8, "ulong", 0, BYTEORDER);   # bytes per second
  c += fwrite (fid, channels*bitsPerSample/8,               "short", 0, BYTEORDER);   # block align
  c += fwrite (fid, bitsPerSample, "short", 0, BYTEORDER);   # bits/sample
  c += fwrite (fid, "data",        "uchar");
  c += fwrite (fid, ckSize,        "ulong", 0, BYTEORDER);   # size of data chunk
  
  if (c < 25)
    fclose (fid);
    error ("wavread: writing to file failed")
  endif
  
  # scale samples
  switch bitsPerSample
    case 8
      y = floor (y*127 + 127);
    case {16,32}
      y = floor (y*((2 ** bitsPerSample) / 2 - 1));
  endswitch
  
  # interleave samples
  l = n*channels;
  for (i = 1:channels)
    yi(i:channels:l) = y(:,i);
  endfor
  
  # write to file
  c = fwrite (fid, yi, format, 0, BYTEORDER);
  
  fclose (fid);
  
endfunction
