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
## @seealso{wavread}
## @end deftypefn

## Author: Michael Zeising <michael.zeising@stud.uni-erlangen.de>
## Created: 06 December 2005

function wavwrite (filename, y, samples_per_sec, bits_per_sample)

  BYTEORDER = "ieee-le";
  
  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  ## parse arguments
  if (nargin < 3)
    warning ("wavwrite: sample rate set to 8000 Hz");
    samples_per_sec = 8000;
  endif

  if (nargin < 4)
    warning ("wavwrite: sample resolution set to 16-bit");
    bits_per_sample = 16;
  endif
  
  ## determine sample format
  switch (bits_per_sample)
    case 8  
      format = "uint8";
    case 16 
      format = "int16";
    case 32 
      format = "int32";
    otherwise
      error ("wavread: sample resolution not supported");
  endswitch
  
  ## calculate filesize
  channels = size(y)(2);
  n = size(y)(1);

  ## size of data chunk
  ck_size = n*channels*(bits_per_sample/8);
  
  ## open file for writing binary

  if (! ischar (filename))
    error ("wavwrite: expecting filename to be a character string");
  endif
    
  [fid, msg] = fopen (filename, "wb");
  if (fid < 0)
    error ("wavwrite: %s", msg)
  endif
  
  ## write RIFF/WAVE header
  c = 0;
  c += fwrite (fid, "RIFF", "uchar");

  ## file size - 8
  c += fwrite (fid, ck_size + 36, "ulong", 0, BYTEORDER);
  c += fwrite (fid, "WAVEfmt ", "uchar");

  ## size of fmt chunk
  c += fwrite (fid, 16, "ulong", 0, BYTEORDER);

  ## sample format code (PCM)
  c += fwrite (fid, 0x0001, "short", 0, BYTEORDER);

  ## channels
  c += fwrite (fid, channels, "short", 0, BYTEORDER);

  ## sample rate
  c += fwrite (fid, samples_per_sec, "ulong", 0, BYTEORDER);

  ## bytes per second
  bps = samples_per_sec*channels*bits_per_sample/8;
  c += fwrite (fid, bps, "ulong", 0, BYTEORDER);

  ## block align
  c += fwrite (fid, channels*bits_per_sample/8, "short", 0, BYTEORDER);

  c += fwrite (fid, bits_per_sample, "short", 0, BYTEORDER);   
  c += fwrite (fid, "data", "uchar");
  c += fwrite (fid, ck_size, "ulong", 0, BYTEORDER);
  
  if (c < 25)
    fclose (fid);
    error ("wavread: writing to file failed");
  endif
  
  ## interleave samples
  yi = reshape (y', n*channels, 1);
  
  ## scale samples
  switch (bits_per_sample)
    case 8
      yi = round (yi*127.5 + 127.5);
    case 16
      yi = floor (yi*32767.5);
    case 32
      yi = floor (yi*2147483647.5);
  endswitch
  
  ## write to file
  c = fwrite (fid, yi, format, 0, BYTEORDER);
  
  fclose (fid);
  
endfunction
