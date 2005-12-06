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
## @deftypefn {Function File} {} @var{y} = wavread(@var{filename})
## Load the RIFF/WAVE sound file @var{filename}, returning the samples in vector 
## @var{y}. If the file contains multichannel data, then @var{y} is a matrix with the 
## channels represented as columns.
##
## @deftypefnx {Function File} {} [@var{y},@var{Fs},@var{bits}] = wavread(@var{filename})
## Additionally return the sample rate (@var{fs}) in Hz and the number of bits 
## per sample (@var{bits}).
##
## @deftypefnx {Function File} {} [...] = wavread(@var{filename},@var{n})
## Read only the first @var{n} samples from each channel.
##
## @deftypefnx {Function File} {} [...] = wavread(@var{filename},[@var{n1} @var{n2}])
## Read only samples @var{n1} through @var{n2} from each channel.
##
## @deftypefnx {Function File} {} [@var{samples} @var{channels}] = wavread(@var{filename},'size')
## Return the number of samples (@var{n}) and channels (@var{ch}) instead of the 
## audio data.
##
## @end deftypefn
##
## @seealso{wavwrite}

## Author: Michael Zeising <michael.zeising@stud.uni-erlangen.de>
## Created: 06 December 2005

function [y, samplesPerSec, bitsPerSample] = wavread (filename, param)
  FORMAT_PCM        = 0x0001;   # PCM (8/16/32 bit)
  FORMAT_IEEE_FLOAT = 0x0003;   # IEEE float (32/64 bit)
  FORMAT_ALAW       = 0x0006;   # 8-bit ITU-T G.711 A-law   (not yet supported)
  FORMAT_MULAW      = 0x0007;   # 8-bit ITU-T G.711 µ-law   (not yet supported)
  FORMAT_IMA_ADPCM  = 0x0011;   # IMA/ADPCM 4:1 compression (not yet supported)
  BYTEORDER         = "ieee-le";

  # open file for binary reading
  [fid, msg] = fopen (filename, "rb");
  if (fid < 0)
    error ("wavread: %s", msg)
  endif
  
  # check for RIFF/WAVE header
  ckID = char (fread (fid, 4))';                     # chunk ID: "RIFF"
  fseek (fid, 4, SEEK_CUR);
  WAVEID = char (fread (fid, 4))';                   # WAVE ID: "WAVE"
  if ((ckID ~= "RIFF") || (WAVEID ~= "WAVE"))
    fclose (fid);
    error ("wavread: file contains no RIFF/WAVE signature");
  endif
  
  # find format chunk within the next 256 (4*64) bytes
  i = 1;
  while 1
    if (char (fread (fid, 4))' == "fmt ")
      break
    endif
    if (i++ == 64)
      fclose (fid);
      error ("wavread: file contains no format chunk") 
    endif
  endwhile
  
  ckSize = fread (fid, 1, "ulong", 0, BYTEORDER);           # format chunk size
  
  formatTag = fread (fid, 1, "short", 0, BYTEORDER);        # sample format code
  if ((formatTag ~= FORMAT_PCM) && (formatTag ~= FORMAT_IEEE_FLOAT))
    fclose (fid);
    error ("wavread: sample format %#x is not supported", formatTag) 
  endif
  
  channels = fread (fid, 1, "short", 0, BYTEORDER);         # number of interleaved channels
  samplesPerSec = fread (fid, 1, "ulong", 0, BYTEORDER);    # sample rate
  fseek (fid, 6, SEEK_CUR);
  bitsPerSample = fread (fid, 1, "short", 0, BYTEORDER);    # bits per sample
  # ignore the rest of the chunk
  fseek (fid, ckSize-16, SEEK_CUR);
  
  # find data chunk
  i = 1;
  while 1
    if (char (fread(fid, 4))' == "data")
      break
    endif
    if (i++ == 64)
      fclose (fid);
      error ("wavread: file contains no data chunk")
    endif
  end
  
  ckSize = fread (fid, 1, "ulong", 0, BYTEORDER);            # data chunk size
  
  # determine sample data type
  if (formatTag == FORMAT_PCM)
    switch bitsPerSample
      case 8
        format = "int8";
      case 16 
        format = "int16";
      case 32 
        format = "int32";
      otherwise
        fclose (fid);
        error ("wavread: %d bits sample resolution is not supported with PCM", bitsPerSample);
    endswitch
  else
    switch bitsPerSample
      case 32 
        format = "float32";
      case 64 
        format = "float64";
      otherwise
        fclose (fid);
        error ("wavread: %d bits sample resolution is not supported with IEEE float", bitsPerSample);
    endswitch
  endif
  
  # parse arguments
  if (exist ("param","var") < 1)
    length = inf;
  else
    if (size(param)(2) == 1)                                 # number of samples is given
      length = param * channels;
    elseif (size(param)(2) == 2)                             # sample range is given
      if fseek(fid, param(1) * channels * (bitsPerSample/8), SEEK_CUR) < 0
        warning ("wavread: seeking failed")
      endif
      length = (param(2)-param(1)) * channels;
    elseif ((size (param)(2) == 4) && (char(param) == "size"))   # size of the file is requested
      fclose (fid);
      y = [ckSize/channels/bitsPerSample/8 channels];
      return
    else
      fclose (fid);
      error ("wavread: invalid argument 2");
    endif
  endif
  
  # read samples
  [yi, n] = fread (fid, length, format, 0, BYTEORDER);
  
  fclose (fid);
  
  if (formatTag == FORMAT_PCM)
    # normalize samples
    switch bitsPerSample
      case 8
        yi = (yi - 127)/127;      # 8-bit samples are unsigned
      case {16,32}
        yi = yi/((2 ** bitsPerSample) / 2 - 1);
    endswitch
  endif
  
  # deinterleave
  y = [];
  for (i = 1:channels)
    y = [y yi(i:channels:n)];
  endfor
  
endfunction
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
