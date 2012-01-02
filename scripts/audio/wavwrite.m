## Copyright (C) 2005-2012 Michael Zeising
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
## @deftypefn  {Function File} {} wavwrite (@var{y}, @var{filename})
## @deftypefnx {Function File} {} wavwrite (@var{y}, @var{Fs}, @var{filename})
## @deftypefnx {Function File} {} wavwrite (@var{y}, @var{Fs}, @var{bps}, @var{filename})
## Write @var{y} to the canonical RIFF/WAVE sound file @var{filename}
## with sample rate @var{Fs} and bits per sample @var{bps}.  The
## default sample rate is 8000 Hz with 16-bits per sample.  Each column
## of the data represents a separate channel.
## @seealso{wavread}
## @end deftypefn

## Author: Michael Zeising <michael@michaels-website.de>
## Created: 06 December 2005

function wavwrite (y, varargin)

  BYTEORDER = "ieee-le";

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  ## Defaults.
  samples_per_sec = 8000;
  bits_per_sample = 16;

  filename = varargin{end};
  if (nargin > 2)
    samples_per_sec = varargin{1};
    if (nargin > 3)
      bits_per_sample = varargin{2};
    endif
  endif

  ## test arguments
  if (columns (y) < 1)
    error ("wavwrite: Y must have at least one column");
  endif
  if (columns (y) > 0x7FFF)
    error ("wavwrite: Y has more than 32767 columns (too many for a WAV-file)");
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
      error ("wavwrite: sample resolution not supported");
  endswitch

  ## calculate filesize
  [n, channels] = size (y);

  ## size of data chunk
  ck_size = n*channels*(bits_per_sample/8);

  if (! ischar (filename))
    error ("wavwrite: expecting FILENAME to be a character string");
  endif

  ## open file for writing binary
  [fid, msg] = fopen (filename, "wb");
  if (fid < 0)
    error ("wavwrite: %s", msg);
  endif

  ## write RIFF/WAVE header
  c = 0;
  c += fwrite (fid, "RIFF", "uchar");

  ## file size - 8
  c += fwrite (fid, ck_size + 36, "uint32", 0, BYTEORDER);
  c += fwrite (fid, "WAVEfmt ", "uchar");

  ## size of fmt chunk
  c += fwrite (fid, 16, "uint32", 0, BYTEORDER);

  ## sample format code (PCM)
  c += fwrite (fid, 1, "uint16", 0, BYTEORDER);

  ## channels
  c += fwrite (fid, channels, "uint16", 0, BYTEORDER);

  ## sample rate
  c += fwrite (fid, samples_per_sec, "uint32", 0, BYTEORDER);

  ## bytes per second
  byteps = samples_per_sec*channels*bits_per_sample/8;
  c += fwrite (fid, byteps, "uint32", 0, BYTEORDER);

  ## block align
  c += fwrite (fid, channels*bits_per_sample/8, "uint16", 0, BYTEORDER);

  c += fwrite (fid, bits_per_sample, "uint16", 0, BYTEORDER);
  c += fwrite (fid, "data", "uchar");
  c += fwrite (fid, ck_size, "uint32", 0, BYTEORDER);

  if (c < 25)
    fclose (fid);
    error ("wavwrite: writing to file failed");
  endif

  ## interleave samples
  yi = reshape (y', n*channels, 1);

  ## scale samples
  switch (bits_per_sample)
    case 8
      yi = round (yi*128 + 128);
    case 16
      yi = round (yi*32768);
    case 32
      yi = round (yi*2147483648);
  endswitch

  ## write to file
  c = fwrite (fid, yi, format, 0, BYTEORDER);

  fclose (fid);

endfunction


%!shared fname
%! fname = tmpnam ();

%!test
%! A = [-1:0.1:1; -1:0.1:1];
%! wavwrite (A, fname);
%! [B, samples_per_sec, bits_per_sample] = wavread (fname);
%! assert (A,B, 1/2^15);
%! assert (samples_per_sec, 8000);
%! assert (bits_per_sample, 16);
%! unlink (fname);
%
%!test
%! A = [-1:0.1:1; -1:0.1:1];
%! wavwrite (A, 4000, fname);
%! [B, samples_per_sec, bits_per_sample] = wavread (fname);
%! assert (A,B, 1/2^15);
%! assert (samples_per_sec, 4000);
%! assert (bits_per_sample, 16);
%! unlink (fname);
%
%!test
%! A = [-1:0.1:1; -1:0.1:1];
%! wavwrite (A, 4000, 8, fname);
%! [B, samples_per_sec, bits_per_sample] = wavread (fname);
%! assert (A,B, 1/128);
%! assert (samples_per_sec, 4000);
%! assert (bits_per_sample, 8);
%! unlink (fname);
%
%!test
%! A = [-2:2];
%! wavwrite (A, fname);
%! B = wavread (fname);
%! B *= 32768;
%! assert (B, [-32768 -32768 0 32767 32767]);
%! unlink (fname);

