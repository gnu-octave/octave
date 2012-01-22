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
## @deftypefn {Function File} {@var{y} =} wavread (@var{filename})
## Load the RIFF/WAVE sound file @var{filename}, and return the samples
## in vector @var{y}.  If the file contains multichannel data, then
## @var{y} is a matrix with the channels represented as columns.
##
## @deftypefnx {Function File} {[@var{y}, @var{Fs}, @var{bps}] =} wavread (@var{filename})
## Additionally return the sample rate (@var{fs}) in Hz and the number of bits
## per sample (@var{bps}).
##
## @deftypefnx {Function File} {[@dots{}] =} wavread (@var{filename}, @var{n})
## Read only the first @var{n} samples from each channel.
##
## @deftypefnx {Function File} {[@dots{}] =} wavread (@var{filename}, @var{n1} @var{n2})
## Read only samples @var{n1} through @var{n2} from each channel.
##
## @deftypefnx {Function File} {[@var{samples}, @var{channels}] =} wavread (@var{filename}, "size")
## Return the number of samples (@var{n}) and channels (@var{ch})
## instead of the audio data.
## @seealso{wavwrite}
## @end deftypefn

## Author: Michael Zeising <michael@michaels-website.de>
## Created: 06 December 2005

function [y, samples_per_sec, bits_per_sample] = wavread (filename, param)

  FORMAT_PCM        = 0x0001;   # PCM (8/16/32 bit)
  FORMAT_IEEE_FLOAT = 0x0003;   # IEEE float (32/64 bit)
  BYTEORDER         = "ieee-le";

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! ischar (filename))
    error ("wavread: FILENAME must be a character string");
  endif

  fid = -1;

  unwind_protect

    [fid, msg] = fopen (filename, "rb");

    if (fid < 0)
      error ("wavread: %s", msg);
    endif

    ## Get file size.
    fseek (fid, 0, "eof");
    file_size = ftell (fid);
    fseek (fid, 0, "bof");

    ## Find RIFF chunk.
    riff_size = find_chunk (fid, "RIFF", file_size);
    riff_pos = ftell (fid);
    if (riff_size == -1)
      error ("wavread: file contains no RIFF chunk");
    endif

    riff_type = char (fread (fid, 4))';
    if (! strcmp (riff_type, "WAVE"))
      error ("wavread: file contains no WAVE signature");
    endif
    riff_pos = riff_pos + 4;
    riff_size = riff_size - 4;

    ## Find format chunk inside the RIFF chunk.
    fseek (fid, riff_pos, "bof");
    fmt_size = find_chunk (fid, "fmt ", riff_size);
    fmt_pos = ftell(fid);
    if (fmt_size == -1)
      error ("wavread: file contains no format chunk");
    endif

    ## Find data chunk inside the RIFF chunk.
    ## We don't assume that it comes after the format chunk.
    fseek (fid, riff_pos, "bof");
    data_size = find_chunk (fid, "data", riff_size);
    data_pos = ftell (fid);
    if (data_size == -1)
      error ("wavread: file contains no data chunk");
    endif

    ### Read format chunk.
    fseek (fid, fmt_pos, "bof");

    ## Sample format code.
    format_tag = fread (fid, 1, "uint16", 0, BYTEORDER);
    if (format_tag != FORMAT_PCM && format_tag != FORMAT_IEEE_FLOAT)
      error ("wavread: sample format %#x is not supported", format_tag);
    endif

    ## Number of interleaved channels.
    channels = fread (fid, 1, "uint16", 0, BYTEORDER);

    ## Sample rate.
    samples_per_sec = fread (fid, 1, "uint32", 0, BYTEORDER);

    ## Bits per sample.
    fseek (fid, 6, "cof");
    bits_per_sample = fread (fid, 1, "uint16", 0, BYTEORDER);

    ### Read data chunk.
    fseek (fid, data_pos, "bof");

    ## Determine sample data type.
    if (format_tag == FORMAT_PCM)
      switch (bits_per_sample)
        case 8
          format = "uint8";
        case 16
          format = "int16";
        case 24
          format = "uint8";
        case 32
          format = "int32";
        otherwise
          error ("wavread: %d bits sample resolution is not supported with PCM",
                 bits_per_sample);
      endswitch
    else
      switch (bits_per_sample)
        case 32
          format = "float32";
        case 64
          format = "float64";
        otherwise
          error ("wavread: %d bits sample resolution is not supported with IEEE float",
                 bits_per_sample);
      endswitch
    endif

    ## Parse arguments.
    if (nargin == 1)
      length = idivide (8 * data_size, bits_per_sample);
    else
      nparams = numel (param);
      if (nparams == 1)
        ## Number of samples is given.
        length = param * channels;
      elseif (nparams == 2)
        ## Sample range is given.
        if (fseek (fid, (param(1)-1) * channels * (bits_per_sample/8), "cof") < 0)
          warning ("wavread: seeking failed");
        endif
        length = (param(2)-param(1)+1) * channels;
      elseif (nparams == 4 && char (param) == "size")
        ## Size of the file is requested.
        tmp = idivide (8 * data_size, channels * bits_per_sample);
        y = [tmp, channels];
        return;
      else
        error ("wavread: invalid PARAM argument");
      endif
    endif

    ## Read samples and close file.
    if (bits_per_sample == 24)
      length *= 3;
    endif

    [yi, n] = fread (fid, length, format, 0, BYTEORDER);

  unwind_protect_cleanup

    if (fid >= 0)
      fclose (fid);
    endif

  end_unwind_protect

  ## Check data.
  if (mod (numel (yi), channels) != 0)
    error ("wavread: data in %s doesn't match the number of channels",
           filename);
  endif

  if (bits_per_sample == 24)
    yi = reshape (yi, 3, rows(yi)/3)';
    yi(yi(:,3) >= 128, 3) -= 256;
    yi = yi * [1; 256; 65536];
  endif

  if (format_tag == FORMAT_PCM)
    ## Normalize samples.
    switch (bits_per_sample)
      case 8
        yi = (yi - 128)/128;
      case 16
        yi /= 32768;
      case 24
        yi /= 8388608;
      case 32
        yi /= 2147483648;
    endswitch
  endif

  ## Deinterleave.
  nr = numel (yi) / channels;
  y = reshape (yi, channels, nr)';

endfunction

## Given a chunk_id, scan through chunks from the current file position
## though at most size bytes.  Return the size of the found chunk, with
## file position pointing to the start of the chunk data.  Return -1 for
## size if chunk is not found.

function chunk_size = find_chunk (fid, chunk_id, size)
  id = "";
  offset = 8;
  chunk_size = 0;

  while (! strcmp (id, chunk_id) && (offset < size))
    fseek (fid, chunk_size, "cof");
    id = char (fread (fid, 4))';
    chunk_size = fread (fid, 1, "uint32", 0, "ieee-le");
    ## Chunk sizes must be word-aligned (2 byte)
    chunk_size += rem (chunk_size, 2);
    offset = offset + 8 + chunk_size;
  endwhile
  if (! strcmp (id, chunk_id))
    chunk_size = -1;
  endif
endfunction

## Mark file as being tested.  Tests for wavread/wavwrite pair are in
## wavwrite.m
%!assert(1)
