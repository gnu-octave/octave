/*

Copyright (C) 2013 Vytautas Jančauskas

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct.h"
#include "ov-struct.h"
#ifdef HAVE_SNDFILE
  #include <sndfile.h>
#endif
  
DEFUN_DLD(audioread, args, ,
"-*- texinfo -*-\n\
@deftypefn{Loadable Function} [@var{y}, @var{Fs}] = audioread(@var{filename})\n\
\n\
Load an audio file that is specified by @var{filename}. It will be loaded in to \
a column matrix with as many rows as there are audio frames and as many columns \
as there are channels in the file. Sampling rate will be stored in @var{Fs}. \
\n\
@end deftypefn\n\
@deftypefn{Loadable Function} [@var{y}, @var{Fs}] = audioread(@var{filename}, @var{samples})\n\
\n\
Read a specified range of samples from a file specified by @var{filename}. \
Argument @var{samples} is a vector with two values specifying starting frame \
and ending frame. \
\n\
@end deftypefn\n\
@deftypefn{Loadable Function} [@var{y}, @var{Fs}] = audioread(@var{filename}, @var{dataType})\n\
\n\
Read a file and return an array of specified type. If @var{dataType} is \"native\" then \
an array of fixed width integer type will be returned depending on how data is stored \
in the audio file. If @var{dataType} is \"double\" a double matrix will be returned. \
\n\
@end deftypefn\n\
@deftypefn{Loadable Function} [@var{y}, @var{Fs}] = audioread(@var{filename}, @var{samples}, @var{dataType})\n\
\n\
Read a file and return a specified range of frames in an array of specified type. \
\n\
@end deftypefn"
)
{
  octave_value_list retval;
#ifdef HAVE_SNDFILE
  Matrix audio;
  octave_value ret_audio;
  SNDFILE *file;
  SF_INFO info;
  info.format = 0;
  int start, end;
  file = sf_open(args(0).string_value ().c_str (), SFM_READ, &info);
  start = 0;
  end = info.frames;
  float *data = (float *)malloc (sizeof(float) * info.frames * info.channels);
  sf_read_float(file, data, info.frames * info.channels);
  if (args.length () == 2 && !args(1).is_string () || args.length () == 3)
    {
      RowVector range = args(1).row_vector_value ();
      start = range(0);
      end = range(1);
    }
  audio.resize (end - start, info.channels);
  for (int i = start; i < end; i++)
    {
      for (int channel = 0; channel < info.channels; channel++)
        {
          audio(i - start, channel) = data[i * info.channels + channel];
        }
    } 
  free (data);
  if (args.length () == 2 && args(1).is_string () || args.length () == 3)
    {
      std::string type;
      if (args.length () == 3)
        {
          type = args(2).string_value ();
        }
      else
        {
          type = args(1).string_value ();
        }
      if (type == "native")
        {
          if (info.format & SF_FORMAT_PCM_S8)
            {
              ret_audio = octave_value ((audio * 127)).int8_array_value ();
            }
          else if (info.format & SF_FORMAT_PCM_U8)
            {
              ret_audio = octave_value ((audio * 127 + 127)).uint8_array_value ();
            }
          else if (info.format & SF_FORMAT_PCM_16)
            {
              ret_audio = octave_value ((audio * 32767)).int16_array_value ();
            }
          else if (info.format & SF_FORMAT_PCM_24)
            {
              ret_audio = octave_value ((audio * 8388608)).int32_array_value ();
            }
          else if (info.format & SF_FORMAT_PCM_32)
            {
              ret_audio = octave_value ((audio * 2147483648)).int32_array_value ();
            }
          else
            {
              ret_audio = octave_value (audio);
            }
        }
      else
        {
          ret_audio = octave_value (audio);
        }
    }
  else
    {
      ret_audio = octave_value (audio);
    }
  retval(0) = ret_audio;
  retval(1) = info.samplerate;
#else
  error("sndfile not found on your system and thus audioread is not functional");
#endif
  return octave_value(retval);
}
