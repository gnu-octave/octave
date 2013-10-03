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

DEFUN_DLD (audioinfo, args, ,
"-*- texinfo -*-\n\
@deftypefn{Loadable Function} info = audioinfo (@var{filename})\n\
\n\
Return information about an audio file specified by @var{filename}.\
\n\
@end deftypefn")
{
  octave_scalar_map retval;
  if (args.length () != 1 || not args(0).is_string ())
    {
      print_usage ();
      return octave_value (retval);
    }
#ifdef HAVE_SNDFILE
  Matrix audio;
  SNDFILE *file;
  SF_INFO info;
  info.format = 0;
  int start, end;
  file = sf_open (args(0).string_value ().c_str (), SFM_READ, &info);
  retval.assign ("Filename", args(0).string_value ());
  retval.assign ("CompressionMethod", "");
  retval.assign ("NumChannels", info.channels);
  retval.assign ("SampleRate", info.samplerate);
  retval.assign ("TotalSamples", info.frames);
  retval.assign ("Duration", (float)info.frames / (float)info.samplerate);

  int bits;
  if (info.format & SF_FORMAT_PCM_S8)
    bits = 8;
  else if (info.format & SF_FORMAT_PCM_U8)
    bits = 8;
  else if (info.format & SF_FORMAT_PCM_16)
    bits = 16;
  else if (info.format & SF_FORMAT_PCM_24)
    bits = 24;
  else if (info.format & SF_FORMAT_PCM_32)
    bits = 32;
  else
    bits = -1;

  retval.assign ("BitsPerSample", bits);
  retval.assign ("BitRate", -1);
  retval.assign ("Title", sf_get_string (file, SF_STR_TITLE));
  retval.assign ("Artist", sf_get_string (file, SF_STR_ARTIST));
  retval.assign ("Comment", sf_get_string (file, SF_STR_COMMENT));
#else
  error ("sndfile not found on your system and thus audioinfo is not functional");
#endif
  return octave_value (retval);
}
