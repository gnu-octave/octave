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

#include <string>
#include <map>

#include "oct.h"
#include "ov-struct.h"

#ifdef HAVE_SNDFILE
#include <sndfile.h>
#endif

DEFUN_DLD (audioread, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{y}, @var{fs}] =} audioread (@var{filename})\n\
\n\
Load an audio file that is specified by @var{filename}.  It will be loaded\n\
in to a column matrix with as many rows as there are audio frames and as many\n\
columns as there are channels in the file.  Sampling rate will be stored in\n\
@var{fs}.\n\
\n\
@end deftypefn\n\
@deftypefn {Loadable Function} {[@var{y}, @var{fs}] =} audioread (@var{filename}, @var{samples})\n\
\n\
Read a specified range of samples from a file specified by @var{filename}.\n\
Argument @var{samples} is a vector with two values specifying starting frame\n\
and ending frame.\n\
\n\
@end deftypefn\n\
@deftypefn {Loadable Function} {[@var{y}, @var{fs}] =} audioread (@var{filename}, @var{datatype})\n\
\n\
Read a file and return an array of specified type.  If @var{datatype} is\n\
@qcode{\"native\"} then an array of fixed width integer type will be returned\n\
depending on how data is stored in the audio file.  If @var{datatype} is\n\
@qcode{\"double\"} a double matrix will be returned.\n\
\n\
@end deftypefn\n\
@deftypefn {Loadable Function} {[@var{y}, @var{fs}] =} audioread (@var{filename}, @var{samples}, @var{datatype})\n\
\n\
Read a file and return a specified range of frames in an array of specified type.\n\
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
  file = sf_open (args(0).string_value ().c_str (), SFM_READ, &info);
  start = 0;
  end = info.frames;
  float *data = (float *)malloc (sizeof (float) * info.frames * info.channels);
  sf_read_float (file, data, info.frames * info.channels);
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
        type = args(2).string_value ();
      else
        type = args(1).string_value ();

      if (type == "native")
        {
          if (info.format & SF_FORMAT_PCM_S8)
            ret_audio = octave_value ((audio * 127)).int8_array_value ();
          else if (info.format & SF_FORMAT_PCM_U8)
            ret_audio = octave_value ((audio * 127 + 127)).uint8_array_value ();
          else if (info.format & SF_FORMAT_PCM_16)
            ret_audio = octave_value ((audio * 32767)).int16_array_value ();
          else if (info.format & SF_FORMAT_PCM_24)
            ret_audio = octave_value ((audio * 8388608)).int32_array_value ();
          else if (info.format & SF_FORMAT_PCM_32)
            ret_audio = octave_value ((audio * 2147483648)).int32_array_value ();
          else
            ret_audio = octave_value (audio);
        }
      else
        ret_audio = octave_value (audio);
    }
  else
    ret_audio = octave_value (audio);

  retval(0) = ret_audio;
  retval(1) = info.samplerate;
#else
  error ("sndfile not found on your system and thus audioread is not functional");
#endif
  return octave_value (retval);
}

#ifdef HAVE_SNDFILE
static void
fill_extension_table (std::map<std::string, int> &table)
{
  table["wav"] = SF_FORMAT_WAV;
  table["aiff"] = SF_FORMAT_AIFF;
  table["au"] = SF_FORMAT_AU;
  table["raw"] = SF_FORMAT_RAW;
  table["paf"] = SF_FORMAT_PAF;
  table["svx"] = SF_FORMAT_SVX;
  table["nist"] = SF_FORMAT_NIST;
  table["voc"] = SF_FORMAT_VOC;
  table["ircam"] = SF_FORMAT_IRCAM;
  table["w64"] = SF_FORMAT_W64;
  table["mat4"] = SF_FORMAT_MAT4;
  table["mat5"] = SF_FORMAT_MAT5;
  table["pvf"] = SF_FORMAT_PVF;
  table["xi"] = SF_FORMAT_XI;
  table["htk"] = SF_FORMAT_HTK;
  table["sds"] = SF_FORMAT_SDS;
  table["avr"] = SF_FORMAT_AVR;
  table["wavex"] = SF_FORMAT_WAVEX;
  table["sd2"] = SF_FORMAT_SD2;
  table["flac"] = SF_FORMAT_FLAC;
  table["caf"] = SF_FORMAT_CAF;
  table["wve"] = SF_FORMAT_WVE;
  table["ogg"] = SF_FORMAT_OGG;
  table["mpc2k"] = SF_FORMAT_MPC2K;
  table["rf64"] = SF_FORMAT_RF64;
}
#endif

DEFUN_DLD (audiowrite, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} audiowrite (@var{filename}, @var{y}, @var{fs})\n\
\n\
Write audio data from the matrix @var{y} to a file specified by @var{filename},\n\
file format will be determined by the file extension.\n\
\n\
@end deftypefn\n\
@deftypefn {Loadable Function} {} audiowrite (@var{filename}, @var{y}, @var{fs}, @var{name}, @var{value})\n\
\n\
Lets you specify additional parameters when writing the file. Those parameters\n\
are given in the table below:\n\
\n\
@table @samp\n\
@item BitsPerSample\n\
Number of bits per sample, valid values are 8, 16, 24 and 32. Default is 16.\n\
@item BitRate\n\
Valid argument name, but ignored. Left for compatibility with MATLAB.\n\
@item Quality\n\
Quality setting for the Ogg Vorbis compressor. Values can range between 0 and 100 with 100 being the highest quality setting. Default is 75.\n\
@item Title\n\
Title for the audio file.\n\
@item Artist\n\
Artist name.\n\
@item Comment\n\
Comment.\n\
@end table\n\
@end deftypefn")
{
  octave_scalar_map retval;
#ifdef HAVE_SNDFILE
  std::map<std::string, int> extension_to_format;
  fill_extension_table (extension_to_format);
  std::string filename = args(0).string_value ();
  std::string extension = filename.substr (filename.find_last_of (".") + 1);
  std::transform (extension.begin (), extension.end (), extension.begin (), ::tolower);
  Matrix audio = args(1).matrix_value ();
  SNDFILE *file;
  SF_INFO info;
  float *data = (float *)malloc (audio.rows () * audio.cols () * sizeof (float));
  for (int i = 0; i < audio.cols (); i++)
    {
      for (int j = 0; j < audio.rows (); j++)
        {
          data[j * audio.cols () + i] = audio(j, i);
        }
    }

  if (extension == "ogg")
    info.format = SF_FORMAT_VORBIS;
  else
    info.format = SF_FORMAT_PCM_16;

  std::string title = "";
  std::string artist = "";
  std::string comment = "";
  float quality = 0.75;
  for (int i = 3; i < args.length (); i += 2)
    {
      if (args(i).string_value () == "BitsPerSample")
        {
          int bits = args(i + 1).int_value ();
          if (bits == 8)
            info.format |= SF_FORMAT_PCM_S8;
          else if (bits == 16)
            info.format |= SF_FORMAT_PCM_16;
          else if (bits == 24)
            info.format |= SF_FORMAT_PCM_24;
          else if (bits == 32)
            info.format |= SF_FORMAT_PCM_32;
          else
            error ("audiowrite: wrong number of bits specified");
        }
      else if (args(i).string_value () == "BitRate")
        ;
      else if (args(i).string_value () == "Quality")
        quality = args(i + 1).int_value () * 0.01;
      else if (args(i).string_value () == "Title")
        title = args(i + 1).string_value ();
      else if (args(i).string_value () == "Artist")
        artist = args(i + 1).string_value ();
      else if (args(i).string_value () == "Comment")
        comment = args(i + 1).string_value ();
      else
        error ("audiowrite: wrong argument name");
    }
  info.samplerate = args(2).int_value ();
  info.channels = audio.cols ();
  info.format |= extension_to_format[extension];
  file = sf_open (filename.c_str (), SFM_WRITE, &info);
  if (title != "")
    sf_set_string (file, SF_STR_TITLE, title.c_str ());
  if (artist != "")
    sf_set_string (file, SF_STR_ARTIST, artist.c_str ());
  if (comment != "")
    sf_set_string (file, SF_STR_COMMENT, comment.c_str ());
  sf_write_float (file, data, audio.rows () * audio.cols ());
  sf_close (file);
  free (data);
#else
  error ("sndfile not found on your system and thus audiowrite is not functional");
#endif
  return octave_value (retval);
}

DEFUN_DLD (audioinfo, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{info} =} audioinfo (@var{filename})\n\
Return information about an audio file specified by @var{filename}.\n\
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
