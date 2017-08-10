/*

Copyright (C) 2013-2017 Vytautas Jančauskas
Copyright (C) 2016 Damjan Angelovski

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <algorithm>
#include <map>
#include <string>

#include "dMatrix.h"
#include "dRowVector.h"
#include "oct-locbuf.h"
#include "unwind-prot.h"

#include "defun-dld.h"
#include "error.h"
#include "errwarn.h"
#include "ov.h"
#include "ovl.h"
#include "pager.h"

#if defined (HAVE_SNDFILE)
#  include <sndfile.h>
#endif

#if defined (HAVE_SNDFILE)
static void
safe_close (SNDFILE *file)
{
  sf_close (file);
}
#endif

DEFUN_DLD (audioread, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{y}, @var{fs}] =} audioread (@var{filename})
@deftypefnx {} {[@var{y}, @var{fs}] =} audioread (@var{filename}, @var{samples})

@deftypefnx {} {[@var{y}, @var{fs}] =} audioread (@var{filename}, @var{datatype})
@deftypefnx {} {[@var{y}, @var{fs}] =} audioread (@var{filename}, @var{samples}, @var{datatype})
Read the audio file @var{filename} and return the audio data @var{y} and
sampling rate @var{fs}.

The audio data is stored as matrix with rows corresponding to audio frames
and columns corresponding to channels.

The optional two-element vector argument @var{samples} specifies starting
and ending frames.

The optional argument @var{datatype} specifies the datatype to return.
If it is @qcode{"native"}, then the type of data depends on how the data
is stored in the audio file.
@seealso{audiowrite, audioformats, audioinfo}
@end deftypefn */)
{
#if defined (HAVE_SNDFILE)

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  std::string filename = args(0).xstring_value ("audioread: FILENAME must be a string");

  SF_INFO info;
  info.format = 0;
  SNDFILE *file = sf_open (filename.c_str (), SFM_READ, &info);

  if (! file)
    error ("audioread: failed to open input file %s", filename.c_str ());

  octave::unwind_protect frame;

  frame.add_fcn (safe_close, file);

  OCTAVE_LOCAL_BUFFER (float, data, info.frames * info.channels);

  sf_read_float (file, data, info.frames * info.channels);

  sf_count_t start = 0;
  sf_count_t end = info.frames;

  if ((nargin == 2 && ! args(1).is_string ()) || nargin == 3)
    {
      RowVector range = args(1).row_vector_value ();

      if (range.numel () != 2)
        error ("audioread: invalid specification for range of frames");

      double dstart = (octave::math::isinf (range(0)) ? info.frames : range(0));
      double dend = (octave::math::isinf (range(1)) ? info.frames : range(1));

      if (dstart < 1 || dstart > dend || dend > info.frames
          || octave::math::x_nint (dstart) != dstart
          || octave::math::x_nint (dend) != dend)
        error ("audioread: invalid specification for range of frames");

      start = dstart - 1;
      end = dend;
    }

  sf_count_t items = end - start;

  Matrix audio (items, info.channels);

  double *paudio = audio.fortran_vec ();

  data += start * info.channels;

  for (int i = 0; i < items; i++)
    {
      for (int channel = 0; channel < info.channels; channel++)
        paudio[items*channel+i] = *data++;
    }

  octave_value ret_audio;

  if ((nargin == 2 && args(1).is_string ()) || nargin == 3)
    {
      std::string type;
      if (nargin == 3)
        type = args(2).string_value ();
      else
        type = args(1).string_value ();

      if (type == "native")
        {
          switch (info.format & SF_FORMAT_SUBMASK)
            {
            case SF_FORMAT_PCM_S8:
              ret_audio = int8NDArray (audio * 127);
              break;
            case SF_FORMAT_PCM_U8:
              ret_audio = uint8NDArray (audio * 127 + 127);
              break;
            case SF_FORMAT_PCM_16:
              ret_audio = int16NDArray (audio * 32767);
              break;
            case SF_FORMAT_PCM_24:
              ret_audio = int32NDArray (audio * 8388608);
              break;
            case SF_FORMAT_PCM_32:
              ret_audio = int32NDArray (audio * 2147483648);
              break;
            default:
              ret_audio = audio;
              break;
            }
        }
      else
        ret_audio = audio;
    }
  else
    ret_audio = audio;

  return ovl (ret_audio, info.samplerate);

#else

  octave_unused_parameter (args);

  err_disabled_feature ("audioread",
                        "reading and writing sound files through libsndfile");

#endif
}

#if defined (HAVE_SNDFILE)

static int
extension_to_format (const std::string& ext)
{
  static bool initialized = false;

  static std::map<std::string, int> table;

  if (! initialized)
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

      initialized = true;
    }

  std::map<std::string, int>::const_iterator it = table.find (ext);

  return (it != table.end ()) ? it->second : 0;
}

#endif

DEFUN_DLD (audiowrite, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} audiowrite (@var{filename}, @var{y}, @var{fs})
@deftypefnx {} {} audiowrite (@var{filename}, @var{y}, @var{fs}, @var{name}, @var{value}, @dots{})

Write audio data from the matrix @var{y} to @var{filename} at sampling rate
@var{fs} with the file format determined by the file extension.

Additional name/value argument pairs may be used to specify the
following options:

@table @samp
@item BitsPerSample
Number of bits per sample.  Valid values are 8, 16, 24, and 32.  Default is
16.

@item BitRate
Valid argument name, but ignored.  Left for compatibility with @sc{matlab}.

@item Quality
Quality setting for the Ogg Vorbis compressor.  Values can range between 0
and 100 with 100 being the highest quality setting.  Default is 75.

@item Title
Title for the audio file.

@item Artist
Artist name.

@item Comment
Comment.
@end table
@seealso{audioread, audioformats, audioinfo}
@end deftypefn */)
{
#if defined (HAVE_SNDFILE)

  int nargin = args.length ();

  if (nargin < 3)
    print_usage ();

  std::string filename = args(0).xstring_value ("audiowrite: FILENAME must be a string");

  double bias = 0.0;
  double scale = 1.0;

  if (args(1).is_uint8_type ())
    bias = scale = std::pow (2.0, 7);
  else if (args(1).is_int16_type ())
    scale = std::pow (2.0, 15);
  else if (args(1).is_int32_type ())
    scale = std::pow (2.0, 31);
  else if (args(1).isinteger ())
    err_wrong_type_arg ("audiowrite", args(1));

  Matrix audio = args(1).matrix_value ();

  int samplerate = args(2).int_value ();

  std::string ext;
  size_t dotpos = filename.find_last_of ('.');
  if (dotpos != std::string::npos)
    ext = filename.substr (dotpos + 1);
  std::transform (ext.begin (), ext.end (), ext.begin (), ::tolower);

  sf_count_t items_to_write = audio.rows () * audio.columns ();

  if (audio.rows () == 1)
    audio = audio.transpose ();

  OCTAVE_LOCAL_BUFFER (float, data, items_to_write);

  sf_count_t idx = 0;
  for (int i = 0; i < audio.rows (); i++)
    {
      for (int j = 0; j < audio.columns (); j++)
        {
          double elem = (audio.xelem (i, j) - bias) / scale;
          data[idx++] = std::min (std::max (elem, -1.0), 1.0);
        }
    }

  SF_INFO info;

  memset (&info, 0, sizeof (info));

  sf_count_t chunk_size = 0;

  if (ext == "ogg")
    {
      info.format = SF_FORMAT_VORBIS;

      // FIXME: there seems to be a bug writing ogg files in one shot
      // that causes a segfault.  Breaking it up into a series of
      // smaller chunks seems to avoid the problem and produce valid
      // files.
      chunk_size = 0x1FFFFE;
    }
  else
    info.format = SF_FORMAT_PCM_16;

  info.channels = audio.columns ();
  info.samplerate = samplerate;
  info.channels = audio.cols ();
  info.format |= extension_to_format (ext);

  std::string title = "";
  std::string artist = "";
  std::string comment = "";
  double quality = 0.75;

  for (int i = 3; i < nargin; i += 2)
    {
      if (i >= nargin - 1)
        error ("audiowrite: invalid number of arguments");

      std::string keyword_orig = args(i).string_value ();
      std::string keyword = args(i).xtolower ().string_value ();
      octave_value value_arg = args(i+1);

      if (keyword == "bitspersample")
        {
          info.format &= ~SF_FORMAT_SUBMASK;
          int bits = value_arg.int_value ();
          if (bits == 8)
            {
              if ((info.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV)
                info.format |= SF_FORMAT_PCM_U8;
              else
                info.format |= SF_FORMAT_PCM_S8;
            }
          else if (bits == 16)
            info.format |= SF_FORMAT_PCM_16;
          else if (bits == 24)
            info.format |= SF_FORMAT_PCM_24;
          else if (bits == 32)
            info.format |= SF_FORMAT_PCM_32;
          else
            error ("audiowrite: wrong number of bits specified");
        }
      else if (keyword == "bitrate")
        warning_with_id ("Octave:audiowrite:unused-parameter",
                         "audiowrite: 'BitRate' accepted for Matlab "
                         "compatibility, but is ignored");
      else if (keyword == "quality")
        {
          double value = value_arg.xdouble_value ("audiowrite: Quality value must be a scalar");

          if (octave::math::isnan (value) || value < 0 || value > 100)
            error ("audiowrite: Quality must be a number between 0 and 100");

          quality = value / 100;
        }
      else if (keyword == "title")
        title = value_arg.string_value ();
      else if (keyword == "artist")
        artist = value_arg.string_value ();
      else if (keyword == "comment")
        comment = value_arg.string_value ();
      else
        error ("audiowrite: unrecognized option: '%s'", keyword_orig.c_str ());
    }

  SNDFILE *file = sf_open (filename.c_str (), SFM_WRITE, &info);

  if (! file)
    error ("audiowrite: failed to open output file %s", filename.c_str ());

  octave::unwind_protect frame;

  frame.add_fcn (safe_close, file);

  sf_command (file, SFC_SET_VBR_ENCODING_QUALITY, &quality, sizeof (quality));

  if (title != "")
    sf_set_string (file, SF_STR_TITLE, title.c_str ());

  if (artist != "")
    sf_set_string (file, SF_STR_ARTIST, artist.c_str ());

  if (comment != "")
    sf_set_string (file, SF_STR_COMMENT, comment.c_str ());

  sf_count_t total_items_written = 0;
  sf_count_t offset = 0;

  if (chunk_size == 0)
    chunk_size = items_to_write;

  while (total_items_written < items_to_write)
    {
      if (items_to_write - offset < chunk_size)
        chunk_size = items_to_write - offset;

      sf_count_t items_written = sf_write_float (file, data+offset, chunk_size);

      if (items_written != chunk_size)
        error ("audiowrite: write failed, wrote %ld of %ld items\n",
               items_written, chunk_size);

      total_items_written += items_written;
      offset += chunk_size;
    }

  // FIXME: shouldn't we return something to indicate whether the file
  // was written successfully?
  return ovl ();

#else

  octave_unused_parameter (args);

  err_disabled_feature ("audiowrite",
                        "reading and writing sound files through libsndfile");

#endif
}

DEFUN_DLD (audioinfo, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{info} =} audioinfo (@var{filename})
Return information about an audio file specified by @var{filename}.

The output @var{info} is a structure containing the following fields:

@table @samp
@item Filename
Name of the audio file.

@item CompressionMethod
Audio compression method.  Unused, only present for compatibility with
@sc{matlab}.

@item NumChannels
Number of audio channels.

@item SampleRate
Sample rate of the audio, in Hertz.

@item TotalSamples
Number of samples in the file.

@item Duration
Duration of the audio, in seconds.

@item BitsPerSample
Number of bits per sample.

@item BitRate
Audio bitrate.  Unused, only present for compatibility with @sc{matlab}.

@item Title
@qcode{"Title"} audio metadata value as a string, or empty if not present.

@item Artist
@qcode{"Artist"} audio metadata value as a string, or empty if not present.

@item Comment
@qcode{"Comment"} audio metadata value as a string, or empty if not present.
@end table
@seealso{audioread, audiowrite}
@end deftypefn */)
{
#if defined (HAVE_SNDFILE)

  if (args.length () != 1)
    print_usage ();

  std::string filename = args(0).xstring_value ("audioinfo: FILENAME must be a string");

  SF_INFO info;
  info.format = 0;
  SNDFILE *file = sf_open (filename.c_str (), SFM_READ, &info);

  if (! file)
    error ("audioinfo: failed to open file %s", filename.c_str ());

  octave::unwind_protect frame;

  frame.add_fcn (safe_close, file);

  octave_scalar_map result;

  result.assign ("Filename", filename);
  result.assign ("CompressionMethod", "");
  result.assign ("NumChannels", info.channels);
  result.assign ("SampleRate", info.samplerate);
  result.assign ("TotalSamples", info.frames);

  double dframes = info.frames;
  double drate = info.samplerate;
  result.assign ("Duration", dframes / drate);

  int bits;
  switch (info.format & SF_FORMAT_SUBMASK)
    {
    case SF_FORMAT_PCM_S8:
      bits = 8;
      break;
    case SF_FORMAT_PCM_U8:
      bits = 8;
      break;
    case SF_FORMAT_PCM_16:
      bits = 16;
      break;
    case SF_FORMAT_PCM_24:
      bits = 24;
      break;
    case SF_FORMAT_PCM_32:
      bits = 32;
      break;
    default:
      bits = -1;
      break;
    }

  result.assign ("BitsPerSample", bits);
  result.assign ("BitRate", -1);
  result.assign ("Title", sf_get_string (file, SF_STR_TITLE));
  result.assign ("Artist", sf_get_string (file, SF_STR_ARTIST));
  result.assign ("Comment", sf_get_string (file, SF_STR_COMMENT));

  return ovl (result);

#else

  octave_unused_parameter (args);

  err_disabled_feature ("audioinfo",
                        "reading and writing sound files through libsndfile");

#endif
}

#if defined (HAVE_SNDFILE)

static void
audio_sub_formats (int format)
{
  int count;
  sf_command (nullptr, SFC_GET_FORMAT_SUBTYPE_COUNT, &count, sizeof (int));

  for (int i = 0; i < count; i++)
    {
      SF_FORMAT_INFO info;
      info.format = i;
      sf_command (nullptr, SFC_GET_FORMAT_SUBTYPE, &info, sizeof (info));

      SF_INFO sfinfo;
      memset (&sfinfo, 0, sizeof (sfinfo));
      sfinfo.channels = 1;
      sfinfo.format = (format & SF_FORMAT_TYPEMASK) | info.format;

      if (sf_format_check (&sfinfo))
        octave_stdout << "  " << info.name << std::endl;
    }
}

#endif

DEFUN_DLD (audioformats, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} audioformats ()
@deftypefnx {} {} audioformats (@var{format})
Display information about all supported audio formats.

If the optional argument @var{format} is given, then display only formats
with names that start with @var{format}.
@seealso{audioread, audiowrite}
@end deftypefn */)
{
#if defined (HAVE_SNDFILE)

  if (args.length () > 1)
    print_usage ();

  std::string search = "";
  if (args.length () > 0)
    {
      search = args(0).string_value ();
      std::transform (search.begin (), search.end (), search.begin (), tolower);
    }

  int count;
  sf_command (nullptr, SFC_GET_FORMAT_MAJOR_COUNT, &count, sizeof (int));

  for (int i = 0; i < count; i++)
    {
      SF_FORMAT_INFO info;
      info.format = i;
      sf_command (nullptr, SFC_GET_FORMAT_MAJOR, &info, sizeof (info));
      bool match = true;

      if (! search.empty ())
        {
          std::string nm = info.name;
          std::transform (nm.begin (), nm.end (), nm.begin (), tolower);
          match = nm.compare (0, search.length (), search) == 0;
        }

      if (match)
        {
          octave_stdout << "name: " << info.name << std::endl;
          octave_stdout << "extension: " << info.extension << std::endl;
          octave_stdout << "id: " << info.format << std::endl;
          octave_stdout << "subformats:" << std::endl;

          audio_sub_formats (info.format);
        }
    }

#else

  octave_unused_parameter (args);

  err_disabled_feature ("audioformats",
                        "getting sound formats through libsndfile");

#endif

  return octave_value_list ();
}
