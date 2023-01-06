////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdint>

#include <algorithm>
#include <ostream>
#include <string>
#include <vector>

#include "Matrix.h"
#include "mach-info.h"
#include "oct-locbuf.h"
#include "quit.h"
#include "unwind-prot.h"

#include "Cell.h"
#include "defun-dld.h"
#include "error.h"
#include "errwarn.h"
#include "oct-map.h"
#include "ov-int32.h"
#include "ov.h"
#include "ovl.h"
#include "parse.h"

#if defined (HAVE_PORTAUDIO)
#  include <portaudio.h>
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

#if defined (HAVE_PORTAUDIO)

static PaSampleFormat
bits_to_format (int bits)
{
  if (bits == 8)
    return paInt8;
  else if (bits == 16)
    return paInt16;
  else if (bits == 24)
    return paInt24;
  else if (bits == 32)
    return paInt32;
  else if (bits == -1)
    return paFloat32;
  else
    return 0;
}

#endif

DEFUN_DLD (audiodevinfo, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{devinfo} =} audiodevinfo ()

@deftypefnx {} {@var{devs} =} audiodevinfo (@var{io})
@deftypefnx {} {@var{name} =} audiodevinfo (@var{io}, @var{id})
@deftypefnx {} {@var{id} =} audiodevinfo (@var{io}, @var{name})
@deftypefnx {} {@var{driverversion} =} audiodevinfo (@var{io}, @var{id}, "DriverVersion")
@deftypefnx {} {@var{id} =} audiodevinfo (@var{io}, @var{rate}, @var{bits}, @var{chans})
@deftypefnx {} {@var{supports} =} audiodevinfo (@var{io}, @var{id}, @var{rate}, @var{bits}, @var{chans})

Return a structure describing the available audio input and output devices.

The @var{devinfo} structure has two fields @qcode{"input"} and
@qcode{"output"}.  The value of each field is a structure array with fields
@qcode{"Name"}, @nospell{@qcode{"DriverVersion"}} and @qcode{"ID"} describing
an audio device.

If the optional argument @var{io} is 1, return information about input devices
only.  If it is 0, return information about output devices only.  If @var{io}
is the only argument supplied, return the number of input or output devices
available.

If the optional argument @var{id} is provided, return information about the
corresponding device.

If the optional argument @var{name} is provided, return the ID of the named
device.

If the optional argument @nospell{@qcode{"DriverVersion"}} is given, return the
name of the driver for the specified device.

Given a sampling rate, bits per sample, and number of channels for an input or
output device, return the ID of the first device that supports playback or
recording using the specified parameters.

If also given a device ID, return true if the device supports playback or
recording using those parameters.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)

  int nargin = args.length ();

  if (nargin > 5)
    print_usage ();

  octave_scalar_map devinfo;
  octave_value_list input;
  octave_value_list output;

  PaError err = Pa_Initialize ();

  if (err != paNoError)
    error ("audiodevinfo: PortAudio initialization failed");

  int num_devices = Pa_GetDeviceCount ();

  if (num_devices < 0)
    num_devices = 0;

  octave_idx_type numinput, numoutput;
  numinput = numoutput = 0;
  for (int i = 0; i < num_devices; i++)
    {
      const PaDeviceInfo *device_info = Pa_GetDeviceInfo (i);

      if (! device_info)
        {
          warning_with_id ("Octave:invalid-audio-device",
                           "invalid audio device ID = %d", i);
          continue;
        }

      if (device_info->maxInputChannels != 0)
        numinput++;

      if (device_info->maxOutputChannels != 0)
        numoutput++;
    }

  Cell input_name (dim_vector (1, numinput));
  Cell input_driver_version (dim_vector (1, numinput));
  Cell input_id (dim_vector (1, numinput));
  Cell output_name (dim_vector (1, numoutput));
  Cell output_driver_version (dim_vector (1, numoutput));
  Cell output_id (dim_vector (1, numoutput));

  octave_idx_type idx_i, idx_o;
  idx_i = idx_o = 0;
  for (int i = 0; i < num_devices; i++)
    {
      const PaDeviceInfo *device_info = Pa_GetDeviceInfo (i);

      if (! device_info)
        {
          warning_with_id ("Octave:invalid-audio-device",
                           "invalid audio device ID = %d", i);
          continue;
        }

      const PaHostApiInfo *api_info = Pa_GetHostApiInfo (device_info->hostApi);

      const char *driver = (api_info ? api_info->name : "");

      char name[128];
      sprintf (name, "%s (%s)", device_info->name, driver);

      if (device_info->maxInputChannels != 0)
        {
          input_name(idx_i) = name;
          input_driver_version(idx_i) = driver;
          input_id(idx_i) = i;
          idx_i++;
        }

      if (device_info->maxOutputChannels != 0)
        {
          output_name(idx_o) = name;
          output_driver_version(idx_o) = driver;
          output_id(idx_o) = i;
          idx_o++;
        }
    }

  octave_map inputdev, outputdev;
  inputdev.setfield ("Name", input_name);
  inputdev.setfield ("DriverVersion", input_driver_version);
  inputdev.setfield ("ID", input_id);
  outputdev.setfield ("Name", output_name);
  outputdev.setfield ("DriverVersion", output_driver_version);
  outputdev.setfield ("ID", output_id);
  devinfo.setfield ("input", inputdev);
  devinfo.setfield ("output", outputdev);

  octave_value retval;

  // Return information about input & output audio devices and their properties.
  if (nargin == 0)
    {
      retval = devinfo;
    }
  // Return the number of input or output devices
  else if (nargin == 1)
    {
      if (args(0).int_value () == 0)
        retval = numoutput;
      else if (args(0).int_value () == 1)
        retval = numinput;
      else
        error ("audiodevinfo: specify 0 for output and 1 for input devices");
    }
  // Return device name when given id or id when given device name.
  else if (nargin == 2)
    {
      bool found = false;
      int outin = args(0).int_value ();
      if (args(1).is_string ())
        {
          std::string name = args(1).string_value ();
          if (outin == 0)
            {
              for (int i = 0; i < numoutput; i++)
                {
                  if (output_name(i).string_value () == name)
                    {
                      retval = output_id(i);
                      found = true;
                      break;
                    }
                }
            }
          else if (outin == 1)
            {
              for (int i = 0; i < numinput; i++)
                {
                  if (input_name(i).string_value () == name)
                    {
                      retval = input_id(i);
                      found = true;
                      break;
                    }
                }
            }
          else
            error ("audiodevinfo: specify 0 for output and 1 for input devices");
        }
      else
        {
          if (outin == 0)
            {
              for (int i = 0; i < numoutput; i++)
                {
                  if (output_id(i).int_value () == args(1).int_value ())
                    {
                      retval = output_name(i);
                      found = true;
                      break;
                    }
                }
            }
          else if (outin == 1)
            {
              for (int i = 0; i < numinput; i++)
                {
                  if (input_id(i).int_value () == args(1).int_value ())
                    {
                      retval = input_name(i);
                      found = true;
                      break;
                    }
                }
            }
          else
            error ("audiodevinfo: specify 0 for output and 1 for input devices");
        }

      if (! found)
        error ("audiodevinfo: no device found for the specified criteria");
    }
  // Return the DriverVersion (really, name of driver) of the specified device
  else if (nargin == 3)
    {
      bool found = false;
      int outin = args(0).int_value ();
      int id = args(1).int_value ();

      std::string arg3 = args(2).string_value ();
      std::transform (arg3.begin (), arg3.end (), arg3.begin (), tolower);
      if (arg3 != "driverversion")
        error (R"(audiodevinfo: third argument must be "DriverVersion")");

      if (outin == 0)
        {
          for (int i = 0; i < numoutput; i++)
            {
              if (output_id(i).int_value () == id)
                {
                  found = true;
                  retval = output_driver_version(i);
                  break;
                }
            }
        }
      else if (outin == 1)
        {
          for (int i = 0; i < numinput; i++)
            {
              if (input_id(i).int_value () == id)
                {
                  found = true;
                  retval = input_driver_version(i);
                  break;
                }
            }
        }
      else
        error ("audiodevinfo: specify 0 for output and 1 for input devices");

      if (! found)
        error ("audiodevinfo: no device found for the specified criteria");
    }
  // Return the ID of the first device meeting specified criteria.
  else if (nargin == 4)
    {
      int io = args(0).int_value ();
      int rate = args(1).int_value ();
      int bits = args(2).int_value ();
      int chans = args(3).int_value ();

      for (int i = 0; i < num_devices; i++)
        {
          PaStreamParameters stream_parameters;
          stream_parameters.device = i;
          stream_parameters.channelCount = chans;
          PaSampleFormat format = bits_to_format (bits);

          if (format != 0)
            stream_parameters.sampleFormat = format;
          else
            error ("audiodevinfo: invalid bits per sample format");

          const PaDeviceInfo *device_info = Pa_GetDeviceInfo (i);

          if (! device_info)
            {
              warning_with_id ("Octave:invalid-audio-device",
                               "invalid audio device ID = %d", i);
              continue;
            }

          stream_parameters.suggestedLatency
            = device_info->defaultLowInputLatency;

          stream_parameters.hostApiSpecificStreamInfo = nullptr;

          if (io == 0)
            {
              if (device_info->maxOutputChannels < chans)
                continue;

              err = Pa_IsFormatSupported (nullptr, &stream_parameters, rate);

              if (err == paFormatIsSupported)
                {
                  retval = i;
                  return retval;
                }
            }
          else if (io == 1)
            {
              if (device_info->maxInputChannels < chans)
                continue;

              err = Pa_IsFormatSupported (&stream_parameters, nullptr, rate);
              if (err == paFormatIsSupported)
                {
                  retval = i;
                  return retval;
                }
            }
        }
      retval = -1;
    }
  // Check if given device supports specified playback or recording modes.
  else if (nargin == 5)
    {
      int io = args(0).int_value ();
      int id = args(1).int_value ();
      int rate = args(2).int_value ();
      int bits = args(3).int_value ();
      int chans = args(4).int_value ();
      PaStreamParameters stream_parameters;
      stream_parameters.device = id;
      stream_parameters.channelCount = chans;
      PaSampleFormat format = bits_to_format (bits);
      if (format != 0)
        stream_parameters.sampleFormat = format;
      else
        error ("audiodevinfo: invalid bits per sample format");

      const PaDeviceInfo *device_info = Pa_GetDeviceInfo (id);

      if (! device_info)
        error ("audiodevinfo: invalid audio device ID = %d", id);

      stream_parameters.suggestedLatency
        = device_info->defaultLowInputLatency;

      stream_parameters.hostApiSpecificStreamInfo = nullptr;
      if (io == 0)
        {
          if (device_info->maxOutputChannels < chans)
            {
              retval = false;
              return retval;
            }
          err = Pa_IsFormatSupported (nullptr, &stream_parameters, rate);
          if (err == paFormatIsSupported)
            {
              retval = true;
              return retval;
            }
        }
      else if (io == 1)
        {
          if (device_info->maxInputChannels < chans)
            {
              retval = false;
              return retval;
            }
          err = Pa_IsFormatSupported (&stream_parameters, nullptr, rate);
          if (err == paFormatIsSupported)
            {
              retval = true;
              return retval;
            }
        }
      else
        error ("audiodevinfo: specify 0 for output and 1 for input devices");

      retval = false;
    }

  return retval;

#else
  octave_unused_parameter (args);

  err_disabled_feature ("audiodevinfo",
                        "audio playback and recording through PortAudio");
#endif
}

/*
%!testif HAVE_PORTAUDIO
%! devinfo = audiodevinfo;
%! assert (rows (devinfo.input), 1);
%! assert (rows (devinfo.output), 1);

%!testif HAVE_PORTAUDIO
%! devinfo = audiodevinfo;
%! nout = audiodevinfo (0);
%! nin = audiodevinfo (1);
%! assert (columns (devinfo.output), nout);
%! assert (columns (devinfo.input), nin);

%!testif HAVE_PORTAUDIO
%! devinfo = audiodevinfo;
%! nout = audiodevinfo (0);
%! nin = audiodevinfo (1);
%! for i = 1:nout
%!   assert (devinfo.output(i).Name, audiodevinfo (0, devinfo.output(i).ID));
%! endfor
%! for i=1:nin
%!   assert (devinfo.input (i).Name, audiodevinfo (1, devinfo.input (i).ID));
%! endfor

%!testif HAVE_PORTAUDIO
%! devinfo = audiodevinfo;
%! nout = audiodevinfo (0);
%! nin = audiodevinfo (1);
%! ## There might be multiple devices with the same name
%! ## (e.g., on Windows WDM-KS)
%! ## Check only the first of each unique device name.
%! [unq_out_name, idx_unique] = unique ({devinfo.output(:).Name});
%! unq_out_id = [devinfo.output(idx_unique).ID];
%! for i = 1:numel (unq_out_name)
%!   assert (audiodevinfo (0, unq_out_name{i}), unq_out_id(i));
%! endfor
%! [unq_in_name, idx_unique] = unique ({devinfo.input(:).Name});
%! unq_in_id = [devinfo.input(idx_unique).ID];
%! for i = 1:numel (unq_in_name)
%!   assert (audiodevinfo (1, unq_in_name{i}), unq_in_id(i));
%! endfor
*/

#if defined (HAVE_PORTAUDIO)

enum audio_type { TYPE_INT8, TYPE_UINT8, TYPE_UINT16, TYPE_DOUBLE };

class audioplayer : public octave_base_dld_value
{
public:
  audioplayer (void);
  ~audioplayer (void);

  // Overloaded base functions
  double player_value (void) const { return 0; }
  virtual double scalar_value (bool = false) const { return 0; }
  void print (std::ostream& os, bool pr_as_read_syntax = false);
  void print_raw (std::ostream& os, bool pr_as_read_syntax) const;

  // Properties
  bool is_constant (void) const { return true; }
  bool is_defined (void) const { return true; }
  bool print_as_scalar (void) const { return true; }

  void init (void);
  void init_fn (void);
  void set_y (const octave_value& y);
  void set_y (octave_function *fcn);
  void set_y (std::string fcn);
  Matrix& get_y (void);
  RowVector get_left (void) const;
  RowVector get_right (void) const;
  void set_fs (int fs);
  int get_fs (void);
  void set_nbits (int nbits);
  int get_nbits (void);
  void set_id (int id);
  int get_id (void);
  int get_channels (void);
  audio_type get_type (void);

  void set_sample_number (unsigned int sample);
  unsigned int get_sample_number (void);
  unsigned int get_total_samples (void);
  void set_end_sample (unsigned int sample);
  unsigned int get_end_sample (void);
  void reset_end_sample (void);
  void set_tag (const charMatrix& tag);
  charMatrix get_tag (void);
  void set_userdata (const octave_value& userdata);
  octave_value get_userdata (void);
  PaStream * get_stream (void);

  void play (void);
  void playblocking (void);
  void pause (void);
  void resume (void);
  void stop (void);
  bool isplaying (void);

  octave_function *octave_callback_function;

private:
  int id;
  int fs;
  int nbits;
  int channels;
  unsigned int sample_number;
  unsigned int end_sample;
  charMatrix tag;
  Matrix y;
  octave_value userdata;
  RowVector left;
  RowVector right;
  PaStream *stream;
  PaStreamParameters output_parameters;
  audio_type type;

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (audioplayer, "audioplayer", "audioplayer");

static int
octave_play_callback (const void *, void *output, unsigned long frames,
                      const PaStreamCallbackTimeInfo *,
                      PaStreamCallbackFlags, void *data)
{
  audioplayer *player = static_cast<audioplayer *> (data);

  if (! player)
    error ("audioplayer callback function called without player");

  octave_value_list retval
    = feval (player->octave_callback_function,
             ovl (static_cast<double> (frames)), 1);

  if (retval.length () < 2)
    error ("audioplayer callback function failed");

  const Matrix sound = retval(0).matrix_value ();
  int return_status = retval(1).int_value ();

  if (frames - sound.rows () != 0 || sound.columns () < 1
      || sound.columns () > 2)
    error ("audioplayer callback function failed");

  // Don't multiply the audio data by scale_factor here.  Although it
  // does move the operation outside of the loops below, it also causes
  // a second copy of the data array to be made.

  const ColumnVector sound_l = sound.column (0);
  const ColumnVector sound_r = (sound.columns () == 1
                                ? sound_l : sound.column (1));

  const double *p_l = sound_l.data ();
  const double *p_r = sound_r.data ();

  switch (player->get_nbits ())
    {
    case 8:
      {
        static double scale_factor = std::pow (2.0, 7) - 1.0;

        int8_t *buffer = static_cast<int8_t *> (output);

        for (unsigned long i = 0; i < frames; i++)
          {
            buffer[2*i] = p_l[i] * scale_factor;
            buffer[2*i+1] = p_r[i] * scale_factor;
          }
      }
      break;

    case 16:
      {
        static double scale_factor = std::pow (2.0, 15) - 1.0;

        int16_t *buffer = static_cast<int16_t *> (output);

        for (unsigned long i = 0; i < frames; i++)
          {
            buffer[2*i] = p_l[i] * scale_factor;
            buffer[2*i+1] = p_r[i] * scale_factor;
          }
      }
      break;

    case 24:
      {
        static double scale_factor = std::pow (2.0, 23) - 1.0;

        static int big_endian = mach_info::words_big_endian ();

        uint8_t *buffer = static_cast<uint8_t *> (output);

        for (unsigned long i = 0; i < frames; i++)
          {
            int32_t sample_l = p_l[i];
            int32_t sample_r = p_r[i];

            sample_l &= 0x00ffffff;
            sample_r &= 0x00ffffff;

            uint8_t *_sample_l = reinterpret_cast<uint8_t *> (&sample_l);
            uint8_t *_sample_r = reinterpret_cast<uint8_t *> (&sample_r);

            unsigned long offset = i * 6;

            buffer[offset+0] = _sample_l[0+big_endian] * scale_factor;
            buffer[offset+1] = _sample_l[1+big_endian] * scale_factor;
            buffer[offset+2] = _sample_l[2+big_endian] * scale_factor;

            buffer[offset+3] = _sample_r[0+big_endian] * scale_factor;
            buffer[offset+4] = _sample_r[1+big_endian] * scale_factor;
            buffer[offset+5] = _sample_r[2+big_endian] * scale_factor;
          }
      }
      break;

    default:
      error ("invalid bit depth in audioplayer callback function");
    }

  return return_status;
}

static int
portaudio_play_callback (const void *, void *output, unsigned long frames,
                         const PaStreamCallbackTimeInfo *,
                         PaStreamCallbackFlags, void *data)
{
  audioplayer *player = static_cast<audioplayer *> (data);

  if (! player)
    error ("audioplayer callback function called without player");

  // Don't multiply the audio data by scale_factor here.  Although it would
  // move the operation outside of the loops below, it also causes a second
  // copy of the *entire* data array to be made when only a small portion
  // (buffer_size elements) is usually needed for this callback.

  const RowVector sound_l = player->get_left ();
  const RowVector sound_r = player->get_right ();

  const double *pl = sound_l.data ();
  const double *pr = sound_r.data ();

  if (player->get_type () == TYPE_DOUBLE)
    {
      switch (player->get_nbits ())
        {
        case 8:
          {
            static double scale_factor = std::pow (2.0, 7) - 1.0;

            int8_t *buffer = static_cast<int8_t *> (output);

            for (unsigned long j = 0; j < frames; j++)
              {
                unsigned int sample_number = player->get_sample_number ();

                if (sample_number >= player->get_end_sample ())
                  return paComplete;

                unsigned long offset = j * 2;

                buffer[offset+0] = pl[sample_number] * scale_factor;
                buffer[offset+1] = pr[sample_number] * scale_factor;

                player->set_sample_number (sample_number + 1);
              }
          }
          break;

        case 16:
          {
            static double scale_factor = std::pow (2.0, 15) - 1.0;

            int16_t *buffer = static_cast<int16_t *> (output);

            for (unsigned long j = 0; j < frames; j++)
              {
                unsigned int sample_number = player->get_sample_number ();

                if (sample_number >= player->get_end_sample ())
                  return paComplete;

                unsigned long offset = j * 2;

                buffer[offset+0] = pl[sample_number] * scale_factor;
                buffer[offset+1] = pr[sample_number] * scale_factor;

                player->set_sample_number (sample_number + 1);
              }
          }
          break;

        case 24:
          {
            static double scale_factor = std::pow (2.0, 23) - 1.0;

            static int big_endian = mach_info::words_big_endian ();

            uint8_t *buffer = static_cast<uint8_t *> (output);

            for (unsigned long j = 0; j < frames; j++)
              {
                unsigned int sample_number = player->get_sample_number ();

                if (sample_number >= player->get_end_sample ())
                  return paComplete;

                int32_t sample_l = pl[sample_number] * scale_factor;
                int32_t sample_r = pr[sample_number] * scale_factor;

                sample_l &= 0x00ffffff;
                sample_r &= 0x00ffffff;

                uint8_t *_sample_l = reinterpret_cast<uint8_t *> (&sample_l);
                uint8_t *_sample_r = reinterpret_cast<uint8_t *> (&sample_r);

                unsigned long offset = j * 6;

                buffer[offset+0] = _sample_l[0+big_endian];
                buffer[offset+1] = _sample_l[1+big_endian];
                buffer[offset+2] = _sample_l[2+big_endian];

                buffer[offset+3] = _sample_r[0+big_endian];
                buffer[offset+4] = _sample_r[1+big_endian];
                buffer[offset+5] = _sample_r[2+big_endian];

                player->set_sample_number (sample_number + 1);
              }
          }
          break;

        default:
          error ("invalid bit depth in audioplayer callback function");
        }
    }
  else if (player->get_type () == TYPE_INT8)
    {
      int8_t *buffer = static_cast<int8_t *> (output);

      for (unsigned long j = 0; j < frames; j++)
        {
          unsigned int sample_number = player->get_sample_number ();

          if (sample_number >= player->get_end_sample ())
            return paComplete;

          unsigned long offset = j * 2;

          buffer[offset+0] = pl[sample_number];
          buffer[offset+1] = pr[sample_number];

          player->set_sample_number (sample_number + 1);
        }
    }
  else if (player->get_type () == TYPE_UINT8)
    {
      uint8_t *buffer = static_cast<uint8_t *> (output);

      for (unsigned long j = 0; j < frames; j++)
        {
          unsigned int sample_number = player->get_sample_number ();

          if (sample_number >= player->get_end_sample ())
            return paComplete;

          unsigned long offset = j * 2;

          buffer[offset+0] = pl[sample_number];
          buffer[offset+1] = pr[sample_number];

          player->set_sample_number (sample_number + 1);
        }
    }
  else if (player->get_type () == TYPE_UINT16)
    {
      int16_t *buffer = static_cast<int16_t *> (output);

      for (unsigned long j = 0; j < frames; j++)
        {
          unsigned int sample_number = player->get_sample_number ();

          if (sample_number >= player->get_end_sample ())
            return paComplete;

          unsigned long offset = j * 2;

          buffer[offset+0] = pl[sample_number];
          buffer[offset+1] = pr[sample_number];

          player->set_sample_number (sample_number + 1);
        }
    }

  return paContinue;
}

audioplayer::audioplayer (void)
  : octave_callback_function (nullptr),
    id (-1), fs (0), nbits (16), channels (0), sample_number (0),
    end_sample (-1), tag (""), y (), userdata (Matrix ()),
    left (), right (), stream (nullptr), output_parameters (), type ()
{ }

audioplayer::~audioplayer (void)
{
  if (isplaying ())
    {
      warning_with_id ("Octave:audio-interrupt",
                       "interrupting audioplayer during playback");
      stop ();
    }
}

void
audioplayer::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
audioplayer::print_raw (std::ostream& os, bool) const
{
  os << 0;
}

void
audioplayer::init_fn (void)
{
  if (Pa_Initialize () != paNoError)
    error ("audioplayer: initialization error");

  if (Pa_GetDeviceCount () < 1)
    error ("audioplayer: no audio devices found or available");

  int device = get_id ();

  if (device == -1)
    device = Pa_GetDefaultOutputDevice ();

  output_parameters.device = device;
  output_parameters.channelCount = 2;
  output_parameters.sampleFormat = bits_to_format (get_nbits ());

  const PaDeviceInfo *device_info = Pa_GetDeviceInfo (device);

  if (! device_info)
    warning_with_id ("Octave:invalid-default-audio-device",
                     "invalid default audio device ID = %d", device);

  output_parameters.suggestedLatency
    = (device_info ? device_info->defaultHighOutputLatency : -1);

  output_parameters.hostApiSpecificStreamInfo = nullptr;
}

void
audioplayer::init (void)
{
  // FIXME: Both of these variables are unused.
  // Should they be eliminated or is something not yet implemented?
  //
  // int channels = y.rows ();
  // RowVector *sound_l = get_left ();

  if (Pa_Initialize () != paNoError)
    error ("audioplayer: initialization error");

  if (Pa_GetDeviceCount () < 1)
    error ("audioplayer: no audio devices found or available");

  int device = get_id ();

  if (device == -1)
    device = Pa_GetDefaultOutputDevice ();

  output_parameters.device = device;
  output_parameters.channelCount = 2;

  if (type == TYPE_DOUBLE)
    output_parameters.sampleFormat = bits_to_format (get_nbits ());
  else if (type == TYPE_INT8)
    output_parameters.sampleFormat = paInt8;
  else if (type == TYPE_UINT8)
    output_parameters.sampleFormat = paUInt8;
  else if (type == TYPE_UINT16)
    output_parameters.sampleFormat = paInt16;

  const PaDeviceInfo *device_info = Pa_GetDeviceInfo (device);

  if (! device_info)
    warning_with_id ("Octave:invalid-default-audio-device",
                     "invalid default audio device ID = %d", device);

  output_parameters.suggestedLatency
    = (device_info ? device_info->defaultHighOutputLatency : -1);

  output_parameters.hostApiSpecificStreamInfo = nullptr;
}

void
audioplayer::set_y (const octave_value& y_arg)
{
  if (y_arg.is_int8_type ())
    type = TYPE_INT8;
  else if (y_arg.is_uint8_type ())
    type = TYPE_UINT8;
  else if (y_arg.is_int16_type ())
    type = TYPE_UINT16;
  else
    type = TYPE_DOUBLE;

  y = y_arg.matrix_value ();

  if (y.rows () > 2)
    y = y.transpose ();

  channels = y.rows ();
  left = y.row (0);

  if (channels == 2)
    right = y.row (1);

  reset_end_sample ();
}

void
audioplayer::set_y (octave_function *fcn)
{
  octave_callback_function = fcn;
  channels = 2;
  reset_end_sample ();
}

Matrix&
audioplayer::get_y (void)
{
  return y;
}

RowVector
audioplayer::get_left (void) const
{
  return left;
}

RowVector
audioplayer::get_right (void) const
{
  return channels == 1 ? left : right;
}

void
audioplayer::set_fs (int fs_arg)
{
  fs = fs_arg;
}

int
audioplayer::get_fs (void)
{
  return fs;
}

void
audioplayer::set_nbits (int nbits_arg)
{
  nbits = nbits_arg;
}

int
audioplayer::get_nbits (void)
{
  return nbits;
}

void
audioplayer::set_id (int id_arg)
{
  id = id_arg;
}

int
audioplayer::get_id (void)
{
  return id;
}

int
audioplayer::get_channels (void)
{
  return channels;
}

audio_type
audioplayer::get_type (void)
{
  return type;
}

void
audioplayer::set_sample_number (unsigned int sample_number_arg)
{
  sample_number = sample_number_arg;
}

unsigned int
audioplayer::get_sample_number (void)
{
  return sample_number;
}

unsigned int
audioplayer::get_total_samples (void)
{
  return left.numel ();
}

void
audioplayer::set_end_sample (unsigned int end_sample_arg)
{
  end_sample = end_sample_arg;
}

unsigned int
audioplayer::get_end_sample (void)
{
  return end_sample;
}

void
audioplayer::reset_end_sample (void)
{
  set_end_sample (left.numel ());
}

void
audioplayer::set_tag (const charMatrix& tag_arg)
{
  tag = tag_arg;
}

charMatrix
audioplayer::get_tag (void)
{
  return tag;
}

void
audioplayer::set_userdata (const octave_value& userdata_arg)
{
  userdata = userdata_arg;
}

octave_value
audioplayer::get_userdata (void)
{
  return userdata;
}

void
audioplayer::playblocking (void)
{
  if (get_stream ())
    stop ();

  const unsigned int buffer_size = get_fs () / 20;
  OCTAVE_LOCAL_BUFFER (uint32_t, buffer, buffer_size * 2);

  PaError err;
  err = Pa_OpenStream (&stream, nullptr, &(output_parameters), get_fs (),
                       buffer_size, paClipOff, nullptr, nullptr);
  if (err != paNoError)
    error ("audioplayer: unable to open audio playback stream");

  err = Pa_StartStream (stream);
  if (err != paNoError)
    error ("audioplayer: unable to start audio playback stream");

  unsigned int start, end;
  start = get_sample_number ();
  end = get_end_sample ();

  unwind_action stop_audioplayer ([=] () { stop (); });

  for (unsigned int i = start; i < end; i += buffer_size)
    {
      octave_quit ();

      if (octave_callback_function != nullptr)
        octave_play_callback (nullptr, buffer, buffer_size, nullptr, 0, this);
      else
        portaudio_play_callback (nullptr, buffer, buffer_size, nullptr, 0, this);

      err = Pa_WriteStream (stream, buffer, buffer_size);
    }
}

void
audioplayer::play (void)
{
  if (get_stream ())
    stop ();

  const unsigned int buffer_size = get_fs () / 20;

  PaError err;
  if (octave_callback_function != nullptr)
    err = Pa_OpenStream (&stream, nullptr, &(output_parameters),
                         get_fs (), buffer_size, paClipOff,
                         octave_play_callback, this);
  else
    err = Pa_OpenStream (&stream, nullptr, &(output_parameters),
                         get_fs (), buffer_size, paClipOff,
                         portaudio_play_callback, this);

  if (err != paNoError)
    error ("audioplayer: failed to open audio playback stream");

  err = Pa_StartStream (stream);
  if (err != paNoError)
    error ("audioplayer: failed to start audio playback stream");
}

void
audioplayer::pause (void)
{
  if (get_stream () == nullptr)
    return;

  PaError err;
  err = Pa_StopStream (stream);
  if (err != paNoError)
    error ("audioplayer: failed to stop audio playback stream");
}

void
audioplayer::resume (void)
{
  if (get_stream () == nullptr)
    return;

  PaError err;
  err = Pa_StartStream (stream);
  if (err != paNoError)
    error ("audioplayer: failed to start audio playback stream");
}

PaStream *
audioplayer::get_stream (void)
{
  return stream;
}

void
audioplayer::stop (void)
{
  if (get_stream () == nullptr)
    return;

  PaError err;
  set_sample_number (0);
  reset_end_sample ();
  if (! Pa_IsStreamStopped (get_stream ()))
    {
      err = Pa_AbortStream (get_stream ());
      if (err != paNoError)
        error ("audioplayer: failed to stop audio playback stream");
    }

  err = Pa_CloseStream (get_stream ());
  if (err != paNoError)
    error ("audioplayer: failed to close audio playback stream");

  stream = nullptr;
}

bool
audioplayer::isplaying (void)
{
  if (get_stream () == nullptr)
    return false;

  PaError err;
  err = Pa_IsStreamActive (stream);
  if (err != 0 && err != 1)
    error ("audioplayer: checking stream activity status failed");

  return (err == 1);
}

class audiorecorder : public octave_base_dld_value
{
public:
  audiorecorder (void);
  ~audiorecorder (void);

  // Overloaded base functions
  double player_value (void) const { return 0; }
  virtual double scalar_value (bool = false) const { return 0; }
  void print (std::ostream& os, bool pr_as_read_syntax = false);
  void print_raw (std::ostream& os, bool pr_as_read_syntax) const;

  // Properties
  bool is_constant (void) const { return true; }
  bool is_defined (void) const { return true; }
  bool print_as_scalar (void) const { return true; }

  void init (void);
  void set_fs (int fs);
  int get_fs (void);
  void set_nbits (int nbits);
  int get_nbits (void);
  PaSampleFormat get_sampleFormat (void);
  void set_id (int id);
  int get_id (void);
  void set_channels (int channels);
  int get_channels (void);
  audio_type get_type (void);

  void set_sample_number (unsigned int sample);
  unsigned int get_sample_number (void);
  unsigned int get_total_samples (void);
  void set_end_sample (unsigned int sample);
  unsigned int get_end_sample (void);
  void reset_end_sample (void);
  void set_tag (const charMatrix& tag);
  charMatrix get_tag (void);
  void set_userdata (const octave_value& userdata);
  octave_value get_userdata (void);
  PaStream * get_stream (void);

  octave_value getaudiodata (void);
  audioplayer * getplayer (void);
  bool isrecording (void);
  audioplayer play (void);
  void record (void);
  void recordblocking (float seconds);
  void pause (void);
  void resume (void);
  void stop (void);
  void append (float sample_l, float sample_r);

  octave_function *octave_callback_function;

private:
  int id;
  int fs;
  int nbits;
  int channels;
  unsigned int sample_number;
  unsigned int end_sample;
  charMatrix tag;
  Matrix y;
  octave_value userdata;
  std::vector<float> left;
  std::vector<float> right;
  PaStream *stream;
  PaStreamParameters input_parameters;
  audio_type type;

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (audiorecorder, "audiorecorder", "audiorecorder");

static int
octave_record_callback (const void *input, void *, unsigned long frames,
                        const PaStreamCallbackTimeInfo *,
                        PaStreamCallbackFlags, void *data)
{
  audiorecorder *recorder = static_cast<audiorecorder *> (data);

  if (! recorder)
    error ("audiorecorder callback function called without recorder");

  int channels = recorder->get_channels ();

  Matrix sound (frames, 2);
  sound.resize (frames, 2);

  if (recorder->get_sampleFormat () == bits_to_format (8))
    {
      static double scale_factor = std::pow (2.0, 7) - 1.0;

      const int8_t *input8 = static_cast<const int8_t *> (input);

      for (unsigned long i = 0; i < frames; i++)
        {
          float sample_l = input8[i*channels] / scale_factor;
          float sample_r = input8[i*channels + (channels - 1)] / scale_factor;

          sound(i, 0) = sample_l;
          sound(i, 1) = sample_r;
        }
    }
  // FIXME: This is a workaround for a bug in PortAudio affecting 8-Bit
  //        recording (see Octave bug #44305).
  //        Remove this clause once the bug in PortAudio has been fixed.
  else if (recorder->get_sampleFormat () == bits_to_format (16)
           && recorder->get_nbits () == 8)
    {
      static double scale_factor = std::pow (2.0, 7) - 1.0;

      const int16_t *input16 = static_cast<const int16_t *> (input);

      for (unsigned long i = 0; i < frames; i++)
        {
          float sample_l = (input16[i*channels] >> 8) / scale_factor;
          float sample_r = (input16[i*channels + (channels - 1)] >> 8)
                           / scale_factor;

          sound(i, 0) = sample_l;
          sound(i, 1) = sample_r;
        }
    }
  else if (recorder->get_sampleFormat () == bits_to_format (16))
    {
      static double scale_factor = std::pow (2.0, 15) - 1.0;

      const int16_t *input16 = static_cast<const int16_t *> (input);

      for (unsigned long i = 0; i < frames; i++)
        {
          float sample_l = input16[i*channels] / scale_factor;
          float sample_r = input16[i*channels + (channels - 1)] / scale_factor;

          sound(i, 0) = sample_l;
          sound(i, 1) = sample_r;
        }
    }
  else if (recorder->get_sampleFormat () == bits_to_format (24))
    {
      static double scale_factor = std::pow (2.0, 23);

      // FIXME: Is there a better way?
      // Could use union of int32_t, uint8_t[3:0]?  (12/31/19)
      const uint8_t *input24 = static_cast<const uint8_t *> (input);

      int32_t sample_l32, sample_r32;

      uint8_t *sample_l = reinterpret_cast<uint8_t *> (&sample_l32);
      uint8_t *sample_r = reinterpret_cast<uint8_t *> (&sample_r32);

      for (unsigned long i = 0; i < frames; i++)
        {
          sample_l32 = sample_r32 = 0;
          for (int j = 0; j < 3; j++)
            {
              sample_l[j] = input24[i*channels*3 + j];
              sample_r[j] = input24[i*channels*3 + (channels - 1)*3 + j];
            }

          if (sample_l32 & 0x00800000)
            sample_l32 |= 0xff000000;

          if (sample_r32 & 0x00800000)
            sample_r32 |= 0xff000000;

          sound(i, 0) = sample_l32 / scale_factor;
          sound(i, 1) = sample_r32 / scale_factor;
        }
    }

  octave_value_list retval
    = feval (recorder->octave_callback_function, ovl (sound), 1);

  return retval(0).int_value ();
}

static int
portaudio_record_callback (const void *input, void *, unsigned long frames,
                           const PaStreamCallbackTimeInfo *,
                           PaStreamCallbackFlags, void *data)
{
  audiorecorder *recorder = static_cast<audiorecorder *> (data);

  if (! recorder)
    error ("audiorecorder callback function called without recorder");

  int channels = recorder->get_channels ();

  if (recorder->get_sampleFormat () == bits_to_format (8))
    {
      static float scale_factor = std::pow (2.0f, 7) - 1.0f;

      const int8_t *input8 = static_cast<const int8_t *> (input);

      for (unsigned long i = 0; i < frames; i++)
        {
          float sample_l = input8[i*channels] / scale_factor;
          float sample_r = input8[i*channels + (channels - 1)] / scale_factor;

          recorder->append (sample_l, sample_r);
        }
    }
  // FIXME: This is a workaround for a bug in PortAudio affecting 8-Bit
  //        recording (see Octave bug #44305).
  //        Remove this clause once the bug in PortAudio has been fixed.
  else if (recorder->get_sampleFormat () == bits_to_format (16)
           && recorder->get_nbits () == 8)
    {
      static double scale_factor = std::pow (2.0, 7) - 1.0;

      const int16_t *input16 = static_cast<const int16_t *> (input);

      for (unsigned long i = 0; i < frames; i++)
        {
          float sample_l = (input16[i*channels] >> 8) / scale_factor;
          float sample_r = (input16[i*channels + (channels - 1)] >> 8)
                           / scale_factor;

          recorder->append (sample_l, sample_r);
        }
    }
  else if (recorder->get_sampleFormat () == bits_to_format (16))
    {
      static float scale_factor = std::pow (2.0f, 15) - 1.0f;

      const int16_t *input16 = static_cast<const int16_t *> (input);

      for (unsigned long i = 0; i < frames; i++)
        {
          float sample_l = input16[i*channels] / scale_factor;
          float sample_r = input16[i*channels + (channels - 1)] / scale_factor;

          recorder->append (sample_l, sample_r);
        }
    }
  else if (recorder->get_sampleFormat () == bits_to_format (24))
    {
      static float scale_factor = std::pow (2.0f, 23);

      // FIXME: Is there a better way?
      // Could use union of int32_t, uint8_t[3:0]?  (12/31/19)
      const uint8_t *input24 = static_cast<const uint8_t *> (input);

      int32_t sample_l32, sample_r32;

      uint8_t *sample_l = reinterpret_cast<uint8_t *> (&sample_l32);
      uint8_t *sample_r = reinterpret_cast<uint8_t *> (&sample_r32);

      for (unsigned long i = 0; i < frames; i++)
        {
          sample_l32 = sample_r32 = 0;
          for (int j = 0; j < 3; j++)
            {
              sample_l[j] = input24[i*channels*3 + j];
              sample_r[j] = input24[i*channels*3 + (channels - 1)*3 + j];
            }

          if (sample_l32 & 0x00800000)
            sample_l32 |= 0xff000000;

          if (sample_r32 & 0x00800000)
            sample_r32 |= 0xff000000;

          recorder->append (sample_l32 / scale_factor,
                            sample_r32 / scale_factor);
        }
    }

  if (recorder->get_sample_number () >= recorder->get_end_sample ())
    return paComplete;

  return paContinue;
}

audiorecorder::audiorecorder (void)
  : octave_callback_function (nullptr),
    id (-1), fs (8000), nbits (8), channels (1), sample_number (0),
    end_sample (-1), tag (""), y (), userdata (Matrix ()),
    left (), right (), stream (nullptr), input_parameters (), type ()
{ }

audiorecorder::~audiorecorder (void)
{
  if (isrecording ())
    {
      warning_with_id ("Octave:audio-interrupt",
                       "interrupting audiorecorder during recording");
      stop ();
    }
}

void
audiorecorder::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
audiorecorder::print_raw (std::ostream& os, bool) const
{
  os << 0;
}

void
audiorecorder::init (void)
{
  if (Pa_Initialize () != paNoError)
    error ("audiorecorder: initialization error");

  if (Pa_GetDeviceCount () < 1)
    error ("audiorecorder: no audio devices found or available");

  int device = get_id ();

  if (device == -1)
    device = Pa_GetDefaultInputDevice ();

  input_parameters.device = device;
  input_parameters.channelCount = get_channels ();
  input_parameters.sampleFormat = bits_to_format (get_nbits ());

  // FIXME: This is a workaround for a bug in PortAudio affecting 8-Bit
  //        recording (see Octave bug #44305).
  //        Remove this clause once the bug in PortAudio has been fixed.
  if (get_nbits () == 8)
    input_parameters.sampleFormat = bits_to_format (16);

  const PaDeviceInfo *device_info = Pa_GetDeviceInfo (device);

  if (! device_info)
    warning_with_id ("Octave:invalid-default-audio-device",
                     "invalid default audio device ID = %d", device);

  input_parameters.suggestedLatency
    = (device_info ? device_info->defaultHighInputLatency : -1);

  input_parameters.hostApiSpecificStreamInfo = nullptr;
}

void
audiorecorder::set_fs (int fs_arg)
{
  fs = fs_arg;
}

int
audiorecorder::get_fs (void)
{
  return fs;
}

void
audiorecorder::set_nbits (int nbits_arg)
{
  nbits = nbits_arg;
}

int
audiorecorder::get_nbits (void)
{
  return nbits;
}

PaSampleFormat
audiorecorder::get_sampleFormat (void)
{
  return input_parameters.sampleFormat;
}

void
audiorecorder::set_id (int id_arg)
{
  id = id_arg;
}

int
audiorecorder::get_id (void)
{
  return id;
}

void
audiorecorder::set_channels (int channels_arg)
{
  if (channels_arg != 1 && channels_arg != 2)
    error ("audiorecorder: number of channels must be 1 or 2");

  channels = channels_arg;
}

int
audiorecorder::get_channels (void)
{
  return channels;
}

audio_type
audiorecorder::get_type (void)
{
  return type;
}

void
audiorecorder::set_sample_number (unsigned int sample_number_arg)
{
  sample_number = sample_number_arg;
}

unsigned int
audiorecorder::get_sample_number (void)
{
  return sample_number;
}

unsigned int
audiorecorder::get_total_samples (void)
{
  return left.size ();
}

void
audiorecorder::set_end_sample (unsigned int end_sample_arg)
{
  end_sample = end_sample_arg;
}

unsigned int
audiorecorder::get_end_sample (void)
{
  return end_sample;
}

void
audiorecorder::reset_end_sample (void)
{
  set_end_sample (left.size ());
}

void
audiorecorder::set_tag (const charMatrix& tag_arg)
{
  tag = tag_arg;
}

charMatrix
audiorecorder::get_tag (void)
{
  return tag;
}

void
audiorecorder::set_userdata (const octave_value& userdata_arg)
{
  userdata = userdata_arg;
}

octave_value
audiorecorder::get_userdata (void)
{
  return userdata;
}

octave_value
audiorecorder::getaudiodata (void)
{
  // Must get size before entering loop as the value of left.size() may change
  // during loop with simultaneous recording and playback (bug #50674).
  unsigned int ls = left.size ();
  Matrix audio (2, ls);

  for (unsigned int i = 0; i < ls; i++)
    {
      audio(0, i) = left[i];
      audio(1, i) = right[i];
    }

  return audio;
}

audioplayer *
audiorecorder::getplayer (void)
{
  audioplayer *player = new audioplayer ();

  player->set_y (getaudiodata ());
  player->set_fs (get_fs ());
  player->set_nbits (get_nbits ());
  player->init ();

  return player;
}

bool
audiorecorder::isrecording (void)
{
  if (get_stream () == nullptr)
    return false;

  PaError err;
  err = Pa_IsStreamActive (stream);
  if (err != 0 && err != 1)
    error ("audiorecorder: checking stream activity status failed");

  return (err == 1);
}

void
audiorecorder::record (void)
{
  if (get_stream ())
    stop ();

  left.clear ();
  right.clear ();

  const unsigned int buffer_size = get_fs () / 20;

  PaError err;
  if (octave_callback_function != nullptr)
    {
      err = Pa_OpenStream (&stream, &(input_parameters), nullptr,
                           get_fs (), buffer_size, paClipOff,
                           octave_record_callback, this);
    }
  else
    {
      err = Pa_OpenStream (&stream, &(input_parameters), nullptr,
                           get_fs (), buffer_size, paClipOff,
                           portaudio_record_callback, this);
    }
  if (err != paNoError)
    error ("audiorecorder: unable to open audio recording stream");

  err = Pa_StartStream (stream);
  if (err != paNoError)
    error ("audiorecorder: unable to start audio recording stream");
}

void
audiorecorder::recordblocking (float seconds)
{
  if (get_stream ())
    stop ();

  left.clear ();
  right.clear ();

  const unsigned int buffer_size = get_fs () / 20;
  OCTAVE_LOCAL_BUFFER (uint8_t, buffer, buffer_size * 2 * 3);

  PaError err;
  err = Pa_OpenStream (&stream, &(input_parameters), nullptr,
                       get_fs (), buffer_size, paClipOff, nullptr, this);
  if (err != paNoError)
    error ("audiorecorder: unable to open audio recording stream");

  err = Pa_StartStream (stream);
  if (err != paNoError)
    error ("audiorecorder: unable to start audio recording stream");

  unsigned int frames = seconds * get_fs ();

  unwind_action stop_audiorecorder ([=] () { stop (); });

  for (unsigned int i = 0; i < frames; i += buffer_size)
    {
      octave_quit ();

      Pa_ReadStream (get_stream (), buffer, buffer_size);

      if (octave_callback_function != nullptr)
        octave_record_callback (buffer, nullptr, buffer_size, nullptr, 0, this);
      else
        portaudio_record_callback (buffer, nullptr, buffer_size, nullptr, 0, this);
    }
}

void
audiorecorder::pause (void)
{
  if (get_stream () == nullptr)
    return;

  PaError err;
  err = Pa_StopStream (stream);
  if (err != paNoError)
    error ("audiorecorder: unable to stop audio recording stream");
}

void
audiorecorder::resume (void)
{
  if (get_stream () == nullptr)
    return;

  PaError err;
  err = Pa_StartStream (stream);
  if (err != paNoError)
    error ("audiorecorder: unable to start audio recording stream");
}

void
audiorecorder::stop (void)
{
  if (get_stream () == nullptr)
    return;

  PaError err;
  if (! Pa_IsStreamStopped (get_stream ()))
    {
      err = Pa_AbortStream (get_stream ());
      if (err != paNoError)
        error ("audioplayer: unable to stop audio playback stream");
    }

  err = Pa_CloseStream (stream);
  if (err != paNoError)
    error ("audiorecorder: unable to close audio recording stream");

  set_sample_number (0);
  reset_end_sample ();
  stream = nullptr;
}

void
audiorecorder::append (float sample_l, float sample_r)
{
  left.push_back (sample_l);
  right.push_back (sample_r);
  set_sample_number (get_sample_number () + 1);
}

PaStream *
audiorecorder::get_stream (void)
{
  return stream;
}

#endif

DEFUN_DLD (__recorder_audiorecorder__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{recorder} =} __recorder_audiorecorder__ (@var{fs}, @var{nbits}, @var{channels})
@deftypefnx {} {@var{recorder} =} __recorder_audiorecorder__ (@var{fs}, @var{nbits}, @var{channels}, @var{id})
@deftypefnx {} {@var{recorder} =} __recorder_audiorecorder__ (@var{fcn}, @dots{})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)

  int nargin = args.length ();

  audiorecorder *recorder = new audiorecorder ();

  if (nargin > 0)
    {
      bool is_function = (args(0).is_string () || args(0).is_function_handle ()
                          || args(0).is_inline_function ());

      if (is_function)
        error ("audiorecorder: callback functions are not yet implemented");
    }

  if (nargin >= 3)
    {
      recorder->set_fs (args(0).int_value ());
      recorder->set_nbits (args(1).int_value ());
      recorder->set_channels (args(2).int_value ());
    }

  if (nargin == 4)
    {
      recorder->set_id (args(3).int_value ());
    }

  recorder->init ();
  retval = recorder;

#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_audiorecorder__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

#if defined (HAVE_PORTAUDIO)

static audiorecorder *
get_recorder (const octave_value& ov)
{
  const octave_base_value& rep = ov.get_rep ();

  octave_base_value *ncrep = const_cast<octave_base_value *> (&rep);

  audiorecorder *rec = dynamic_cast<audiorecorder *> (ncrep);
  if (! rec)
    error ("audiodevinfo.cc (get_recorder): dynamic_cast to audiorecorder failed");

  return rec;
}

#endif

DEFUN_DLD (__recorder_getaudiodata__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{data} =} __recorder_getaudiodata__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  retval = get_recorder (args(0))->getaudiodata ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_getaudiodata__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__recorder_get_channels__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} __recorder_get_channels__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  retval = get_recorder (args(0))->get_channels ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_get_channels__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__recorder_get_fs__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{fs} =} __recorder_get_fs__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  retval = get_recorder (args(0))->get_fs ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_get_fs__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__recorder_get_id__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{id} =} __recorder_get_id__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  retval = get_recorder (args(0))->get_id ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_get_id__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__recorder_get_nbits__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{nbits} =} __recorder_get_nbits__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  retval = get_recorder (args(0))->get_nbits ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_get_nbits__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__recorder_get_sample_number__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} __recorder_get_sample_number__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  retval = get_recorder (args(0))->get_sample_number ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_get_sample_number__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__recorder_get_tag__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{tag} =} __recorder_get_tag__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  retval = get_recorder (args(0))->get_tag ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_get_tag__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__recorder_get_total_samples__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} __recorder_get_total_samples__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  retval = get_recorder (args(0))->get_total_samples ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_get_total_samples__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__recorder_get_userdata__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{data} =} __recorder_get_userdata__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  retval = get_recorder (args(0))->get_userdata ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_get_userdata__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__recorder_isrecording__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} __recorder_isrecording__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  retval = get_recorder (args(0))->isrecording ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_isrecording__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__recorder_pause__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __recorder_pause__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  get_recorder (args(0))->pause ();
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_pause__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__recorder_recordblocking__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __recorder_recordblocking__ (@var{recorder}, @var{seconds})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  float seconds = args(1).float_value ();
  get_recorder (args(0))->recordblocking (seconds);
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_recordblocking__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__recorder_record__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} __recorder_record__ (@var{recorder})
@deftypefnx {} {} __recorder_record__ (@var{recorder}, @var{seconds})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  audiorecorder *recorder = get_recorder (args(0));

  if (args.length () == 2)
    recorder->set_end_sample (args(1).int_value () * recorder->get_fs ());

  recorder->record ();
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_record__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__recorder_resume__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __recorder_resume__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    get_recorder (args(0))->resume ();
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_resume__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__recorder_set_fs__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __recorder_set_fs__ (@var{recorder}, @var{fs})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 2)
    get_recorder (args(0))->set_fs (args(1).int_value ());
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_set_fs__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__recorder_set_tag__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __recorder_set_tag__ (@var{recorder}, @var{tag})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 2)
    get_recorder (args(0))->set_tag (args(1).char_matrix_value ());
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_set_tag__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__recorder_set_userdata__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __recorder_set_userdata__ (@var{recorder}, @var{data})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 2)
    get_recorder (args(0))->set_userdata (args(1));
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_set_userdata__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__recorder_stop__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __recorder_stop__ (@var{recorder})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    get_recorder (args(0))->stop ();
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__recorder_stop__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__player_audioplayer__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{player} =} __player_audioplayer__ (@var{y}, @var{fs})
@deftypefnx {} {@var{player} =} __player_audioplayer__ (@var{y}, @var{fs}, @var{nbits})
@deftypefnx {} {@var{player} =} __player_audioplayer__ (@var{y}, @var{fs}, @var{nbits}, @var{id})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)

  audioplayer *recorder = new audioplayer ();

  bool is_function = (args(0).is_string () || args(0).is_function_handle ()
                      || args(0).is_inline_function ());

  if (is_function)
    error ("audioplayer: callback functions are not yet implemented");

  recorder->set_y (args(0));
  recorder->set_fs (args(1).int_value ());

  if (args.length () > 2)
    {
      // FIXME: Should be able to support 32-bit streams (bug #57939)
      int nbits = args(2).int_value ();
      if (nbits != 8 && nbits != 16 && nbits != 24)
        error ("audioplayer: NBITS must be 8, 16, or 24");

      switch (args.length ())
        {
        case 3:
          recorder->set_nbits (nbits);
          break;

        case 4:
          recorder->set_nbits (nbits);
          recorder->set_id (args(3).int_value ());
          break;
        }
    }

  if (is_function)
    recorder->init_fn ();
  else
    recorder->init ();

  retval = recorder;
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_audioplayer__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

#if defined (HAVE_PORTAUDIO)

static audioplayer *
get_player (const octave_value& ov)
{
  const octave_base_value& rep = ov.get_rep ();

  octave_base_value *ncrep = const_cast<octave_base_value *> (&rep);

  audioplayer *pl = dynamic_cast<audioplayer *> (ncrep);
  if (! pl)
    error ("audiodevinfo.cc (get_player): dynamic_cast to audioplayer failed");

  return pl;
}

#endif

DEFUN_DLD (__player_get_channels__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} __player_get_channels__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    retval = get_player (args(0))->get_channels ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_get_channels__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__player_get_fs__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{fs} =} __player_get_fs__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    retval = get_player (args(0))->get_fs ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_get_fs__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__player_get_id__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{id} =} __player_get_id__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    retval = get_player (args(0))->get_id ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_get_id__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__player_get_nbits__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{nbits} =} __player_get_nbits__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    retval = get_player (args(0))->get_nbits ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_get_nbits__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__player_get_sample_number__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} __player_get_sample_number__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    retval = get_player (args(0))->get_sample_number ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_get_sample_number__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__player_get_tag__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{tag} =} __player_get_tag__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    retval = get_player (args(0))->get_tag ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_get_tag__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__player_get_total_samples__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} __player_get_total_samples__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    retval = get_player (args(0))->get_total_samples ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_get_total_samples__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__player_get_userdata__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{data} =} __player_get_userdata__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    retval = get_player (args(0))->get_userdata ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_get_userdata__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__player_isplaying__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} __player_isplaying__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    retval = get_player (args(0))->isplaying ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_isplaying__",
                        "audio playback and recording through PortAudio");
#endif

  return retval;
}

DEFUN_DLD (__player_pause__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __player_pause__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    get_player (args(0))->pause ();
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_pause__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__player_playblocking__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} __player_playblocking__ (@var{player})
@deftypefnx {} {} __player_playblocking__ (@var{player}, @var{start})
@deftypefnx {} {} __player_playblocking__ (@var{player}, [@var{start}, @var{end}])
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)

  audioplayer *player = get_player (args(0));

  if (args.length () == 1)
    {
      player->playblocking ();
    }
  else if (args.length () == 2)
    {
      if (args(1).is_matrix_type ())
        {
          RowVector range = args(1).row_vector_value ();

          unsigned int start = range.elem (0) - 1;
          unsigned int end = range.elem (1) - 1;

          if (start > player->get_total_samples ()
              || start > end || end > player->get_total_samples ())
            error ("audioplayer: invalid range specified for playback");

          player->set_sample_number (start);
          player->set_end_sample (end);
        }
      else
        {
          unsigned int start = args(1).int_value () - 1;

          if (start > player->get_total_samples ())
            error ("audioplayer: invalid range specified for playback");

          player->set_sample_number (start);
        }

      player->playblocking ();
    }

  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_playblocking__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__player_play__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} __player_play__ (@var{player})
@deftypefnx {} {} __player_play__ (@var{player}, @var{start})
@deftypefnx {} {} __player_play__ (@var{player}, [@var{start}, @var{end}])
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)

  if (args.length () == 1)
    {
      get_player (args(0))->play ();
    }
  else if (args.length () == 2)
    {
      audioplayer *player = get_player (args(0));

      if (args(1).is_matrix_type ())
        {
          RowVector range = args(1).row_vector_value ();

          unsigned int start = range.elem (0) - 1;
          unsigned int end = range.elem (1) - 1;

          if (start > player->get_total_samples ()
              || start > end || end > player->get_total_samples ())
            error ("audioplayer: invalid range specified for playback");

          player->set_sample_number (start);
          player->set_end_sample (end);
        }
      else
        {
          unsigned int start = args(1).int_value () - 1;

          if (start > player->get_total_samples ())
            error ("audioplayer: invalid range specified for playback");

          player->set_sample_number (start);
        }

      player->play ();
    }

  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_play__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__player_resume__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __player_resume__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    get_player (args(0))->resume ();
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_resume__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__player_set_fs__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __player_set_fs__ (@var{player}, @var{fs})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 2)
    get_player (args(0))->set_fs (args(1).int_value ());
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_set_fs__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__player_set_tag__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __player_set_tag__ (@var{player}, @var{tag})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 2)
    get_player (args(0))->set_tag (args(1).char_matrix_value ());
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_set_tag__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__player_set_userdata__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __player_set_userdata__ (@var{player}, @var{data})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 2)
    get_player (args(0))->set_userdata (args(1));
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_set_userdata__",
                        "audio playback and recording through PortAudio");
#endif
}

DEFUN_DLD (__player_stop__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __player_stop__ (@var{player})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_PORTAUDIO)
  if (args.length () == 1)
    get_player (args(0))->stop ();
  return ovl ();
#else
  octave_unused_parameter (args);

  err_disabled_feature ("__player_stop__",
                        "audio playback and recording through PortAudio");
#endif
}

OCTAVE_END_NAMESPACE(octave)
