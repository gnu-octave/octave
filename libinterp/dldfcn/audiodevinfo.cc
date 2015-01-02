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

// <cstdint> requires c++11
#include <stdint.h>

#include <string>
#include <vector>

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-int32.h"
#include "ov-struct.h"
#include "parse.h"

#if defined (HAVE_PORTAUDIO)
#include <portaudio.h>
#endif

PaSampleFormat
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

DEFUN_DLD (audiodevinfo, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{devinfo} =} audiodevinfo ()\n\
\n\
Returns a structure with two fields called \"input\" and \"output\".\n\
Each structure contains an array of structures with three fields called\n\
\"Name\", \"DriverVersion\" and \"ID\". Each structure contains information\n\
about a PortAudio device.\n\
\n\
@end deftypefn\n\
\n\
@deftypefn {Loadable Function} {@var{devs} =} audiodevinfo (@var{io})\n\
\n\
Returns the number of input or output devices available. Set @var{io} to 1\n\
for input devices and to 0 for output devices.\n\
@end deftypefn\n\
\n\
@deftypefn {Loadable Function} {@var{name} =} audiodevinfo (@var{io}, @var{id})\n\
\n\
Returns the name of a device specified by numerical @var{id}. Set @var{io}\n\
to 1 for input devices and to 0 for output devices.\n\
@end deftypefn\n\
\n\
@deftypefn {Loadable Function} {@var{id} =} audiodevinfo (@var{io}, @var{name})\n\
\n\
Returns the id of a device specified by name. Set @var{io}\n\
to 1 for input devices and to 0 for output devices.\n\
@end deftypefn\n\
\n\
@deftypefn {Loadable Function} {@var{id} =} audiodevinfo (@var{io}, @var{rate}, @var{bits}, @var{chans})\n\
\n\
Returns the id of the first device that supports playback or recording\n\
using the specified sampling rate (@var{rate}), bits per sample (@var{bits})\n\
and number of channels (@var{chans}). Set @var{io} to 1 for input devices\n\
and to 0 for output devices.\n\
@end deftypefn\n\
\n\
@deftypefn {Loadable Function} {@var{supports} =} audiodevinfo (@var{io}, @var{id}, @var{rate}, @var{bits}, @var{chans})\n\
\n\
Returns 1 if the device bearing @var{id} supports specified sampling rate\n\
(@var{rate}), bits per sample (@var{bits}) and number of channels (@var{chans}).\n\
Returns 0 otherwise. Set @var{io} to 1 for input devices and to 0 for output\n\
devices.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  PaError err;
  octave_scalar_map devinfo;
  octave_value_list input;
  octave_value_list output;

  err = Pa_Initialize ();
  if (err != paNoError)
    {
      error ("audiodevinfo: cannot initialize PortAudio");
      return retval;
    }

  int num_devices = Pa_GetDeviceCount ();
  if (num_devices < 0)
    {
      error ("audiodevinfo: no audio device found");
      return retval;
    }

  octave_idx_type numinput = 0, numoutput = 0;
  for (int i = 0; i < num_devices; i++)
    {
      const PaDeviceInfo *device_info = Pa_GetDeviceInfo (i);
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

  octave_idx_type idx_i = 0, idx_o = 0;
  for (int i = 0; i < num_devices; i++)
    {
      const PaDeviceInfo *device_info = Pa_GetDeviceInfo (i);
      const char *driver;
      char name[128];
      driver = Pa_GetHostApiInfo (device_info->hostApi)->name;
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

  // Return information about input and output audio devices and
  // their properties.
  if (nargin == 0)
    retval = devinfo;
  // Return the number of input or output devices
  else if (nargin == 1)
    {
      if (args(0).int_value () == 0)
        retval = octave_value (numoutput);
      else if (args(0).int_value () == 1)
        retval = octave_value (numinput);
      else
        {
          error ("audiodevinfo: please specify 0 for output and 1 for input devices");
          return retval;
        }
    }
  // Return device name when given id or id when given device name.
  else if (nargin == 2)
    {
      bool found = false;
      int outin = args(0).int_value ();
      if (args(1).is_string ())
        {
          if (outin == 0)
            {
              for (int i = 0; i < numoutput; i++)
                {
                  if (output_name(i).string_value () == args(1).string_value ())
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
                  if (input_name(i).string_value () == args(1).string_value ())
                    {
                      retval = input_id(i);
                      found = true;
                      break;
                    }
                }
            }
          else
            {
              error ("audiodevinfo: please specify 0 for output and 1 for input devices");
              return retval;
            }
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
            {
              error ("audiodevinfo: please specify 0 for output and 1 for input devices");
              return retval;
            }
        }
      if (not found)
        error ("audiodevinfo: no device meeting the specified criteria found");
    }
  else if (nargin == 3)
    {
      //
    }
  // Return the id of the first device meeting specified criteria.
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
            {
              error ("audiodevinfo: no such bits per sample format");
              return retval;
            }
          stream_parameters.suggestedLatency =
              Pa_GetDeviceInfo (i)->defaultLowInputLatency;
          stream_parameters.hostApiSpecificStreamInfo = NULL;
          if (io == 0)
            {
              if (Pa_GetDeviceInfo (i)->maxOutputChannels < chans)
                continue;

              err = Pa_IsFormatSupported (NULL, &stream_parameters, rate);
              if (err == paFormatIsSupported)
                {
                  retval = i;
                  return retval;
                }
            }
          else if (io == 1)
            {
              if (Pa_GetDeviceInfo (i)->maxInputChannels < chans)
                continue;

              err = Pa_IsFormatSupported (&stream_parameters, NULL, rate);
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
        {
          error ("audiodevinfo: no such bits per sample format");
          return retval;
        }
      stream_parameters.suggestedLatency =
        Pa_GetDeviceInfo (id)->defaultLowInputLatency;
      stream_parameters.hostApiSpecificStreamInfo = NULL;
      if (io == 0)
        {
          if (Pa_GetDeviceInfo (id)->maxOutputChannels < chans)
            {
              retval = 0;
              return retval;
            }
          err = Pa_IsFormatSupported (NULL, &stream_parameters, rate);
          if (err == paFormatIsSupported)
            {
              retval = 1;
              return retval;
            }
        }
      else if (io == 1)
        {
          if (Pa_GetDeviceInfo (id)->maxInputChannels < chans)
            {
              retval = 0;
              return retval;
            }
          err = Pa_IsFormatSupported (&stream_parameters, NULL, rate);
          if (err == paFormatIsSupported)
            {
              retval = 1;
              return retval;
            }
        }
      else
        {
          error ("audiodevinfo: please specify 0 for output and 1 for input devices");
          return retval;
        }
      retval = 0;
    }
  else
    {
      error ("audiodevinfo: wrong number of arguments");
      return retval;
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

/*
%!test
%! devinfo = audiodevinfo;
%! assert (rows (devinfo.input), 1);
%! assert (rows (devinfo.output), 1);

%!test
%! devinfo = audiodevinfo;
%! nout = audiodevinfo (0);
%! nin = audiodevinfo (1);
%! assert (columns (devinfo.output), nout);
%! assert (columns (devinfo.input), nin);

%!test
%! devinfo = audiodevinfo;
%! nout = audiodevinfo (0);
%! nin = audiodevinfo (1);
%! for i = 1:nout,
%!   assert (devinfo.output(i).Name, audiodevinfo (0, devinfo.output(i).ID))
%! endfor
%! for i=1:nin,
%!   assert (devinfo.input(i).Name, audiodevinfo (1, devinfo.input(i).ID))
%! endfor

%!test
%! devinfo = audiodevinfo;
%! nout = audiodevinfo (0);
%! nin = audiodevinfo (1);
%! for i = 1:nout,
%!   assert (devinfo.output(i).ID, audiodevinfo (0, devinfo.output(i).Name))
%! endfor
%! for i = 1:nin,
%!   assert (devinfo.input(i).ID, audiodevinfo (1, devinfo.input(i).Name))
%! endfor
*/

enum audio_type { INT8, UINT8, INT16, DOUBLE };

class audioplayer : public octave_base_value
{
public:
  audioplayer (void);
  ~audioplayer (void) {};

  // Overloaded base functions
  double player_value (void) const { return 0; }
  virtual double scalar_value (bool = false) const { return 0; }
  void print (std::ostream& os, bool pr_as_read_syntax = false) const;
  void print_raw (std::ostream& os, bool pr_as_read_syntax) const;

  // Properties
  bool is_constant (void) const { return true; }
  bool is_defined (void) const { return true; }
  bool print_as_scalar (void) const { return true; }

  void init (void);
  void init_fn (void);
  void set_y (octave_value y);
  void set_y (octave_function *fn);
  void set_y (std::string fn);
  Matrix& get_y (void);
  RowVector *get_left (void);
  RowVector *get_right (void);
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
  void set_tag (charMatrix tag);
  charMatrix get_tag (void);
  void set_userdata (octave_value userdata);
  octave_value get_userdata (void);
  PaStream *get_stream (void);
  octave_function *octave_callback_function;

  void playblocking (void);
  void play (void);
  void pause (void);
  void resume (void);
  void stop (void);
  bool isplaying (void);

private:
  Matrix y;
  RowVector left;
  RowVector right;
  charMatrix tag;
  octave_value userdata;
  int channels;
  int fs;
  int nbits;
  int id;
  unsigned int sample_number;
  unsigned int end_sample;
  PaStream *stream;
  PaStreamParameters output_parameters;
  audio_type type;
  DECLARE_OCTAVE_ALLOCATOR
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#define BUFFER_SIZE 512

DEFINE_OCTAVE_ALLOCATOR (audioplayer);
DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (audioplayer, "audioplayer", "audioplayer");

int
is_big_endian (void)
{
  union
    {
      uint32_t i;
      char c[4];
    } bint = { 0x01020304 };
  return bint.c[0] == 1;
}

static int
octave_play_callback (const void *, void *output, unsigned long frames,
                      const PaStreamCallbackTimeInfo *,
                      PaStreamCallbackFlags, void *data)
{
  audioplayer *player = static_cast<audioplayer *> (data);
  int big_endian = is_big_endian ();
  octave_value_list args, retval;
  args(0) = frames;
  retval = feval (player->octave_callback_function, args, 1);
  RowVector sound_l, sound_r;
  Matrix sound = retval(0).matrix_value ();
  int return_status = retval(1).int_value ();
  sound_l.resize (frames);
  sound_r.resize (frames);
  if (sound.cols () == 1)
    {
      for (unsigned long i = 0; i < frames; i++)
        {
          sound_l(i) = sound(i, 0);
          sound_r(i) = sound(i, 0);
        }
    }
  else if (sound.cols () == 2)
    {
      for (unsigned long i = 0; i < frames; i++)
        {
          sound_l(i) = sound(i, 0);
          sound_r(i) = sound(i, 1);
        }
    }
  else
    return paAbort;

  for (unsigned long i = 0; i < frames; i++)
    {
      if (player->get_nbits () == 8)
        {
          int8_t *buffer = static_cast<int8_t *> (output);
          buffer[2 * i] = sound_l.elem (i) * (pow (2.0, 7) - 1);
          buffer[2 * i + 1] = sound_r.elem (i) * (pow (2.0, 7) - 1);
        }
      else if (player->get_nbits () == 16)
        {
          int16_t *buffer = static_cast<int16_t *> (output);
          buffer[2 * i] = sound_l.elem (i) * (pow (2.0, 15) - 1);
          buffer[2 * i + 1] = sound_r.elem (i) * (pow (2.0, 15) - 1);
        }
      else if (player->get_nbits () == 24)
        {
          uint8_t *buffer = static_cast<uint8_t *> (output);
          int32_t sample_l = sound_l.elem (i) * (pow (2.0, 23) - 1);
          int32_t sample_r = sound_r.elem (i) * (pow (2.0, 23) - 1);
          sample_l &= 0x00ffffff;
          sample_r &= 0x00ffffff;
          // FIXME: Would a mask work better?
          uint8_t *_sample_l = reinterpret_cast<uint8_t *> (&sample_l);
          uint8_t *_sample_r = reinterpret_cast<uint8_t *> (&sample_r);
          buffer[i * 6 + 0] = _sample_l[0 + big_endian];
          buffer[i * 6 + 1] = _sample_l[1 + big_endian];
          buffer[i * 6 + 2] = _sample_l[2 + big_endian];
          buffer[i * 6 + 3] = _sample_r[0 + big_endian];
          buffer[i * 6 + 4] = _sample_r[1 + big_endian];
          buffer[i * 6 + 5] = _sample_r[2 + big_endian];
        }
    }
  return return_status;
}

static int
portaudio_play_callback (const void *, void *output, unsigned long frames,
                         const PaStreamCallbackTimeInfo*,
                         PaStreamCallbackFlags, void *data)
{
  audioplayer *player = static_cast<audioplayer *> (data);
  int big_endian = is_big_endian ();
  int channels = player->get_channels ();
  RowVector *sound_l = player->get_left ();
  RowVector *sound_r;

  if (channels > 1)
    sound_r = player->get_right ();
  else
    sound_r = sound_l;

  for (unsigned long j = 0, k = 0; j < frames; j++, k += 2)
    {
      unsigned int sample_number = player->get_sample_number ();
      if (sample_number > player->get_end_sample ())
        return paAbort;

      if (player->get_type () == DOUBLE)
        {
          if (player->get_nbits () == 8)
            {
              int8_t *buffer = static_cast<int8_t *> (output);
              buffer[k] = sound_l->elem (sample_number) * (pow (2.0, 7) - 1);
              buffer[k + 1] = sound_r->elem (sample_number) * (pow (2.0, 7) - 1);
            }
          else if (player->get_nbits () == 16)
            {
              int16_t *buffer = static_cast<int16_t *> (output);
              buffer[k] = sound_l->elem (sample_number) * (pow (2.0, 15) - 1);
              buffer[k + 1] = sound_r->elem (sample_number) * (pow (2.0, 15) - 1);
            }
          else if (player->get_nbits () == 24)
            {
              uint8_t *buffer = static_cast<uint8_t *> (output);
              int32_t sample_l = sound_l->elem (sample_number) * (pow (2.0, 23) - 1);
              int32_t sample_r = sound_r->elem (sample_number) * (pow (2.0, 23) - 1);
              sample_l &= 0x00ffffff;
              sample_r &= 0x00ffffff;
              // FIXME: Would a mask work better?
              uint8_t *_sample_l = reinterpret_cast<uint8_t *> (&sample_l);
              uint8_t *_sample_r = reinterpret_cast<uint8_t *> (&sample_r);
              buffer[j * 6 + 0] = _sample_l[0 + big_endian];
              buffer[j * 6 + 1] = _sample_l[1 + big_endian];
              buffer[j * 6 + 2] = _sample_l[2 + big_endian];
              buffer[j * 6 + 3] = _sample_r[0 + big_endian];
              buffer[j * 6 + 4] = _sample_r[1 + big_endian];
              buffer[j * 6 + 5] = _sample_r[2 + big_endian];
            }
        }
      else if (player->get_type () == INT8)
        {
          int8_t *buffer = static_cast<int8_t *> (output);
          buffer[k] = sound_l->elem (sample_number);
          buffer[k + 1] = sound_r->elem (sample_number);
        }
      else if (player->get_type () == UINT8)
        {
          uint8_t *buffer = static_cast<uint8_t *> (output);
          buffer[k] = sound_l->elem (sample_number);
          buffer[k + 1] = sound_r->elem (sample_number);
        }
      else if (player->get_type () == INT16)
        {
          int16_t *buffer = static_cast<int16_t *> (output);
          buffer[k] = sound_l->elem (sample_number);
          buffer[k + 1] = sound_r->elem (sample_number);
        }
      player->set_sample_number (sample_number + 1);
    }
  return paContinue;
}

audioplayer::audioplayer (void)
{
  this->nbits = 16;
  this->id = -1;
  this->sample_number = 0;
  this->tag = charMatrix ("");
  Matrix userdata;
  this->userdata = octave_value (userdata);
  this->stream = 0;
  this->octave_callback_function = 0;
}

void
audioplayer::print (std::ostream& os, bool pr_as_read_syntax) const
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
  PaError err;
  int device;

  err = Pa_Initialize ();
  if (err != paNoError)
    {
      error ("audioplayer: Initialization error!");
      return;
    }

  int numDevices = Pa_GetDeviceCount ();
  if (numDevices < 0)
    {
      error ("audioplayer: No audio devices found!");
      return;
    }

  if (this->get_id () == -1)
    device = Pa_GetDefaultOutputDevice ();
  else
    device = this->get_id ();

  output_parameters.device = device;
  output_parameters.channelCount = 2;
  output_parameters.sampleFormat = bits_to_format (this->get_nbits ());
  output_parameters.suggestedLatency = Pa_GetDeviceInfo (device)->defaultHighOutputLatency;
  output_parameters.hostApiSpecificStreamInfo = NULL;
}

void
audioplayer::init (void)
{
  PaError err;

  // Both of these variables are unused.  Should they be
  // eliminated or is something not yet implemented?
  //
  // int channels = this->y.rows ();
  // RowVector *sound_l = this->get_left ();

  int device;

  err = Pa_Initialize ();
  if (err != paNoError)
    {
      error ("audioplayer: Initialization error!");
      return;
    }

  int numDevices = Pa_GetDeviceCount ();
  if (numDevices < 0)
    {
      error ("audioplayer: No audio devices found!");
      return;
    }

  if (this->get_id () == -1)
    device = Pa_GetDefaultOutputDevice ();
  else
    device = this->get_id ();

  output_parameters.device = device;
  output_parameters.channelCount = 2;

  if (this->type == DOUBLE)
    output_parameters.sampleFormat = bits_to_format (this->get_nbits ());
  else if (this->type == INT8)
    output_parameters.sampleFormat = paInt8;
  else if (this->type == UINT8)
    output_parameters.sampleFormat = paUInt8;
  else if (this->type == INT16)
    output_parameters.sampleFormat = paInt16;

  output_parameters.suggestedLatency = Pa_GetDeviceInfo (device)->defaultHighOutputLatency;
  output_parameters.hostApiSpecificStreamInfo = NULL;
}

void
audioplayer::set_y (octave_value y)
{
  if (y.is_int8_type ())
    this->type = INT8;
  else if (y.is_uint8_type ())
    this->type = UINT8;
  else if (y.is_int16_type ())
    this->type = INT16;
  else
    this->type = DOUBLE;

  this->y = y.matrix_value ();
  if (this->y.rows () > 2)
    this->y = this->y.transpose ();

  this->channels = this->y.rows ();
  this->left = this->y.row (0);
  if (this->channels == 2)
    this->right = this->y.row (1);

  this->reset_end_sample ();
}

void
audioplayer::set_y (octave_function *fn)
{
  this->octave_callback_function = fn;
  this->channels = 2;
  this->reset_end_sample ();
}

Matrix&
audioplayer::get_y (void)
{
  return this->y;
}

RowVector *
audioplayer::get_left (void)
{
  return &(this->left);
}

RowVector *
audioplayer::get_right (void)
{
  return &(this->right);
}

void
audioplayer::set_fs (int fs)
{
  this->fs = fs;
}

int
audioplayer::get_fs (void)
{
  return this->fs;
}

void
audioplayer::set_nbits (int nbits)
{
  this->nbits = nbits;
}

int
audioplayer::get_nbits (void)
{
  return this->nbits;
}

void
audioplayer::set_id (int id)
{
  this->id = id;
}

int
audioplayer::get_id (void)
{
  return this->id;
}

int
audioplayer::get_channels (void)
{
  return this->channels;
}

audio_type
audioplayer::get_type (void)
{
  return this->type;
}

void
audioplayer::set_sample_number (unsigned int sample_number)
{
  this->sample_number = sample_number;
}

unsigned int
audioplayer::get_sample_number (void)
{
  return this->sample_number;
}

unsigned int
audioplayer::get_total_samples (void)
{
  return this->left.length ();
}

void
audioplayer::set_end_sample (unsigned int end_sample)
{
  this->end_sample = end_sample;
}

unsigned int
audioplayer::get_end_sample (void)
{
  return this->end_sample;
}

void
audioplayer::reset_end_sample (void)
{
  this->set_end_sample (this->left.length ());
}

void
audioplayer::set_tag (charMatrix tag)
{
  this->tag = tag;
}

charMatrix
audioplayer::get_tag (void)
{
  return this->tag;
}

void
audioplayer::set_userdata (octave_value userdata)
{
  this->userdata = userdata;
}

octave_value
audioplayer::get_userdata (void)
{
  return this->userdata;
}

void
audioplayer::playblocking (void)
{
  if (this->get_stream ())
    this->stop ();

  PaError err;
  uint32_t buffer[BUFFER_SIZE * 2];
  err = Pa_OpenStream (&stream, NULL, &(this->output_parameters), this->get_fs (), BUFFER_SIZE, paClipOff, NULL, NULL);
  if (err != paNoError)
    {
      error ("audioplayer: Error opening audio playback stream");
      return;
    }

  err = Pa_StartStream (stream);
  if (err != paNoError)
    {
      error ("audioplayer: Error starting audio playback stream");
      return;
    }

  unsigned int start, end;
  start = this->get_sample_number ();
  end = this->get_end_sample ();
  for (unsigned int i = start; i < end; i += BUFFER_SIZE)
    {
      if (this->octave_callback_function != 0)
        octave_play_callback (0, buffer, BUFFER_SIZE, 0, 0, this);
      else
        portaudio_play_callback (0, buffer, BUFFER_SIZE, 0, 0, this);

      err = Pa_WriteStream (stream, buffer, BUFFER_SIZE);
    }

  err = Pa_StopStream (stream);
  if (err != paNoError)
    {
      error ("audioplayer: Error stoping audio playback stream");
      return;
    }

  err = Pa_CloseStream (stream);
  if (err != paNoError)
    {
      error ("audioplayer: Error closing audio playback stream");
      return;
    }

  stream = 0;
  this->set_sample_number (0);
  this->reset_end_sample ();
}

void
audioplayer::play (void)
{
  if (this->get_stream ())
    this->stop ();

  PaError err;
  if (this->octave_callback_function != 0)
    err = Pa_OpenStream (&stream, NULL, &(this->output_parameters),
                         this->get_fs (), BUFFER_SIZE, paClipOff,
                         octave_play_callback, this);
  else
    err = Pa_OpenStream (&stream, NULL, &(this->output_parameters),
                         this->get_fs (), BUFFER_SIZE, paClipOff,
                         portaudio_play_callback, this);

  if (err != paNoError)
    {
      error ("audioplayer: Error opening audio playback stream");
      return;
    }

  err = Pa_StartStream (stream);
  if (err != paNoError)
    {
      error ("audioplayer: Error starting audio playback stream");
      return;
    }
}

void
audioplayer::pause (void)
{
  if (this->get_stream () == 0)
    return;

  PaError err;
  err = Pa_StopStream (stream);
  if (err != paNoError)
    {
      error ("audiorecorder: Error stoping audio recording stream");
      return;
    }
}

void
audioplayer::resume (void)
{
  if (this->get_stream () == 0)
    return;

  PaError err;
  err = Pa_StartStream (stream);
  if (err != paNoError)
    {
      error ("audiorecorder: Error starting audio recording stream");
      return;
    }
}

PaStream *
audioplayer::get_stream (void)
{
  return this->stream;
}

void
audioplayer::stop (void)
{
  if (this->get_stream () == 0)
    return;

  PaError err;
  this->set_sample_number (0);
  this->reset_end_sample ();
  if (not Pa_IsStreamStopped (this->get_stream ()))
    {
      err = Pa_AbortStream (this->get_stream ());
      if (err != paNoError)
        {
          error ("audioplayer: Error stopping audio playback stream");
          return;
        }
    }

  err = Pa_CloseStream (this->get_stream ());
  if (err != paNoError)
    {
      error ("audioplayer: Error closing audio playback stream");
      return;
    }

  stream = 0;
}

bool
audioplayer::isplaying (void)
{
  if (this->get_stream () == 0)
    return false;

  PaError err;
  err = Pa_IsStreamActive (stream);
  if (err != 0 && err != 1)
    {
      error ("audiorecorder: Error checking stream activity status");
      return false;
    }

  return (err == 1);
}

class audiorecorder : public octave_base_value
{
public:
  audiorecorder (void);
  ~audiorecorder (void) {};

  // Overloaded base functions
  double player_value (void) const { return 0; }
  virtual double scalar_value (bool = false) const { return 0; }
  void print (std::ostream& os, bool pr_as_read_syntax = false) const;
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
  void set_tag (charMatrix tag);
  charMatrix get_tag (void);
  void set_userdata (octave_value userdata);
  octave_value get_userdata (void);
  PaStream *get_stream (void);
  octave_function *octave_callback_function;

  octave_value getaudiodata (void);
  audioplayer *getplayer (void);
  bool isrecording (void);
  audioplayer play (void);
  void record (void);
  void recordblocking (float seconds);
  void pause (void);
  void resume (void);
  void stop (void);
  void append (float sample_l, float sample_r);

private:
  Matrix y;
  std::vector<float> left;
  std::vector<float> right;
  charMatrix tag;
  octave_value userdata;
  int channels;
  int fs;
  int nbits;
  int id;
  unsigned int sample_number;
  unsigned int end_sample;
  PaStream *stream;
  PaStreamParameters input_parameters;
  audio_type type;
  DECLARE_OCTAVE_ALLOCATOR
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

DEFINE_OCTAVE_ALLOCATOR (audiorecorder);
DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (audiorecorder, "audiorecorder", "audiorecorder");

static int
octave_record_callback (const void *input, void *, unsigned long frames,
                        const PaStreamCallbackTimeInfo *,
                        PaStreamCallbackFlags, void *data)
{
  audiorecorder *recorder = static_cast<audiorecorder *> (data);
  int channels = recorder->get_channels ();
  float sample_l, sample_r;
  Matrix sound;
  sound.resize (frames, 2);
  if (recorder->get_nbits () == 8)
    {
      const int8_t *input8 = static_cast<const int8_t *> (input);
      for (unsigned long i = 0; i < frames; i++)
        {
          sample_l = input8[i * channels] / (pow (2.0, 7) - 1.0);
          sample_r = input8[i * channels + (channels - 1)] / (pow (2.0, 7) - 1.0);
          sound(i, 0) = sample_l;
          sound(i, 1) = sample_r;
        }
      }
  else if (recorder->get_nbits () == 16)
    {
      const int16_t *input16 = static_cast<const int16_t *> (input);
      for (unsigned long i = 0; i < frames; i++)
        {
          sample_l = input16[i * channels] / (pow (2.0, 15) - 1.0);
          sample_r = input16[i * channels + (channels - 1)] / (pow (2.0, 15) - 1.0);
          sound(i, 0) = sample_l;
          sound(i, 1) = sample_r;
        }
    }
  else if (recorder->get_nbits () == 24)
    {
      // FIXME: Is there a better way?
      const uint8_t *input24 = static_cast<const uint8_t *> (input);
      int32_t sample_l32, sample_r32;
      uint8_t *_sample_l = reinterpret_cast<uint8_t *> (&sample_l);
      uint8_t *_sample_r = reinterpret_cast<uint8_t *> (&sample_r);
      for (unsigned long i = 0; i < frames; i++)
        {
          for (int j = 0; j < 3; j++)
            {
              _sample_l[j] = input24[i * channels * 3 + j];
               _sample_r[j] = input24[i * channels * 3 + (channels - 1) * 3 + j];
            }
          if (sample_l32 & 0x00800000)
            sample_l32 |= 0xff000000;
          if (sample_r32 & 0x00800000)
            sample_r32 |= 0xff000000;
          sound(i, 0) = sample_l32 / pow (2.0, 23);
          sound(i, 1) = sample_r32 / pow (2.0, 23);
        }
    }

  octave_value_list args, retval;
  args(0) = sound;
  retval = feval (recorder->octave_callback_function, args, 1);
  return retval(0).int_value ();
}

static int
portaudio_record_callback (const void *input, void *, unsigned long frames,
                           const PaStreamCallbackTimeInfo *,
                           PaStreamCallbackFlags, void *data)
{
  audiorecorder *recorder = static_cast<audiorecorder *> (data);
  int channels = recorder->get_channels ();
  float sample_l, sample_r;
  if (recorder->get_nbits () == 8)
    {
      const int8_t *input8 = static_cast<const int8_t *> (input);
      for (unsigned long i = 0; i < frames; i++)
        {
          sample_l = input8[i * channels] / (pow (2.0, 7) - 1.0);
          sample_r = input8[i * channels + (channels - 1)] / (pow (2.0, 7) - 1.0);
          recorder->append (sample_l, sample_r);
        }
    }
  else if (recorder->get_nbits () == 16)
    {
      const int16_t *input16 = static_cast<const int16_t *> (input);
      for (unsigned long i = 0; i < frames; i++)
        {
          sample_l = input16[i * channels] / (pow (2.0, 15) - 1.0);
          sample_r = input16[i * channels + (channels - 1)] / (pow (2.0, 15) - 1.0);
          recorder->append (sample_l, sample_r);
        }
    }
  else if (recorder->get_nbits () == 24)
    {
      // FIXME: Is there a better way?
      const uint8_t *input24 = static_cast<const uint8_t *> (input);
      int32_t sample_l32, sample_r32;
      uint8_t *_sample_l = reinterpret_cast<uint8_t *> (&sample_l);
      uint8_t *_sample_r = reinterpret_cast<uint8_t *> (&sample_r);
      for (unsigned long i = 0; i < frames; i++)
        {
          for (int j = 0; j < 3; j++)
            {
              _sample_l[j] = input24[i * channels * 3 + j];
              _sample_r[j] = input24[i * channels * 3 + (channels - 1) * 3 + j];
            }
          if (sample_l32 & 0x00800000)
            sample_l32 |= 0xff000000;
          if (sample_r32 & 0x00800000)
            sample_r32 |= 0xff000000;
          recorder->append (sample_l32 / pow (2.0, 23), sample_r32 / pow (2.0, 23));
        }
    }

  if (recorder->get_sample_number () > recorder->get_end_sample ())
    return paComplete;

  return paContinue;
}

audiorecorder::audiorecorder (void)
{
  this->id = -1;
  this->sample_number = 0;
  this->channels = 1;
  this->tag = charMatrix ("");
  Matrix userdata;
  this->userdata = octave_value (userdata);
  this->stream = 0;
  this->end_sample = -1;
  this->set_fs (44100);
  this->set_nbits (16);
  this->set_channels (2);
  this->octave_callback_function = 0;
}

void
audiorecorder::print (std::ostream& os, bool pr_as_read_syntax) const
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
  PaError err;
  int device;
  err = Pa_Initialize ();
  if (err != paNoError)
    {
      error ("audiorecorder: Initialization error!");
      return;
    }

  int numDevices = Pa_GetDeviceCount ();
  if (numDevices < 0)
    {
      error ("audiorecorder: No audio devices found!");
      return;
    }

  if (this->get_id () == -1)
    device = Pa_GetDefaultInputDevice ();
  else
    device = this->get_id ();

  this->input_parameters.device = device;
  this->input_parameters.channelCount = this->get_channels ();
  this->input_parameters.sampleFormat = bits_to_format (this->get_nbits ());
  this->input_parameters.suggestedLatency = Pa_GetDeviceInfo (device)->defaultHighInputLatency;
  this->input_parameters.hostApiSpecificStreamInfo = NULL;
}

void
audiorecorder::set_fs (int fs)
{
  this->fs = fs;
}

int
audiorecorder::get_fs (void)
{
  return this->fs;
}

void
audiorecorder::set_nbits (int nbits)
{
  this->nbits = nbits;
}

int
audiorecorder::get_nbits (void)
{
  return this->nbits;
}

void
audiorecorder::set_id (int id)
{
  this->id = id;
}

int
audiorecorder::get_id (void)
{
  return this->id;
}

void
audiorecorder::set_channels (int channels)
{
  assert (channels == 1 || channels == 2);
  this->channels = channels;
}

int
audiorecorder::get_channels (void)
{
  return this->channels;
}

audio_type
audiorecorder::get_type (void)
{
  return this->type;
}

void
audiorecorder::set_sample_number (unsigned int sample_number)
{
  this->sample_number = sample_number;
}

unsigned int
audiorecorder::get_sample_number (void)
{
  return this->sample_number;
}

unsigned int
audiorecorder::get_total_samples (void)
{
  return this->left.size ();
}

void
audiorecorder::set_end_sample (unsigned int end_sample)
{
  this->end_sample = end_sample;
}

unsigned int
audiorecorder::get_end_sample (void)
{
  return this->end_sample;
}

void
audiorecorder::reset_end_sample (void)
{
  this->set_end_sample (this->left.size ());
}

void
audiorecorder::set_tag (charMatrix tag)
{
  this->tag = tag;
}

charMatrix
audiorecorder::get_tag (void)
{
  return this->tag;
}

void
audiorecorder::set_userdata (octave_value userdata)
{
  this->userdata = userdata;
}

octave_value
audiorecorder::get_userdata (void)
{
  return this->userdata;
}

octave_value
audiorecorder::getaudiodata (void)
{
  Matrix audio (2, this->left.size ());
  for (unsigned int i = 0; i < this->left.size (); i++)
    {
      audio(0, i) = this->left[i];
      audio(1, i) = this->right[i];
    }
  return octave_value (audio);
}

audioplayer *
audiorecorder::getplayer (void)
{
  audioplayer *player = new audioplayer ();
  player->set_y (this->getaudiodata ());
  player->set_fs (this->get_fs ());
  player->set_nbits (this->get_nbits ());
  player->init ();
  return player;
}

bool
audiorecorder::isrecording (void)
{
  if (this->get_stream () == 0)
    return false;

  PaError err;
  err = Pa_IsStreamActive (stream);
  if (err != 0 && err != 1)
    {
      error ("audiorecorder: Error checking stream activity status");
      return false;
    }

  return (err == 1);
}

void
audiorecorder::record (void)
{
  if (this->get_stream ())
    this->stop ();

  this->left.clear ();
  this->right.clear ();
  PaError err;
  if (this->octave_callback_function != 0)
    {
      err = Pa_OpenStream (&stream, &(this->input_parameters), NULL,
                           this->get_fs (), BUFFER_SIZE, paClipOff,
                           octave_record_callback, this);
    }
  else
    {
      err = Pa_OpenStream (&stream, &(this->input_parameters), NULL,
                           this->get_fs (), BUFFER_SIZE, paClipOff,
                           portaudio_record_callback, this);
    }
  if (err != paNoError)
    {
      error ("audiorecorder: Error opening audio recording stream");
      return;
    }
  err = Pa_StartStream (stream);
  if (err != paNoError)
    {
      error ("audiorecorder: Error starting audio recording stream");
      return;
    }
}

void
audiorecorder::recordblocking (float seconds)
{
  if (this->get_stream ())
    this->stop ();

  this->left.clear ();
  this->right.clear ();

  PaError err;
  err = Pa_OpenStream (&stream, &(this->input_parameters), NULL,
                       this->get_fs (), BUFFER_SIZE, paClipOff, NULL, this);
  if (err != paNoError)
    {
      error ("audiorecorder: Error opening audio recording stream");
      return;
    }

  err = Pa_StartStream (stream);
  if (err != paNoError)
    {
      error ("audiorecorder: Error starting audio recording stream");
      return;
    }

  unsigned int frames = seconds * this->get_fs ();
  uint8_t buffer[BUFFER_SIZE * 2 * 3];
  for (unsigned long i = 0; i < frames / BUFFER_SIZE; i++)
    {
      Pa_ReadStream (this->get_stream (), buffer, BUFFER_SIZE);
      if (this->octave_callback_function != 0)
        octave_record_callback (buffer, NULL, BUFFER_SIZE, 0, 0, this);
      else
        portaudio_record_callback (buffer, NULL, BUFFER_SIZE, 0, 0, this);
    }
}

void
audiorecorder::pause (void)
{
  if (this->get_stream () == 0)
    return;

  PaError err;
  err = Pa_StopStream (stream);
  if (err != paNoError)
    {
      error ("audiorecorder: Error stoping audio recording stream");
      return;
    }
}

void
audiorecorder::resume (void)
{
  if (this->get_stream () == 0)
    return;

  PaError err;
  err = Pa_StartStream (stream);
  if (err != paNoError)
    {
      error ("audiorecorder: Error starting audio recording stream");
      return;
    }
}

void
audiorecorder::stop (void)
{
  if (this->get_stream () == 0)
    return;

  PaError err;
  if (not Pa_IsStreamStopped (this->get_stream ()))
    {
      err = Pa_AbortStream (this->get_stream ());
      if (err != paNoError)
        {
          error ("audioplayer: Error stopping audio playback stream");
          return;
        }
    }

  err = Pa_CloseStream (stream);
  if (err != paNoError)
    {
      error ("audiorecorder: Error closing audio recording stream");
      return;
    }

  this->set_sample_number (0);
  this->reset_end_sample ();
  stream = 0;
}

void
audiorecorder::append (float sample_l, float sample_r)
{
  this->left.push_back (sample_l);
  this->right.push_back (sample_r);
  this->set_sample_number (this->get_sample_number () + 1);
}

PaStream *
audiorecorder::get_stream (void)
{
  return this->stream;
}

DEFUN_DLD (__recorder_audiorecorder__, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{recorder} =} __recorder_audiorecorder__ (@var{fs}, @var{nbits}, @var{channels})\n\
@deftypefnx {Loadable Function} {@var{recorder} =} __recorder_audiorecorder__ (@var{fs}, @var{nbits}, @var{channels}, @var{id})\n\
@deftypefnx {Loadable Function} {@var{recorder} =} __recorder_audiorecorder__ (@var{fcn}, @dots{})\n\
Undocumented internal function.\n\
@end deftypefn")
{
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  audiorecorder* retval = new audiorecorder ();
  int offset = 0;
  if (nargin > 0)
    {
      bool is_function = args(0).is_string () || args(0).is_function_handle () || args(0).is_inline_function ();
      if (is_function)
        {
          retval->octave_callback_function = args(0).function_value ();
          offset = 1;
        }
    }
  switch (nargin - offset)
     {
      case 3:
        retval->set_fs (args(0 + offset).int_value ());
        retval->set_nbits (args(1 + offset).int_value ());
        retval->set_channels (args(2 + offset).int_value ());
        break;
      case 4:
        retval->set_fs (args(0 + offset).int_value ());
        retval->set_nbits (args(1 + offset).int_value ());
        retval->set_channels (args(2 + offset).int_value ());
        retval->set_id (args(3 + offset).int_value ());
        break;
    }
  retval->init ();
  return octave_value (retval);
#else
  octave_value retval;
  error ("portaudio not found on your system and thus audio functionality is not present");
  return retval;
#endif
}

static audiorecorder *
get_recorder (const octave_value& ov)
{
  const octave_base_value& rep = ov.get_rep ();

  octave_base_value *ncrep = const_cast<octave_base_value *> (&rep);

  return dynamic_cast<audiorecorder *> (ncrep);
}

DEFUN_DLD (__recorder_getaudiodata__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{data}} __recorder_getaudiodata__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  audiorecorder *recorder = get_recorder (args(0));
  retval = octave_value (recorder->getaudiodata ());
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_get_channels__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{n} =} __recorder_get_channels__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      retval = octave_value (recorder->get_channels ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_get_fs__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{fs} =} __recorder_get_fs__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      retval = octave_value (recorder->get_fs ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_get_id__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{id} =} __recorder_get_id__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      retval = octave_value (recorder->get_id ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_get_nbits__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{nbits} =} __recorder_get_nbits__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      retval = octave_value (recorder->get_nbits ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_get_sample_number__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{n} =} __recorder_get_sample_number__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      retval = octave_value (recorder->get_sample_number ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_get_tag__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{tag} =} __recorder_get_tag__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      retval = octave_value (recorder->get_tag ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_get_total_samples__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{n} =} __recorder_get_total_samples__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      retval = octave_value (recorder->get_total_samples ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_get_userdata__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{data} =} __recorder_get_userdata__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      retval = recorder->get_userdata ();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_isrecording__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __recorder_isrecording__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      if (recorder->isrecording ())
        return octave_value (1);
      else
        return octave_value (0);
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_pause__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __recorder_pause__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      recorder->pause ();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_recordblocking__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __recorder_recordblocking__ (@var{recorder}, @var{seconds})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  audiorecorder *recorder = get_recorder (args(0));
  recorder->recordblocking (args(1).float_value ());
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_record__, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} __recorder_record__ (@var{recorder})\n\
@deftypefnx {Loadable Function} {} __recorder_record__ (@var{recorder}, @var{seconds})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  audiorecorder *recorder = get_recorder (args(0));
  if (args.length () == 1)
    {
      recorder->record ();
    }
  else if (args.length () == 2)
    {
      recorder->set_end_sample (args(1).int_value () * recorder->get_fs ());
      recorder->record ();
    }
  else
    {
      error ("audiorecorder: wrong number of arguments passed to record");
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_resume__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __recorder_resume__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder *recorder = get_recorder (args(0));
      recorder->resume ();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_set_fs__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __recorder_set_fs__ (@var{recorder}, @var{fs})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 2)
    {
      audiorecorder *recorder = get_recorder (args(0));
      recorder->set_fs (args(1).int_value ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_set_tag__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __recorder_set_tag__ (@var{recorder}, @var{tag})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 2)
    {
      audiorecorder *recorder = get_recorder (args(0));
      recorder->set_tag (args(1).char_matrix_value ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_set_userdata__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __recorder_set_userdata__ (@var{recorder}, @var{data})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 2)
    {
      audiorecorder *recorder = get_recorder (args(0));
      recorder->set_userdata (args(1));
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__recorder_stop__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __recorder_stop__ (@var{recorder})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  audiorecorder *recorder = get_recorder (args(0));
  recorder->stop ();
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_audioplayer__, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{player} =} __player_audioplayer__ (@var{y}, @var{fs})\n\
@deftypefnx {Loadable Function} {@var{player} =} __player_audioplayer__ (@var{y}, @var{fs}, @var{nbits})\n\
@deftypefnx {Loadable Function} {@var{player} =} __player_audioplayer__ (@var{y}, @var{fs}, @var{nbits}, @var{id})\n\
Undocumented internal function.\n\
@end deftypefn")
{
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  audioplayer* retval = new audioplayer ();
  bool is_function = args(0).is_string () || args(0).is_function_handle () || args(0).is_inline_function ();
  if (is_function)
    retval->set_y (args(0).function_value ());
  else
    retval->set_y (args(0));
  retval->set_fs (args(1).int_value ());
  switch (nargin)
    {
      case 3:
        retval->set_nbits (args(2).int_value ());
        break;
      case 4:
        retval->set_nbits (args(2).int_value ());
        retval->set_id (args(3).int_value ());
        break;
    }
  if (is_function)
    retval->init_fn ();
  else
    retval->init ();
  return octave_value (retval);
#else
  octave_value retval;
  error ("portaudio not found on your system and thus audio functionality is not present");
  return retval;
#endif
}

static audioplayer *
get_player (const octave_value& ov)
{
  const octave_base_value& rep = ov.get_rep ();

  octave_base_value *ncrep = const_cast<octave_base_value *> (&rep);

  return dynamic_cast<audioplayer *> (ncrep);
}

DEFUN_DLD (__player_get_channels__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{n} =} __player_get_channels__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      retval = octave_value (player->get_channels ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_get_fs__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{fs} =} __player_get_fs__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      retval = octave_value (player->get_fs ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_get_id__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{id} =} __player_get_id__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      retval = octave_value (player->get_id ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_get_nbits__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{nbits} =} __player_get_nbits__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      retval = octave_value (player->get_nbits ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_get_sample_number__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{n} =} __player_get_sample_number__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      retval = octave_value (player->get_sample_number ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_get_tag__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{tag} =} __player_get_tag__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      retval = octave_value (player->get_tag ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_get_total_samples__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{n} =} __player_get_total_samples__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      retval = octave_value (player->get_total_samples ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_get_userdata__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{data} =} __player_get_userdata__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      retval = player->get_userdata ();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_isplaying__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __player_isplaying__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      if (player->isplaying ())
        return octave_value (1);
      else
        return octave_value (0);
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_pause__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __player_pause__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      player->pause ();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_playblocking__, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} __player_playblocking__ (@var{player})\n\
@deftypefnx {Loadable Function} {} __player_playblocking__ (@var{player}, @var{start})\n\
@deftypefnx {Loadable Function} {} __player_playblocking__ (@var{player}, [@var{start}, @var{end}])\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      player->playblocking ();
    }
  else
    {
      audioplayer *player = get_player (args(0));
      if (args(1).is_matrix_type ())
        {
          unsigned int start, end;
          RowVector range = args(1).row_vector_value ();
          start = range.elem (0) - 1;
          end = range.elem (1) - 1;
          if (start > player->get_total_samples ()
              || start > end || end > player->get_total_samples ())
            error ("audioplayer: invalid range specified for playback");
          player->set_sample_number (start);
          player->set_end_sample (end);
        }
      else
        {
          unsigned int start;
          start = args(1).int_value () - 1;
          if (start > player->get_total_samples ())
            error ("audioplayer: invalid range specified for playback");
          player->set_sample_number (start);
        }
      player->playblocking ();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_play__, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} __player_play__ (@var{player})\n\
@deftypefnx {Loadable Function} {} __player_play__ (@var{player}, @var{start})\n\
@deftypefnx {Loadable Function} {} __player_play__ (@var{player}, [@var{start}, @var{end}])\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      player->play ();
    }
  else
    {
      audioplayer *player = get_player (args(0));
      if (args(1).is_matrix_type ())
        {
          unsigned int start, end;
          RowVector range = args(1).row_vector_value ();
          start = range.elem (0) - 1;
          end = range.elem (1) - 1;
          if (start > player->get_total_samples ()
              || start > end || end > player->get_total_samples ())
            error ("audioplayer: invalid range specified for playback");
          player->set_sample_number (start);
          player->set_end_sample (end);
        }
      else
        {
          unsigned int start;
          start = args(1).int_value () - 1;
          if (start > player->get_total_samples ())
            error ("audioplayer: invalid range specified for playback");
          player->set_sample_number (start);
        }
      player->play ();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_resume__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __player_resume__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args(0));
      player->resume ();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_set_fs__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __player_set_fs__ (@var{player}, @var{fs})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 2)
    {
      audioplayer *player = get_player (args(0));
      player->set_fs (args(1).int_value ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_set_tag__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __player_set_tag__ (@var{player}, @var{tag})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 2)
    {
      audioplayer *player = get_player (args(0));
      player->set_tag (args(1).char_matrix_value ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_set_userdata__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __player_set_userdata__ (@var{player}, @var{data})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 2)
    {
      audioplayer *player = get_player (args(0));
      player->set_userdata (args(1));
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD (__player_stop__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __player_stop__ (@var{player})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audioplayer *player = get_player (args (0));
      player->stop ();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}
