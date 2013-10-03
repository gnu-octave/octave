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
#include "ov.h"
#include "parse.h"
#include <portaudio.h>
#include <stdint.h>

#include "player_class.h"

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
octave_play_callback (const void *input, void *output,
                      unsigned long frames,
                      const PaStreamCallbackTimeInfo *time,
                      PaStreamCallbackFlags status,
                      void *data)
{
  audioplayer *player = (audioplayer *)data;
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
      for (int i = 0; i < frames; i++)
        {
          sound_l(i) = sound(i, 0);
          sound_r(i) = sound(i, 0);
        }
    }
  else if (sound.cols () == 2)
    {
      for (int i = 0; i < frames; i++)
        {
          sound_l(i) = sound(i, 0);
          sound_r(i) = sound(i, 1);
        }
    }
  else
    return paAbort;

  for (int i = 0; i < frames; i++)
    {
      if (player->get_nbits () == 8)
        {
          int8_t *buffer = (int8_t *)output;
          buffer[2 * i] = sound_l.elem (i) * (pow (2.0, 7) - 1);
          buffer[2 * i + 1] = sound_r.elem (i) * (pow (2.0, 7) - 1);
        }
      else if (player->get_nbits () == 16)
        {
          int16_t *buffer = (int16_t *)output;
          buffer[2 * i] = sound_l.elem (i) * (pow (2.0, 15) - 1);
          buffer[2 * i + 1] = sound_r.elem (i) * (pow (2.0, 15) - 1);
        }
      else if (player->get_nbits () == 24)
        {
          uint8_t *buffer = (uint8_t *)output;
          int32_t sample_l = sound_l.elem (i) * (pow (2.0, 23) - 1);
          int32_t sample_r = sound_r.elem (i) * (pow (2.0, 23) - 1);
          sample_l &= 0x00ffffff;
          sample_r &= 0x00ffffff;
          uint8_t *_sample_l = (uint8_t *)&sample_l;
          uint8_t *_sample_r = (uint8_t *)&sample_r;
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
portaudio_play_callback (const void *input, void *output,
                         unsigned long frames,
                         const PaStreamCallbackTimeInfo* time,
                         PaStreamCallbackFlags status,
                         void *data)
{
  audioplayer *player = (audioplayer *)data;
  int big_endian = is_big_endian ();
  int channels = player->get_channels ();
  RowVector *sound_l = player->get_left ();
  RowVector *sound_r;

  if (channels > 1)
    sound_r = player->get_right ();
  else
    sound_r = sound_l;

  for (int j = 0, k = 0; j < frames; j++, k += 2)
    {
      unsigned int sample_number = player->get_sample_number ();
      if (sample_number > player->get_end_sample ())
        return paAbort;

      if (player->get_type () == DOUBLE)
        {
          if (player->get_nbits () == 8)
            {
              int8_t *buffer = (int8_t *)output;
              buffer[k] = sound_l->elem (sample_number) * (pow (2.0, 7) - 1);
              buffer[k + 1] = sound_r->elem (sample_number) * (pow (2.0, 7) - 1);
            }
          else if (player->get_nbits () == 16)
            {
              int16_t *buffer = (int16_t *)output;
              buffer[k] = sound_l->elem (sample_number) * (pow (2.0, 15) - 1);
              buffer[k + 1] = sound_r->elem (sample_number) * (pow (2.0, 15) - 1);
            }
          else if (player->get_nbits () == 24)
            {
              uint8_t *buffer = (uint8_t *)output;
              int32_t sample_l = sound_l->elem (sample_number) * (pow (2.0, 23) - 1);
              int32_t sample_r = sound_r->elem (sample_number) * (pow (2.0, 23) - 1);
              sample_l &= 0x00ffffff;
              sample_r &= 0x00ffffff;
              uint8_t *_sample_l = (uint8_t *)&sample_l;
              uint8_t *_sample_r = (uint8_t *)&sample_r;
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
          int8_t *buffer = (int8_t *)output;
          buffer[k] = sound_l->elem (sample_number);
          buffer[k + 1] = sound_r->elem (sample_number);
        }
      else if (player->get_type () == UINT8)
        {
          uint8_t *buffer = (uint8_t *)output;
          buffer[k] = sound_l->elem (sample_number);
          buffer[k + 1] = sound_r->elem (sample_number);
        }
      else if (player->get_type () == INT16)
        {
          int16_t *buffer = (int16_t *)output;
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
audioplayer::print_raw (std::ostream& os, bool pr_as_read_syntax) const
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
  int channels = this->y.rows ();
  RowVector *sound_l = this->get_left ();
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
  for (int i = start; i < end; i += BUFFER_SIZE)
    {
      if (this->octave_callback_function != 0)
        octave_play_callback (0, (void *)buffer, BUFFER_SIZE, 0, 0, (void *)this);
      else
        portaudio_play_callback (0, (void *)buffer, BUFFER_SIZE, 0, 0, (void *)this);
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
    err = Pa_OpenStream (&stream, NULL, &(this->output_parameters), this->get_fs (), BUFFER_SIZE, paClipOff, octave_play_callback, (void *)this);
  else
    err = Pa_OpenStream (&stream, NULL, &(this->output_parameters), this->get_fs (), BUFFER_SIZE, paClipOff, portaudio_play_callback, (void *)this);

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
  if (err != 0 and err != 1)
    {
      error ("audiorecorder: Error checking stream activity status");
      return false;
    }

  return (err == 1);
}
