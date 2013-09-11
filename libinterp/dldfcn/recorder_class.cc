#include "oct.h"
#include "ov.h"
#include "parse.h"
#include <portaudio.h>
#include <stdint.h>

#include "player_class.h"
#include "recorder_class.h"

PaSampleFormat bits_to_format(int bits)
{
  if (bits == 8)
    {
      return paInt8;
    }
  else if (bits == 16)
    {
      return paInt16;
    }
  else if (bits == 24)
    {
      return paInt24;
    }
  else if (bits == 32)
    {
      return paInt32;
    }
  else if (bits == -1)
    {
      return paFloat32;
    }
  else 
    {
      return 0;
    }
}

#define BUFFER_SIZE 512

DEFINE_OCTAVE_ALLOCATOR (audiorecorder);
DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (audiorecorder, "audiorecorder", "audiorecorder");

static int octave_record_callback (const void *input, void *output,
                                   unsigned long frames,
                                   const PaStreamCallbackTimeInfo* time,
                                   PaStreamCallbackFlags status,
                                   void *data)
{
  audiorecorder *recorder = (audiorecorder *)data;
  int channels = recorder->get_channels ();
  int return_status;
  float sample_l, sample_r;
  Matrix sound;
  sound.resize(frames, 2);
  if (recorder->get_nbits () == 8)
    {
      int8_t *input8 = (int8_t *)input;
      for (int i = 0; i < frames; i++)
        {
          sample_l = input8[i * channels] / (pow(2.0, 7) - 1.0);
          sample_r = input8[i * channels + (channels - 1)] / (pow(2.0, 7) - 1.0);
          sound(i, 0) = sample_l;
          sound(i, 1) = sample_r;
        }
      }
  else if (recorder->get_nbits () == 16)
    {
      int16_t *input16 = (int16_t *)input;
      for (int i = 0; i < frames; i++)
        {
          sample_l = input16[i * channels] / (pow(2.0, 15) - 1.0);
          sample_r = input16[i * channels + (channels - 1)] / (pow(2.0, 15) - 1.0);
          sound(i, 0) = sample_l;
          sound(i, 1) = sample_r;          
        }
    }
  else if (recorder->get_nbits () == 24)
    {
      uint8_t *input24 = (uint8_t *)input;
      int32_t sample_l32, sample_r32;
      uint8_t *_sample_l = (uint8_t *)&sample_l;
      uint8_t *_sample_r = (uint8_t *)&sample_r;
      for (int i = 0; i < frames; i++)
        {
          for (int j = 0; j < 3; j++)
            {
              _sample_l[j] = input24[i * channels * 3 + j];
              _sample_r[j] = input24[i * channels * 3 + (channels - 1) * 3 + j];
            }
          if (sample_l32 & 0x00800000)
            {
              sample_l32 |= 0xff000000;
            }
          if (sample_r32 & 0x00800000)
            {
              sample_r32 |= 0xff000000;
            }
          sound(i, 0) = sample_l32 / pow(2.0, 23);
          sound(i, 1) = sample_r32 / pow(2.0, 23);
        }
    }
    octave_value_list args, retval;
    args(0) = sound;
    retval = feval (recorder->octave_callback_function, args, 1);
    return_status = retval(0).int_value ();
    return return_status;
}

static int portaudio_record_callback (const void *input, void *output,
                               unsigned long frames,
                               const PaStreamCallbackTimeInfo* time,
                               PaStreamCallbackFlags status,
                               void *data)
{
  audiorecorder *recorder = (audiorecorder *)data;
  int channels = recorder->get_channels ();
  float sample_l, sample_r;
  if (recorder->get_nbits () == 8)
    {
      int8_t *input8 = (int8_t *)input;
      for (int i = 0; i < frames; i++)
        {
          sample_l = input8[i * channels] / (pow(2.0, 7) - 1.0);
          sample_r = input8[i * channels + (channels - 1)] / (pow(2.0, 7) - 1.0);
          recorder->append(sample_l, sample_r);
        }
      }
  else if (recorder->get_nbits () == 16)
    {
      int16_t *input16 = (int16_t *)input;
      for (int i = 0; i < frames; i++)
        {
          sample_l = input16[i * channels] / (pow(2.0, 15) - 1.0);
          sample_r = input16[i * channels + (channels - 1)] / (pow(2.0, 15) - 1.0);
          recorder->append(sample_l, sample_r);
        }
    }
  else if (recorder->get_nbits () == 24)
    {
      uint8_t *input24 = (uint8_t *)input;
      int32_t sample_l32, sample_r32;
      uint8_t *_sample_l = (uint8_t *)&sample_l;
      uint8_t *_sample_r = (uint8_t *)&sample_r;
      for (int i = 0; i < frames; i++)
        {
          for (int j = 0; j < 3; j++)
            {
              _sample_l[j] = input24[i * channels * 3 + j];
              _sample_r[j] = input24[i * channels * 3 + (channels - 1) * 3 + j];
            }
          if (sample_l32 & 0x00800000)
            {
              sample_l32 |= 0xff000000;
            }
          if (sample_r32 & 0x00800000)
            {
              sample_r32 |= 0xff000000;
            }
          recorder->append(sample_l32 / pow(2.0, 23), sample_r32 / pow(2.0, 23));
        }
    }
    if (recorder->get_sample_number () > recorder->get_end_sample ())
      {
        return paComplete;
      }
    return paContinue;
}

audiorecorder::audiorecorder ()
{
  this->id = -1;
  this->sample_number = 0;
  this->channels = 1;
  this->tag = charMatrix ("");
  Matrix userdata;
  this->userdata = octave_value (userdata);
  this->stream = 0;
  this->end_sample = -1;
  this->set_fs(44100);
  this->set_nbits(16);
  this->set_channels(2);
  this->octave_callback_function = 0;
}

audiorecorder::~audiorecorder ()
{

}

void audiorecorder::print (std::ostream& os, bool pr_as_read_syntax ) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void audiorecorder::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  os << 0;
}

void audiorecorder::init ()
{
  PaError err;
  int device;
  err = Pa_Initialize ();
  if (err != paNoError) 
    { 
      error ("audiorecorder: Initialization error!");
      return;
    }
  int numDevices;
  numDevices = Pa_GetDeviceCount ();
  if (numDevices < 0) 
    {
      error ("audiorecorder: No audio devices found!");
      return;
    }
  if (this->get_id () == -1)
    {
      device = Pa_GetDefaultInputDevice ();
    }  
  else 
    {
      device = this->get_id ();
    }
  this->input_parameters.device = device;
  this->input_parameters.channelCount = this->get_channels ();
  this->input_parameters.sampleFormat = bits_to_format (this->get_nbits ());
  this->input_parameters.suggestedLatency = Pa_GetDeviceInfo (device)->defaultHighInputLatency;
  this->input_parameters.hostApiSpecificStreamInfo = NULL;
}

void audiorecorder::set_fs (int fs)
{
  this->fs = fs;
}

int audiorecorder::get_fs ()
{
  return this->fs;
}

void audiorecorder::set_nbits (int nbits)
{
  this->nbits = nbits;
}

int audiorecorder::get_nbits ()
{
  return this->nbits;
}

void audiorecorder::set_id (int id)
{
  this->id = id;
}

int audiorecorder::get_id ()
{
  return this->id;
}

void audiorecorder::set_channels (int channels)
{
  assert(channels == 1 or channels == 2);
  this->channels = channels;
}

int audiorecorder::get_channels ()
{
  return this->channels;
}

audio_type audiorecorder::get_type ()
{
  return this->type;
}

void audiorecorder::set_sample_number (unsigned int sample_number)
{
  this->sample_number = sample_number;
}

unsigned int audiorecorder::get_sample_number ()
{
  return this->sample_number;
}

unsigned int audiorecorder::get_total_samples ()
{
  return this->left.size ();
}

void audiorecorder::set_end_sample (unsigned int end_sample)
{
  this->end_sample = end_sample;
}

unsigned int audiorecorder::get_end_sample ()
{
  return this->end_sample;
}

void audiorecorder::reset_end_sample ()
{
  this->set_end_sample (this->left.size ());
}

void audiorecorder::set_tag (charMatrix tag)
{
  this->tag = tag;
}

charMatrix audiorecorder::get_tag ()
{
  return this->tag;
}

void audiorecorder::set_userdata (octave_value userdata)
{
  this->userdata = userdata;
}

octave_value audiorecorder::get_userdata ()
{
  return this->userdata;
}

octave_value audiorecorder::getaudiodata()
{
  Matrix audio (2, this->left.size());
  for (int i = 0; i < this->left.size(); i++)
    {
      audio(0, i) = this->left[i];
      audio(1, i) = this->right[i];
    }
  return octave_value (audio);
}

audioplayer *audiorecorder::getplayer ()
{
  audioplayer *player = new audioplayer ();
  player->set_y(this->getaudiodata ());
  player->set_fs(this->get_fs ());
  player->set_nbits(this->get_nbits ());
  player->init();
  return player;
}

bool audiorecorder::isrecording()
{
  if (this->get_stream() == 0)
    {
      return false;
    }
  PaError err;
  err = Pa_IsStreamActive (stream);
  if (err != 0 and err != 1) 
    {
      error ("audiorecorder: Error checking stream activity status");
      return false;
    }
  return bool(err);
}

void audiorecorder::record()
{
  if (this->get_stream())
    {
      this->stop ();
    }
  this->left.clear ();
  this->right.clear ();
  PaError err;
  if (this->octave_callback_function != 0)
    {
      err = Pa_OpenStream (&stream, &(this->input_parameters), NULL, this->get_fs (), BUFFER_SIZE, paClipOff, octave_record_callback, (void *)this);
    }
  else
    {
      err = Pa_OpenStream (&stream, &(this->input_parameters), NULL, this->get_fs (), BUFFER_SIZE, paClipOff, portaudio_record_callback, (void *)this);
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

void audiorecorder::recordblocking (float seconds)
{
  if (this->get_stream ())
    {
      this->stop ();
    }
  this->left.clear ();
  this->right.clear ();
  PaError err;
  err = Pa_OpenStream (&stream, &(this->input_parameters), NULL, this->get_fs (), BUFFER_SIZE, paClipOff, NULL, (void *)this);
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
  for (int i = 0; i < frames / BUFFER_SIZE; i++)
    {
      Pa_ReadStream(this->get_stream (), (void *)buffer, BUFFER_SIZE);
      if (this->octave_callback_function != 0)
        {
          octave_record_callback((void *)buffer, NULL, BUFFER_SIZE, 0, 0, (void *)this);
        }
      else
        {
          portaudio_record_callback((void *)buffer, NULL, BUFFER_SIZE, 0, 0, (void *)this);
        }
    }
}

void audiorecorder::pause ()
{
  if (this->get_stream () == 0)
    {
      return;
    }
  PaError err;
  err = Pa_StopStream (stream);
  if (err != paNoError) 
    {
      error ("audiorecorder: Error stoping audio recording stream");
      return;
    } 
}

void audiorecorder::resume()
{
  if (this->get_stream() == 0)
    {
      return;
    }
  PaError err;
  err = Pa_StartStream (stream);
  if (err != paNoError) 
    {
      error ("audiorecorder: Error starting audio recording stream");
      return;
    }
}

void audiorecorder::stop()
{
  if (this->get_stream() == 0)
    {
      return;
    }
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

void audiorecorder::append (float sample_l, float sample_r)
{
  this->left.push_back (sample_l);
  this->right.push_back (sample_r);
  this->set_sample_number (this->get_sample_number () + 1);
}

PaStream *audiorecorder::get_stream()
{
  return this->stream;
}
