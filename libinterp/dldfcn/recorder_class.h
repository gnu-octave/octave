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

#ifndef RECORDER_CLASS_H
#define RECORDER_CLASS_H

#include <vector>

#include "oct.h"
#include "ov-int32.h"
#include <portaudio.h>

#include "player_class.h"

class audiorecorder : public octave_base_value
{
public:
  audiorecorder (void);
  ~audiorecorder (void) {};

  // Overloaded base functions
  double player_value (void) const { return 0; }
  virtual double scalar_value (bool frc_str_conv = false) const { return 0; }
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

#endif // RECORDER_CLASS_H
