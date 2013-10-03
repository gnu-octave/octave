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

#ifndef PLAYER_CLASS_H
#define PLAYER_CLASS_H

#include <string>
#include "oct.h"
#include "ov-int32.h"
#include <portaudio.h>

#include "player_class.h"

enum audio_type { INT8, UINT8, INT16, DOUBLE };

class audioplayer : public octave_base_value
{
public:
  audioplayer (void);
  ~audioplayer (void) {};

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

#endif // PLAYER_CLASS_H
