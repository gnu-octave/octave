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

#include "oct.h"
#include "ov-struct.h"

#ifdef HAVE_PORTAUDIO
#include "player_class.cc"
#include "recorder_class.cc"
#endif
   
DEFUN_DLD(__recorder_audiorecorder__, args, ,
"__recorder_audiorecorder__"
)
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
          retval->octave_callback_function = args (0).function_value ();
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
  retval->init();
  return octave_value(retval);
#else
  octave_value retval;
  error ("portaudio not found on your system and thus audio functionality is not present");
  return retval;
#endif
}
   
DEFUN_DLD(__recorder_getaudiodata__, args, ,
"__recorder_getaudiodata__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  audiorecorder* recorder = 0;
  const octave_base_value& rep = args (0).get_rep ();
  recorder = &((audiorecorder &)rep);
  retval = octave_value (recorder->getaudiodata ());
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_get_channels__, args, ,
"__recorder_get_channels__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_channels());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_get_fs__, args, ,
"__recorder_get_fs__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_fs());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_get_id__, args, ,
"__recorder_get_id__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_id());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_get_nbits__, args, ,
"__recorder_get_nbits__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_nbits ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_get_sample_number__, args, ,
"__recorder_get_sample_number__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_sample_number());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_get_tag__, args, ,
"__recorder_get_tag__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value (recorder->get_tag ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_get_total_samples__, args, ,
"__recorder_get_total_samples__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_total_samples());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_get_userdata__, args, ,
"__recorder_get_userdata__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = recorder->get_userdata ();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_isrecording__, args, ,
"__recorder_isrecording__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      if (recorder->isrecording())
        {
          return octave_value(1);
        }
      else
        {
          return octave_value(0);
        }
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_pause__, args, ,
"__recorder_pause__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      recorder->pause();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_recordblocking__, args, ,
"__recorder_recordblocking__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  audiorecorder* recorder = 0;
  const octave_base_value& rep = args (0).get_rep ();
  recorder = &((audiorecorder &)rep);
  recorder->recordblocking (args (1).float_value ());
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_record__, args, ,
"__recorder_record__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  audiorecorder* recorder = 0;
  const octave_base_value& rep = args (0).get_rep ();
  recorder = &((audiorecorder &)rep);
  if (args.length () == 1)
    {
      recorder->record ();
    }
  else if (args.length () == 2)
    {
      recorder->set_end_sample (args (1).int_value () * recorder->get_fs ());
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

DEFUN_DLD(__recorder_resume__, args, ,
"__recorder_resume__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      recorder->resume();
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_set_fs__, args, ,
"__recorder_set_fs__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 2)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      recorder->set_fs (args (1).int_value());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_set_tag__, args, ,
"__recorder_set_tag__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 2)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      recorder->set_tag (args(1).char_matrix_value ());
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_set_userdata__, args, ,
"__recorder_set_userdata__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  int nargin = args.length ();
  if (nargin == 2)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      recorder->set_userdata (args(1));
    }
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}

DEFUN_DLD(__recorder_stop__, args, ,
"__recorder_stop__"
)
{
  octave_value retval;
#ifdef HAVE_PORTAUDIO
  audiorecorder* recorder = 0;
  const octave_base_value& rep = args (0).get_rep ();
  recorder = &((audiorecorder &)rep);
  recorder->stop();
#else
  error ("portaudio not found on your system and thus audio functionality is not present");
#endif
  return retval;
}
