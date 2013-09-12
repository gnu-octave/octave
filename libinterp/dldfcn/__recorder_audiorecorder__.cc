#include "oct.h"
#include "ov-struct.h"

#include "player_class.cc"
#include "recorder_class.cc"
   
DEFUN_DLD(__recorder_audiorecorder__, args, ,
"__recorder_audiorecorder__"
)
{
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
}
   
DEFUN_DLD(__recorder_getaudiodata__, args, ,
"__recorder_getaudiodata__"
)
{
  octave_value retval;
  audiorecorder* recorder = 0;
  const octave_base_value& rep = args (0).get_rep ();
  recorder = &((audiorecorder &)rep);
  retval = octave_value (recorder->getaudiodata ());
  return retval;
}

DEFUN_DLD(__recorder_get_channels__, args, ,
"__recorder_get_channels__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_channels());
    }
  return retval;
}

DEFUN_DLD(__recorder_get_fs__, args, ,
"__recorder_get_fs__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_fs());
    }
  return retval;
}

DEFUN_DLD(__recorder_get_id__, args, ,
"__recorder_get_id__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_id());
    }
  return retval;
}

DEFUN_DLD(__recorder_get_nbits__, args, ,
"__recorder_get_nbits__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_nbits ());
    }
  return retval;
}

DEFUN_DLD(__recorder_get_sample_number__, args, ,
"__recorder_get_sample_number__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_sample_number());
    }
  return retval;
}

DEFUN_DLD(__recorder_get_tag__, args, ,
"__recorder_get_tag__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value (recorder->get_tag ());
    }
  return retval;
}

DEFUN_DLD(__recorder_get_total_samples__, args, ,
"__recorder_get_total_samples__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = octave_value(recorder->get_total_samples());
    }
  return retval;
}

DEFUN_DLD(__recorder_get_userdata__, args, ,
"__recorder_get_userdata__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      retval = recorder->get_userdata ();
    }
  return retval;
}

DEFUN_DLD(__recorder_isrecording__, args, ,
"__recorder_isrecording__"
)
{
  octave_value retval;
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
  return retval;
}

DEFUN_DLD(__recorder_pause__, args, ,
"__recorder_pause__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      recorder->pause();
    }
  return retval;
}

DEFUN_DLD(__recorder_recordblocking__, args, ,
"__recorder_recordblocking__"
)
{
  octave_value retval;
  audiorecorder* recorder = 0;
  const octave_base_value& rep = args (0).get_rep ();
  recorder = &((audiorecorder &)rep);
  recorder->recordblocking (args (1).float_value ());
  return retval;
}

DEFUN_DLD(__recorder_record__, args, ,
"__recorder_record__"
)
{
  octave_value retval;
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
  return retval;
}

DEFUN_DLD(__recorder_resume__, args, ,
"__recorder_resume__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 1)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      recorder->resume();
    }
  return retval;
}

DEFUN_DLD(__recorder_set_fs__, args, ,
"__recorder_set_fs__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 2)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      recorder->set_fs (args (1).int_value());
    }
  return retval;
}

DEFUN_DLD(__recorder_set_tag__, args, ,
"__recorder_set_tag__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 2)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      recorder->set_tag (args(1).char_matrix_value ());
    }
  return retval;
}

DEFUN_DLD(__recorder_set_userdata__, args, ,
"__recorder_set_userdata__"
)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 2)
    {
      audiorecorder* recorder = 0;
      const octave_base_value& rep = args (0).get_rep ();
      recorder = &((audiorecorder &)rep);
      recorder->set_userdata (args(1));
    }
  return retval;
}

DEFUN_DLD(__recorder_stop__, args, ,
"__recorder_stop__"
)
{
  octave_value retval;
  audiorecorder* recorder = 0;
  const octave_base_value& rep = args (0).get_rep ();
  recorder = &((audiorecorder &)rep);
  recorder->stop();
  return retval;
}
