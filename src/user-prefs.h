/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_user_prefs_h)
#define octave_user_prefs_h 1

#include <string>

struct user_preferences
{
  int echo_executing_commands;
  int history_size;
  int ignore_function_time_stamp;
  int print_answer_id_name;
  int read_only_constants;
  int save_precision;
  int saving_history;
  int suppress_verbose_help_message;
  int treat_neg_dim_as_zero;
  int warn_divide_by_zero;

  string default_save_format;
  string editor;
  string exec_path;
  string history_file;
  string imagepath;
  string info_file;
  string info_prog;
  string loadpath;
  string pwd;
};

extern user_preferences user_pref;

extern void init_user_prefs (void);

extern int echo_executing_commands (void);
extern int history_size (void);
extern int ignore_function_time_stamp (void);
extern int print_answer_id_name (void);
extern int read_only_constants (void);
extern int saving_history (void);
extern int suppress_verbose_help_message (void);
extern int treat_neg_dim_as_zero (void);
extern int warn_divide_by_zero (void);

extern int set_save_precision (void);

extern int sv_default_save_format (void);
extern int sv_editor (void);
extern int sv_exec_path (void);
extern int sv_history_file (void);
extern int sv_imagepath (void);
extern int sv_info_file (void);
extern int sv_info_prog (void);
extern int sv_loadpath (void);
extern int sv_pwd (void);

enum echo_state
{
  ECHO_OFF = 0,
  ECHO_SCRIPTS = 1,
  ECHO_FUNCTIONS = 2,
  ECHO_CMD_LINE = 4
};

extern int check_preference (const string& var);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
