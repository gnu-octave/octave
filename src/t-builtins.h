// builtin text function support.                          -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_t_builtins_h)
#define octave_t_builtins_h 1

#include "tree-const.h"

struct builtin_text_functions
{
  char *name;
  int nargin_max;
  Text_fcn text_fcn;
  char *help_string;
};

extern Octave_object builtin_casesen (int argc, char **argv, int nargout);
extern Octave_object builtin_cd (int argc, char **argv, int nargout);
extern Octave_object builtin_clear (int argc, char **argv, int nargout);
extern Octave_object builtin_document (int argc, char **argv, int nargout);
extern Octave_object builtin_edit_history (int argc, char **argv,
					    int nargout);
extern Octave_object builtin_format (int argc, char **argv, int nargout);
extern Octave_object builtin_help (int argc, char **argv, int nargout);
extern Octave_object builtin_history (int argc, char **argv, int nargout);
extern Octave_object builtin_hold (int argc, char **argv, int nargout);
extern Octave_object builtin_load (int argc, char **argv, int nargout);
extern Octave_object builtin_ls (int argc, char **argv, int nargout);
extern Octave_object builtin_run_history (int argc, char **argv, int nargout);
extern Octave_object builtin_save (int argc, char **argv, int nargout);
extern Octave_object builtin_set (int argc, char **argv, int nargout);
extern Octave_object builtin_show (int argc, char **argv, int nargout);
extern Octave_object builtin_who (int argc, char **argv, int nargout);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
