// utils.h                                               -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if !defined (octave_utils_h)
#define octave_utils_h 1

class istream;
class ostrstream;
class tree_constant;
class Octave_object;

extern char *strsave (const char *);
extern char *strconcat (const char *, const char *);

extern void discard_until (istream&, char);

#if 0
extern char *read_until (istream&, char);
#endif

extern char *search_path_for_file (const char *, const char *);
extern char *file_in_path (const char *, const char *);
extern char *fcn_file_in_path (const char *);
extern char *oct_file_in_path (const char *);

extern char *octave_tmp_file_name (void);

extern char **pathstring_to_vector (char *pathstring);

extern void jump_to_top_level (void) NORETURN;

extern int almost_match (const char *std, const char *s,
			 int min_match_len = 1, int case_sens = 1);
extern int keyword_almost_match (const char **std, int *min_len,
				 const char *s, int min_toks_to_match,
				 int max_toks);

extern char **get_fcn_file_names (int& ffl_len, const char *dir,
				  int no_suffix); 
extern char **get_fcn_file_names (int& ffl_len, int no_suffix);

extern int NINT (double x);
extern double D_NINT (double x);

extern char **make_argv (const Octave_object& args, const char *fcn_name);

extern int empty_arg (const char *name, int nr, int nc);

extern ostrstream& list_in_columns (ostrstream& os, char **list);

extern char *undo_string_escape (char c);
extern char *undo_string_escapes (const char *s);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
