// utils.h                                               -*- C++ -*-
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

#if !defined (_utils_h)
#define _utils_h 1

#include <time.h>

class istream;
class ostrstream;
class tree_constant;

// Tell g++ that jump_to_top_level doesn't return;

#ifdef __GNUG__
typedef void v_fcn_v (void);
volatile v_fcn_v jump_to_top_level;
#endif

extern char *strsave (const char *);
extern char *strconcat (const char *, const char *);
extern void discard_until (istream&, char);
extern void check_dimensions (int& nr, int& nc, const char *warnfor);
extern void raw_mode (int);
extern int kbhit (void);
extern char **pathstring_to_vector (char *);
extern char *octave_home (void);
extern char *octave_lib_dir (void);
extern char *octave_info_dir (void);
extern char *default_path (void);
extern char *default_info_file (void);
extern char *default_editor (void);
extern char *get_site_defaults (void);
extern char *default_pager (void);
extern char *file_in_path (const char *, const char *);
extern char *m_file_in_path (const char *);
extern char *polite_directory_format (const char *);
extern int absolute_pathname (const char *);
extern int absolute_program (const char *);
extern char *base_pathname (char *);
extern int read_octal (const char *);
extern char *sub_append_string (char *, char *, int *, int *);
extern char *decode_prompt_string (const char *);
extern void pathname_backup (char *, int);
extern char *make_absolute (const char *, const char *);
extern char *get_working_directory (const char *);
extern int change_to_directory (const char *);
extern int is_newer (const char *, time_t);
extern void jump_to_top_level (void);
extern char *s_plural (int);
extern char *es_plural (int);
extern char *save_in_tmp_file (tree_constant& t, int nd = 2, int para = 0); 
extern void mark_for_deletion (const char *);
extern void cleanup_tmp_files (void);
extern int send_to_plot_stream (const char *cmd);
extern void close_plot_stream (void);
extern int almost_match (const char *std, const char *s,
			 int min_match_len = 1, int case_sens = 1);
extern int keyword_almost_match (const char **std, int *min_len,
				 const char *s, int min_toks_to_match,
				 int max_toks);
extern char **get_m_file_names (int& mfl_len, const char *dir, int no_suffix);
extern char **get_m_file_names (int& mfl_len, int no_suffix);
extern int NINT (double x);
extern double D_NINT (double x);
extern void delete_ppchar (char **);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
