// user-prefs.h                                              -*- C++ -*-
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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_user_prefs_h)
#define octave_user_prefs_h 1

struct user_preferences
{
  int automatic_replot;
  int define_all_return_values;
  int do_fortran_indexing;
  int empty_list_elements_ok;
  int ignore_function_time_stamp;
  int implicit_str_to_num_ok;
  int ok_to_lose_imaginary_part;
  int output_max_field_width;
  int output_precision;
  int page_screen_output;
  int prefer_column_vectors;
  int prefer_zero_one_indexing;
  int print_answer_id_name;
  int print_empty_dimensions;
  int propagate_empty_matrices;
  int read_only_constants;
  int resize_on_range_error;
  int return_last_computed_value;
  int save_precision;
  int silent_functions;
  int split_long_rows;
  int struct_levels_to_print;
  int suppress_verbose_help_message;
  int treat_neg_dim_as_zero;
  int warn_assign_as_truth_value;
  int warn_comma_in_global_decl;
  int warn_divide_by_zero;
  int warn_function_name_clash;
  int whitespace_in_literal_matrix;

  char *default_save_format;
  char *editor;
  char *gnuplot_binary;
  char *imagepath;
  char *info_file;
  char *loadpath;
  char *pager_binary;
  char *ps1;
  char *ps2;
  char *ps4;
  char *pwd;
};

extern user_preferences user_pref;

extern void init_user_prefs (void);

extern int automatic_replot (void);
extern int define_all_return_values (void);
extern int do_fortran_indexing (void);
extern int empty_list_elements_ok (void);
extern int ignore_function_time_stamp (void);
extern int implicit_str_to_num_ok (void);
extern int ok_to_lose_imaginary_part (void);
extern int page_screen_output (void);
extern int prefer_column_vectors (void);
extern int prefer_zero_one_indexing (void);
extern int print_answer_id_name (void);
extern int print_empty_dimensions (void);
extern int propagate_empty_matrices (void);
extern int read_only_constants (void);
extern int resize_on_range_error (void);
extern int return_last_computed_value (void);
extern int silent_functions (void);
extern int split_long_rows (void);
extern int struct_levels_to_print (void);
extern int suppress_verbose_help_message (void);
extern int treat_neg_dim_as_zero (void);
extern int warn_assign_as_truth_value (void);
extern int warn_comma_in_global_decl (void);
extern int warn_divide_by_zero (void);
extern int warn_function_name_clash (void);
extern int whitespace_in_literal_matrix (void);

extern int set_output_max_field_width (void);
extern int set_output_precision (void);
extern int set_save_precision (void);

extern int sv_default_save_format (void);
extern int sv_editor (void);
extern int sv_gnuplot_binary (void);
extern int sv_imagepath (void);
extern int sv_info_file (void);
extern int sv_loadpath (void);
extern int sv_pager_binary (void);
extern int sv_ps1 (void);
extern int sv_ps2 (void);
extern int sv_ps4 (void);
extern int sv_pwd (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
