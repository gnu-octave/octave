# Copyright (C) 1996 John W. Eaton
# 
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

function dump_prefs (file)

# usage: dump_prefs (file)
#
# Have Octave dump all the current user preference variables to FILE
# in a format that can be parsed by Octave later.  If FILE is omitted,
# the listing is printed to stdout.

  if (nargin == 0)
    file = stdout;
  endif

# XXX FIXME XXX -- vectors of strings would be really useful here...
# XXX FIXME XXX -- maybe this should be a built-in function so that we
# wouldn't have to remember to update it each time the list of
# preference variables changes

  dump_1_pref (file, "EDITOR");
  dump_1_pref (file, "IMAGEPATH");
  dump_1_pref (file, "INFO_FILE");
  dump_1_pref (file, "LOADPATH");
  dump_1_pref (file, "OCTAVE_VERSION");
  dump_1_pref (file, "PAGER");
  dump_1_pref (file, "PS1");
  dump_1_pref (file, "PS2");
  dump_1_pref (file, "automatic_replot");
  dump_1_pref (file, "whitespace_in_literal_matrix");
  dump_1_pref (file, "default_save_format");
  dump_1_pref (file, "do_fortran_indexing");
  dump_1_pref (file, "empty_list_elements_ok");
  dump_1_pref (file, "eps");
  dump_1_pref (file, "gnuplot_binary");
  dump_1_pref (file, "ignore_function_time_stamp");
  dump_1_pref (file, "implicit_str_to_num_ok");
  dump_1_pref (file, "ok_to_lose_imaginary_part");
  dump_1_pref (file, "output_max_field_width");
  dump_1_pref (file, "output_precision");
  dump_1_pref (file, "page_screen_output");
  dump_1_pref (file, "prefer_column_vectors");
  dump_1_pref (file, "prefer_zero_one_indexing");
  dump_1_pref (file, "print_answer_id_name");
  dump_1_pref (file, "print_empty_dimensions");
  dump_1_pref (file, "propagate_empty_matrices");
  dump_1_pref (file, "resize_on_range_error");
  dump_1_pref (file, "return_last_computed_value");
  dump_1_pref (file, "save_precision");
  dump_1_pref (file, "silent_functions");
  dump_1_pref (file, "split_long_rows");
  dump_1_pref (file, "treat_neg_dim_as_zero");
  dump_1_pref (file, "warn_assign_as_truth_value");
  dump_1_pref (file, "warn_comma_in_global_decl");
  dump_1_pref (file, "warn_divide_by_zero");

endfunction
