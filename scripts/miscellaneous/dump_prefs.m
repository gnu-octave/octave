## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} dump_prefs (@var{file})
## Have Octave dump all the current user preference variables to
## @var{file} in a format that can be parsed by Octave later.  If
## @var{file} is omitted, the listing is printed to stdout.
## @end deftypefn

## Author: jwe

function dump_prefs (file)

  if (nargin == 0)
    file = stdout;
  endif

  ## XXX FIXME XXX -- it would be nice to be able to get the list of
  ## built-in variables directly from Octave so that we wouldn't have to
  ## remember to update it each time the list of preference variables
  ## changes

  var_list = ["EDITOR";
              "EXEC_PATH";
              "IMAGEPATH";
              "INFO_FILE";
              "INFO_PROGRAM";
              "LOADPATH";
              "PAGER";
              "PS1";
              "PS2";
              "PS4";
              "automatic_replot";
              "beep_on_error";
              "completion_append_char";
              "default_eval_print_flag";
              "default_global_variable_value";
              "default_return_value";
              "default_save_format";
              "define_all_return_values";
              "do_fortran_indexing";
              "echo_executing_commands";
              "empty_list_elements_ok";
              "fixed_point_format";
              "gnuplot_binary";
              "gnuplot_command_end";
              "gnuplot_command_plot";
              "gnuplot_command_replot";
              "gnuplot_command_splot";
              "gnuplot_command_title";
              "gnuplot_command_using";
              "gnuplot_command_with";
              "gnuplot_has_frames";
              "history_file";
              "history_size";
              "ignore_function_time_stamp";
              "implicit_num_to_str_ok";
              "implicit_str_to_num_ok";
              "initialize_global_variables";
              "max_recursion_depth";
              "ok_to_lose_imaginary_part";
              "output_max_field_width";
              "output_precision";
              "page_output_immediately";
              "page_screen_output";
              "prefer_column_vectors";
              "print_answer_id_name";
              "print_empty_dimensions";
              "print_rhs_assign_val";
              "propagate_empty_matrices";
              "resize_on_range_error";
              "return_last_computed_value";
              "save_precision";
              "saving_history";
              "silent_functions";
              "split_long_rows";
              "string_fill_char";
              "struct_levels_to_print";
              "suppress_verbose_help_message";
              "treat_neg_dim_as_zero";
              "warn_assign_as_truth_value";
              "warn_divide_by_zero";
              "warn_function_name_clash";
              "warn_future_time_stamp";
              "warn_missing_semicolon";
              "warn_variable_switch_label";
              "whitespace_in_literal_matrix"];

  for i = 1:rows(var_list)
    var = deblank (var_list(i,:));
    try
      fprintf (file, "  %s = %s\n", var, type ("-q", var));
    catch
      fprintf (file, "# %s = <no value or error in displaying it>\n", var);
    end_try_catch
  endfor

endfunction
