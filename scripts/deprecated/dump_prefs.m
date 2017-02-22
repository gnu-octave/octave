## Copyright (C) 1994-2017 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} dump_prefs ()
## @deftypefnx {} {} dump_prefs (@var{fid})
##
## @code{dump_prefs} is deprecated and will be removed in Octave version 4.4.
## Please use individual preference get/set routines in all new code.
##
## Dump the current settings of all user preferences to stdout in a format that
## can be parsed by Octave later.
##
## If the optional argument @var{fid} is given then the results are written to
## the file specified by file descriptor @var{fid}.
## @end deftypefn

## Author: jwe

## Deprecated in 4.0

function dump_prefs (fid)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "dump_prefs is obsolete and will be removed from a future version of Octave, recode using individual preference get/set routines");
  endif

  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 0)
    fid = stdout;
  endif

  ## FIXME: It would be nice to be able to get the list of built-in variables
  ## directly from Octave so that we wouldn't have to remember to update it
  ## each time the list of preference variables changes

  pref_list = {"EDITOR"
              "EXEC_PATH"
              "IMAGE_PATH"
              "PAGER"
              "PAGER_FLAGS"
              "PS1"
              "PS2"
              "PS4"
              "allow_noninteger_range_as_index"
              "beep_on_error"
              "built_in_docstrings_file"
              "completion_append_char"
              "confirm_recursive_rmdir"
              "crash_dumps_octave_core"
              "debug_java"
              "debug_on_error"
              "debug_on_interrupt"
              "debug_on_warning"
              "disable_diagonal_matrix"
              "disable_permutation_matrix"
              "disable_range"
              "do_braindead_shortcircuit_evaluation"
              "doc_cache_file"
              "echo_executing_commands"
              "fixed_point_format"
              "gnuplot_binary"
              "history_file"
              "history_save"
              "history_size"
              "history_timestamp_format_string"
              "ignore_function_time_stamp"
              "info_file"
              "info_program"
              "java_matrix_autoconversion"
              "java_unsigned_autoconversion"
              "makeinfo_program"
              "max_recursion_depth"
              "missing_component_hook"
              "missing_function_hook"
              "octave_core_file_limit"
              "octave_core_file_name"
              "octave_core_file_options"
              "optimize_subsasgn_calls"
              "output_max_field_width"
              "output_precision"
              "page_output_immediately"
              "page_screen_output"
              "print_empty_dimensions"
              "print_struct_array_contents"
              "save_default_options"
              "save_header_format_string"
              "save_precision"
              "sighup_dumps_octave_core"
              "sigterm_dumps_octave_core"
              "silent_functions"
              "sparse_auto_mutate"
              "split_long_rows"
              "string_fill_char"
              "struct_levels_to_print"
              "suppress_verbose_help_message"
              "texi_macros_file"};

  for i = 1:rows (pref_list)
    pref = pref_list{i};
    try
      val = feval (pref);
      if (isnumeric (val))
        val = sprintf ("%g", val);
      endif
      fprintf (fid, "  %s = %s\n", pref, val);
    catch
      fprintf (fid, "# %s = <no value or error in displaying it>\n", pref);
    end_try_catch
  endfor

endfunction


%!error dump_prefs (1,2)
