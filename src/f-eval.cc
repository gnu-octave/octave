// f-eval.cc                                           -*- C++ -*-
/*

Copyright (C) 1994 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "unwind-prot.h"
#include "tree-const.h"
#include "variables.h"
#include "octave.h"
#include "input.h"
#include "parse.h"
#include "lex.h"
#include "f-eval.h"

Octave_object
feval (const Octave_object& args, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  Octave_object retval;

  tree_fvc *fcn = is_valid_function (args(1), "feval", 1);
  if (fcn != (tree_fvc *) NULL)
    {
      int nargin = args.length () - 1;
      Octave_object tmp_args (nargin);
      for (int i = 0; i < nargin; i++)
	tmp_args(i) = args(i+1);
      retval = fcn->eval (0, nargout, tmp_args);
    }

  return retval;
}

tree_constant
eval_string (const char *string, int print, int ans_assign,
	     int& parse_status)
{
  begin_unwind_frame ("eval_string");

  unwind_protect_int (get_input_from_eval_string);
  unwind_protect_ptr (global_command);
  unwind_protect_ptr (current_eval_string);

  get_input_from_eval_string = 1;
  current_eval_string = string;

  YY_BUFFER_STATE old_buf = current_buffer ();
  YY_BUFFER_STATE new_buf = create_buffer ((FILE *) NULL);

  add_unwind_protect (restore_input_buffer, (void *) old_buf);
  add_unwind_protect (delete_input_buffer, (void *) new_buf);

  switch_to_buffer (new_buf);

  unwind_protect_ptr (curr_sym_tab);

  reset_parser ();

  parse_status = yyparse ();

// Important to reset the idea of where input is coming from before
// trying to eval the command we just parsed -- it might contain the
// name of an function file that still needs to be parsed!

  tree *command = global_command;

  run_unwind_frame ("eval_string");

  tree_constant retval;

  if (parse_status == 0 && command != NULL_TREE)
    {
      retval = command->eval (print);
      delete command;
    }

  return retval;
}

tree_constant
eval_string (const tree_constant& arg, int& parse_status)
{
  if (! arg.is_string_type ())
    {
      error ("eval: expecting string argument");
      return -1;
    }

  char *string = arg.string_value ();

// Yes Virginia, we always print here...

  return eval_string (string, 1, 1, parse_status);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
