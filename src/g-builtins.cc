// g-builtins.cc                                           -*- C++ -*-
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

/*

The function builtin_pwd adapted from a similar function from GNU
Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <strstream.h>
#include <iostream.h>
#include <fstream.h>
#include <stdio.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <signal.h>
#include <math.h>

#include "f-balance.h"
#include "f-chol.h"
#include "f-colloc.h"
#include "f-dassl.h"
#include "f-det.h"
#include "f-eig.h"
#include "f-expm.h"
#include "f-fft.h"
#include "f-fsolve.h"
#include "f-fsqp.h"
#include "f-givens.h"
#include "f-hess.h"
#include "f-ifft.h"
#include "f-inv.h"
#include "f-lpsolve.h"
#include "f-lsode.h"
#include "f-lu.h"
#include "f-npsol.h"
#include "f-qpsol.h"
#include "f-qr.h"
#include "f-quad.h"
#include "f-qzval.h"
#include "f-rand.h"
#include "f-schur.h"
#include "f-svd.h"
#include "f-syl.h"

#include "sighandlers.h"
#include "procstream.h"
#include "error.h"
#include "variables.h"
#include "builtins.h"
#include "g-builtins.h"
#include "user-prefs.h"
#include "utils.h"
#include "tree.h"
#include "tree-const.h"
#include "input.h"
#include "pager.h"
#include "octave.h"
#include "version.h"
#include "file-io.h"

extern "C"
{
#include <readline/readline.h>

extern char *term_clrpag;
extern void _rl_output_character_function ();
}

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#ifdef WITH_DLD
#include "dynamic-ld.h"
#define Q_STR(name) #name
#define DLD_FCN(name) Q_STR (builtin_##name##_2)
#define DLD_OBJ(name) Q_STR (tc-##name##.o)
#define DLD_BUILTIN(args,n_in,n_out,name,code) \
return octave_dld_tc2_and_go (args, n_in, n_out, Q_STR (name), \
			      DLD_FCN (name), DLD_OBJ (name));

#else
#define DLD_BUILTIN(name,args,n_in,n_out,code) code
#endif

// Non-zero means that pwd always give verbatim directory, regardless
// of symbolic link following.
static int verbatim_pwd = 1;

/*
 * Are all elements of a constant nonzero?
 */
tree_constant *
builtin_all (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("all");
  else
    {
      if (args != NULL_TREE_CONST && args[1].is_defined ())
	{
	  retval = new tree_constant [2];
	  retval[0] = args[1].all ();
	}
    }

  return retval;
}

/*
 * Are any elements of a constant nonzero?
 */
tree_constant *
builtin_any (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("any");
  else
    {
      if (args != NULL_TREE_CONST && args[1].is_defined ())
	{
	  retval = new tree_constant [2];
	  retval[0] = args[1].any ();
	}
    }

  return retval;
}

/*
 * Balancing for eigenvalue problems
 */
tree_constant *
builtin_balance (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin <= 1 || nargin > 4 || nargout < 1 || nargout > 4)
    print_usage ("balance");
  else
    {
      DLD_BUILTIN (args, nargin, nargout, balance,
		   retval = balance (args, nargin, nargout));
    }

  return retval;
}

/*
 * Cholesky factorization.
 */
tree_constant *
builtin_chol (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 && nargout == 1)
    DLD_BUILTIN (args, nargin, nargout, chol,
                retval = chol (args, nargin, nargout);)
  else
    usage ("R = chol(A) \n");

  return retval;
}

/*
 * Clear the screen?
 */
tree_constant *
builtin_clc (const tree_constant *args, int nargin, int nargout)
{
  rl_beg_of_line ();
  rl_kill_line (1);

#if ! defined (_GO32_)
  if (term_clrpag)
    tputs (term_clrpag, 1, _rl_output_character_function);
  else
    crlf ();
#else
  crlf ();
#endif

  fflush (rl_outstream);

  return NULL_TREE_CONST;
}

/*
 * Time in a vector.
 */
tree_constant *
builtin_clock (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  time_t now;
  struct tm *tm;

  time (&now);
  tm = localtime (&now);

  Matrix m (1, 6);
  m.elem (0, 0) = tm->tm_year + 1900;
  m.elem (0, 1) = tm->tm_mon + 1;
  m.elem (0, 2) = tm->tm_mday;
  m.elem (0, 3) = tm->tm_hour;
  m.elem (0, 4) = tm->tm_min;
  m.elem (0, 5) = tm->tm_sec;

  retval = new tree_constant [2];
  retval[0] = tree_constant (m);

  return retval;
}

/*
 * Close the stream to the plotter.
 */
tree_constant *
builtin_closeplot (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  close_plot_stream ();
  return retval;
}

/*
 * Collocation roots and weights.
 */
tree_constant *
builtin_colloc (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin < 2 || nargin > 4)
    print_usage ("colloc");
  else
    DLD_BUILTIN (args, nargin, nargout, colloc,
		 retval = collocation_weights (args, nargin);)

  return retval;
}

/*
 * Cumulative sums and products.
 */
tree_constant *
builtin_cumprod (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("cumprod");
  else
    {
      if (args != NULL_TREE_CONST && args[1].is_defined ())
	{
	  retval = new tree_constant [2];
	  retval[0] = args[1].cumprod ();
	}
    }

  return retval;
}

tree_constant *
builtin_cumsum (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("cumsum");
  else
    {
      if (args != NULL_TREE_CONST && args[1].is_defined ())
	{
	  retval = new tree_constant [2];
	  retval[0] = args[1].cumsum ();
	}
    }

  return retval;
}

/*
 * DAEs.
 */
tree_constant *
builtin_dassl (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = new tree_constant [2];

  if ((nargin == 5 || nargin == 6) && nargout > 0)
    DLD_BUILTIN (args, nargin, nargout, dassl,
		 retval = dassl (args, nargin, nargout);)
  else
    print_usage ("dassl");

  return retval;
}

tree_constant *
builtin_dassl_options (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  DLD_BUILTIN (args, nargin, nargout, dassl_options,
	       retval = dassl_options (args, nargin, nargout);)

  return retval;
}

/*
 * Time in a string.
 */
tree_constant *
builtin_date (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  time_t now;
  struct tm *tm;

  time (&now);
  tm = localtime (&now);
  char date[32];
  int len = strftime (date, 31, "%d-%b-%y", tm);
  if (len > 0)
    {
      retval = new tree_constant [2];
      retval[0] = tree_constant (date);
    }

  return retval;
}

/*
 * Determinant of a matrix.
 */
tree_constant *
builtin_det (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    DLD_BUILTIN (args, nargin, nargout, det,
		 {
		   retval = new tree_constant [2];
		   retval[0] = determinant (args[1]);
		 })
  else
    print_usage ("det");

  return retval;
}

/*
 * Diagonal elements of a matrix.
 */
tree_constant *
builtin_diag (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    {
      retval = new tree_constant [2];
      retval[0] = args[1].diag ();
    }
  else if (nargin == 3)
    {
      retval = new tree_constant [2];
      retval[0] = args[1].diag (args[2]);
    }
  else
    print_usage ("diag");

  return retval;
}

/*
 * Display value without trimmings.
 */
tree_constant *
builtin_disp (tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    args[1].eval (1);
  else
    print_usage ("disp");

  return retval;
}

/*
 * Compute eigenvalues and eigenvectors.
 */
tree_constant *
builtin_eig (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 && (nargout == 1 || nargout == 2))
    DLD_BUILTIN (args, nargin, nargout, eig,
		 retval = eig (args, nargin, nargout);)
  else
    print_usage ("eig");

  return retval;
}

/*
 * Print error message and set the error state.  This should
 * eventually take us up to the top level, possibly printing traceback
 * messages as we go.
 */
tree_constant *
builtin_error (tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  char *msg = "unspecified_error";

  if (nargin == 2 && args != NULL_TREE_CONST && args[1].is_defined ())
    {
      if (args[1].is_string_type ())
	{
	  msg = args[1].string_value ();

	  if (msg == (char *) NULL || *msg == '\0')
	    return retval;
	}
      else if (args[1].is_empty ())
	{
	  return retval;
	}
    }

  error (msg);

  return retval;
}

/*
 * Evaluate text argument as octave source.
 */
tree_constant *
builtin_eval (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    {
      int parse_status = 0;
      retval = new tree_constant [2];
      retval[0] = eval_string (args[1], parse_status);
    }
  else
    print_usage ("eval");

  return retval;
}

/*
 * Check if variable or file exists.
 */
tree_constant *
builtin_exist (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 && args[1].is_string_type ())
    {
      int status = identifier_exists (args[1].string_value ());
      retval = new tree_constant [2];
      retval[0] = tree_constant ((double) status);
    }
  else
    print_usage ("exist");

  return retval;
}

/*
 * Matrix exponential.
 */
tree_constant *
builtin_expm (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    DLD_BUILTIN (args, nargin, nargout, matrix_exp,
		 {
		   retval = new tree_constant [2];
		   retval[0] = matrix_exp (args[1]);
		 })
  else
    print_usage ("expm");

  return retval;
}

/*
 * Identity matrix.
 */
tree_constant *
builtin_eye (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  switch (nargin)
    {
    case 2:
      retval = new tree_constant [2];
      retval[0] = identity_matrix (args[1]);
      break;
    case 3:
      retval = new tree_constant [2];
      retval[0] = identity_matrix (args[1], args[2]);
      break;
    default:
      print_usage ("eye");
      break;
    }

  return retval;
}

/*
 * Closing a file
 */
tree_constant *
builtin_fclose (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("fclose");
  else
    retval = fclose_internal (args);

  return retval;
}

/*
 * Check file for EOF condition.
 */
tree_constant *
builtin_feof (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin < 1)
    print_usage ("feof");
  else
    retval = feof_internal (args, nargin, nargout);

  return retval;
}

/*
 * Check file for error condition.
 */
tree_constant *
builtin_ferror (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin < 1)
    print_usage ("ferror");
  else
    retval = ferror_internal (args, nargin, nargout);

  return retval;
}

/*
 * Evaluate first argument as a function.
 */
tree_constant *
builtin_feval (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin > 1)
    retval = feval (args, nargin, nargout);
  else
    print_usage ("feval");

  return retval;
}

/*
 * Flushing output to a file.
 */
tree_constant *
builtin_fflush (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("fflush");
  else
    retval = fflush_internal (args);

  return retval;
}

/*
 * Fast Fourier Transform.
 */
tree_constant *
builtin_fft (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    DLD_BUILTIN (args, nargin, nargout, fft,
		 {
		   retval = new tree_constant [2];
		   retval[0] = fft (args[1]);
		 })
  else
    print_usage ("fft");

  return retval;
}

/*
 * Get a string from a file.
 */
tree_constant *
builtin_fgets (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 3  && nargout < 3)
    print_usage ("fgets");
  else
    retval = fgets_internal (args, nargout);

  return retval;
}

/*
 * Find nonzero elements.  This should probably only work if
 * do_fortran_indexing is true...
 */
tree_constant *
builtin_find (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    {
      retval = new tree_constant [2];
      retval[0] = find_nonzero_elem_idx (args[1]);
    }
  else
    print_usage ("find");

  return retval;
}

/*
 * Don\'t really count floating point operations.
 */
tree_constant *
builtin_flops (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin > 2)
    print_usage ("flops");

  warning ("flops always returns zero");

  retval = new tree_constant [2];
  retval[0] = tree_constant (0.0);

  return retval;
}

/*
 * Opening a file.
 */
tree_constant *
builtin_fopen (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 3)
    print_usage ("fopen");
  else
    retval = fopen_internal (args);

  return retval;
}

/*
 * Formatted printing to a file.
 */
tree_constant *
builtin_fprintf (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin < 3)
    print_usage ("fprintf");
  else
    retval = do_printf ("fprintf", args, nargin, nargout);

  return retval;
}

/*
 * Read binary data from a file.
 */
tree_constant *
builtin_fread (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin < 2)
    print_usage ("fread");
  else
    retval = fread_internal (args, nargin, nargout);

  return retval;
}

/*
 * Rewind a file.
 */
tree_constant *
builtin_frewind (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("frewind");
  else
    retval = frewind_internal (args);

  return retval;
}

/*
 * Report on open files.
 */
tree_constant *
builtin_freport (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin > 1)
    warning ("freport: ignoring extra arguments");

  retval = freport_internal ();

  return retval;
}

/*
 * Formatted reading from a file.
 */
tree_constant *
builtin_fscanf (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2 && nargin != 3)
    print_usage ("fscanf");
  else
    retval = do_scanf ("fscanf", args, nargin, nargout);

  return retval;
}

/*
 * Seek a point in a file for reading and/or writing.
 */
tree_constant *
builtin_fseek (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 3 && nargin != 4)
    print_usage ("fseek");
  else
    retval = fseek_internal (args, nargin);

  return retval;
}

/*
 * Nonlinear algebraic equations.
 */
tree_constant *
builtin_fsolve (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin >= 3 && nargin <= 7 && nargout >= 1 && nargout <= 3)
    DLD_BUILTIN (args, nargin, nargout, fsolve,
		 retval = fsolve (args, nargin, nargout);)
  else
    print_usage ("fsolve");

  return retval;
}

tree_constant *
builtin_fsolve_options (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  DLD_BUILTIN (args, nargin, nargout, fsolve_options,
	       retval = fsolve_options (args, nargin, nargout);)

  return retval;
}

/*
 * NLPs.
 */
tree_constant *
builtin_fsqp (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

#if defined (FSQP_MISSING)
  print_usage ("fsqp");
#else
  if ((nargin == 3 || nargin == 5 || nargin == 6 || nargin == 8
       || nargin == 9 || nargin == 11)
      && (nargout >= 1 && nargout <= 3))
    DLD_BUILTIN (args, nargin, nargout, fsqp,
		 retval = fsqp (args, nargin, nargout);)
  else
    print_usage ("fsolve");
#endif

  return retval;
}

tree_constant *
builtin_fsqp_options (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

#if defined (FSQP_MISSING)
  print_usage ("fsqp_options");
#else
  DLD_BUILTIN (args, nargin, nargout, fsqp_options,
	       retval = fsqp_options (args, nargin, nargout);)
#endif

  return retval;
}

/*
 * Tell current position of file.
 */
tree_constant *
builtin_ftell (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("ftell");
  else
    retval = ftell_internal (args);

  return retval;
}

/*
 * Write binary data to a file.
 */
tree_constant *
builtin_fwrite (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin < 3)
    print_usage ("fwrite");
  else
    retval = fwrite_internal (args, nargin, nargout);

  return retval;
}

/*
 * Get the value of an environment variable.
 */
tree_constant *
builtin_getenv (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 && args[1].is_string_type ())
    {
      retval = new tree_constant [2];
      char *value = getenv (args[1].string_value ());
      if (value != (char *) NULL)
	retval[0] = tree_constant (value);
      else
	retval[0] = tree_constant ("");
    }
  else
    print_usage ("getenv");

  return retval;
}

/*
 * Inverse Fast Fourier Transform.
 */
tree_constant *
builtin_ifft (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    DLD_BUILTIN (args, nargin, nargout, ifft,
		 {
		   retval = new tree_constant [2];
		   retval[0] = ifft (args[1]);
		 })
  else
    print_usage ("ifft");

  return retval;
}

/*
 * Inverse of a square matrix.
 */
tree_constant *
builtin_inv (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    DLD_BUILTIN (args, nargin, nargout, inv,
		 {
		   retval = new tree_constant [2];
		   retval[0] = inverse (args[1]);
		 })
  else
    print_usage ("inv");

  return retval;
}

/*
 * Prompt user for input.
 */
tree_constant *
builtin_input (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 || nargin == 3)
    {
      retval = new tree_constant [2];
      retval[0] = get_user_input (args, nargin, nargout);
    }
  else
    print_usage ("input");

  return retval;
}

/*
 * Does the given string name a global variable?
 */
tree_constant *
builtin_is_global (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = new tree_constant [2];
  retval[0] = tree_constant (0.0);

  if (nargin == 2 && args[1].is_string_type ())
    {
      char *name = args[1].string_value ();
      if (is_globally_visible (name))
	retval[0] = tree_constant (1.0);
    }
  else
    print_usage ("is_global");

  return retval;
}

/*
 * Is the argument a string?
 */
tree_constant *
builtin_isstr (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("isstr");
  else
    {
      if (args != NULL_TREE_CONST && args[1].is_defined ())
	{
	  retval = new tree_constant [2];
	  retval[0] = args[1].isstr ();
	}
    }

  return retval;
}

/*
 * Maybe help in debugging.
 */
tree_constant *
builtin_keyboard (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 1 || nargin == 2)
    {
      retval = new tree_constant [2];
      retval[0] = get_user_input (args, nargin, nargout, 1);
    }
  else
    print_usage ("keyboard");

  return retval;
}

/*
 * Matrix logarithm.
 */
tree_constant *
builtin_logm (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    retval = matrix_log (args[1]);
  else
    print_usage ("logm");

  return retval;
}

/*
 * LPs.
 */
tree_constant *
builtin_lpsolve (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

// Force a bad value of inform, and empty matrices for x and phi.
  retval = new tree_constant [4];
  Matrix m;
  retval[0] = tree_constant (m);
  retval[1] = tree_constant (m);
  retval[2] = tree_constant (-1.0);

  if (nargin == 0)
    DLD_BUILTIN (args, nargin, nargout, lpsolve,
		 retval = lpsolve (args, nargin, nargout);)
  else
    print_usage ("lp_solve");

  return retval;
}

tree_constant *
builtin_lpsolve_options (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  DLD_BUILTIN (args, nargin, nargout, lpsolve_options,
	       retval = lpsolve_options (args, nargin, nargout);)

  return retval;
}

/*
 * ODEs.
 */
tree_constant *
builtin_lsode (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if ((nargin == 4 || nargin == 5) && nargout == 1)
    DLD_BUILTIN (args, nargin, nargout, lsode,
		 retval = lsode (args, nargin, nargout);)
  else
    print_usage ("lsode");

  return retval;
}

tree_constant *
builtin_lsode_options (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  DLD_BUILTIN (args, nargin, nargout, lsode_options,
	       retval = lsode_options (args, nargin, nargout);)

  return retval;
}

/*
 * LU factorization.
 */
tree_constant *
builtin_lu (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 && nargout < 4)
    DLD_BUILTIN (args, nargin, nargout, lu,
		 retval = lu (args[1], nargout);)
  else
    print_usage ("lu");

  return retval;
}

/*
 * Max values.
 */
tree_constant *
builtin_max (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if ((nargin == 2 && (nargout == 1 || nargout == 2))
      || (nargin == 3 && nargout == 1))
    retval = column_max (args, nargin, nargout);
  else
    print_usage ("max");

  return retval;
}

/*
 * Min values.
 */
tree_constant *
builtin_min (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if ((nargin == 2 && (nargout == 1 || nargout == 2))
      || (nargin == 3 && nargout == 1))
    retval = column_min (args, nargin, nargout);
  else
    print_usage ("min");

  return retval;
}

/*
 * NLPs.
 */
tree_constant *
builtin_npsol (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

#if defined (NPSOL_MISSING)
// Force a bad value of inform, and empty matrices for x, phi, and lambda.
  retval = new tree_constant [4];
  Matrix m;
  retval[0] = tree_constant (m);
  retval[1] = tree_constant (m);
  retval[2] = tree_constant (-1.0);
  retval[3] = tree_constant (m);
  print_usage ("npsol");
#else
  if ((nargin == 3 || nargin == 5 || nargin == 6 || nargin == 8
       || nargin == 9 || nargin == 11)
      && (nargout >= 1 && nargout <= 4))
    DLD_BUILTIN (args, nargin, nargout, npsol,
		 retval = npsol (args, nargin, nargout);)
  else
    print_usage ("npsol");
#endif

  return retval;
}

tree_constant *
builtin_npsol_options (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

#if defined (NPSOL_MISSING)
  print_usage ("npsol_options");
#else
  DLD_BUILTIN (args, nargin, nargout, npsol_options,
	       retval = npsol_options (args, nargin, nargout);)
#endif

  return retval;
}

/*
 * A matrix of ones.
 */
tree_constant *
builtin_ones (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  switch (nargin)
    {
    case 2:
      retval = new tree_constant [2];
      retval[0] = fill_matrix (args[1], 1.0, "ones");
      break;
    case 3:
      retval = new tree_constant [2];
      retval[0] = fill_matrix (args[1], args[2], 1.0, "ones");
      break;
    default:
      print_usage ("ones");
      break;
    }

  return retval;
}

/*
 * You guessed it.
 */
tree_constant *
builtin_pause (const tree_constant *args, int nargin, int nargout)
{
  if (! (nargin == 1 || nargin == 2))
    {
      print_usage ("pause");
      return NULL_TREE_CONST;
    }

  if (interactive)
    {
      if (nargin == 2)
	sleep (NINT (args[1].double_value ()));
      else if (kbhit () == EOF)
	clean_up_and_exit (0);
    }

  return NULL_TREE_CONST;
}

/*
 * Delete turds from /tmp.
 */
tree_constant *
builtin_purge_tmp_files (const tree_constant *, int, int)
{
  cleanup_tmp_files ();
  return NULL_TREE_CONST;
}

/*
 * Formatted printing.
 */
tree_constant *
builtin_printf (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin < 2)
    print_usage ("printf");
  else
    retval = do_printf ("printf", args, nargin, nargout);

  return retval;
}

/*
 * Product.
 */
tree_constant *
builtin_prod (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("prod");
  else
    {
      if (args != NULL_TREE_CONST && args[1].is_defined ())
	{
	  retval = new tree_constant [2];
	  retval[0] = args[1].prod ();
	}
    }

  return retval;
}

/*
 * Print name of current working directory.
 */
tree_constant *
builtin_pwd (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  char *directory;

  if (verbatim_pwd)
    {
      char *buffer = new char [MAXPATHLEN];
      directory = getcwd (buffer, MAXPATHLEN);

      if (!directory)
	{
	  warning ("pwd: can't find working directory!");
	  delete buffer;
	}
    }
  else
    {
      directory = get_working_directory ("pwd");
    }

  if (directory)
    {
      char *s = strconcat (directory, "\n");
      retval = new tree_constant [2];
      retval[0] = tree_constant (s);
      delete [] s;
    }
  return retval;
}

/*
 * QPs.
 */
tree_constant *
builtin_qpsol (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

#if defined (QPSOL_MISSING)
// Force a bad value of inform, and empty matrices for x, phi, and lambda.
  retval = new tree_constant [5];
  Matrix m;
  retval[0] = tree_constant (m);
  retval[1] = tree_constant (m);
  retval[2] = tree_constant (-1.0);
  retval[3] = tree_constant (m);
  print_usage ("qpsol");
#else
  if ((nargin == 4 || nargin == 6 || nargin == 7 || nargin == 9)
      && (nargout >= 1 && nargout <= 4))
    DLD_BUILTIN (args, nargin, nargout, qpsol,
		 retval = qpsol (args, nargin, nargout);)
  else
    print_usage ("qpsol");
#endif

  return retval;
}

tree_constant *
builtin_qpsol_options (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

#if defined (QPSOL_MISSING)
  print_usage ("qpsol");
#else
  DLD_BUILTIN (args, nargin, nargout, qpsol_options,
	       retval = qpsol_options (args, nargin, nargout);)
#endif

  return retval;
}

/*
 * QR factorization.
 */
tree_constant *
builtin_qr (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 && nargout < 3)
    DLD_BUILTIN (args, nargin, nargout, qr,
		 retval = qr (args[1], nargout);)
  else
    print_usage ("qr");

  return retval;
}

/*
 * generalized eigenvalues via qz
 */
tree_constant *
builtin_qzval (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 3 && nargout < 2)
    DLD_BUILTIN (args, nargin, nargout, qzvalue,
		 retval = qzvalue (args, nargin, nargout);)
  else
    print_usage ("qzval");

  return retval;
}

/*
 * Random numbers.
 */
tree_constant *
builtin_quad (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if ((nargin > 3 && nargin < 7) && (nargout > 0 && nargout < 5))
    DLD_BUILTIN (args, nargin, nargout, quad,
		 retval = do_quad (args, nargin, nargout);)
  else
    print_usage ("quad");

  return retval;
}

tree_constant *
builtin_quad_options (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  DLD_BUILTIN (args, nargin, nargout, quad_options,
	       retval = quad_options (args, nargin, nargout);)

  return retval;
}

/*
 * I'm outta here.
 */
tree_constant *
builtin_quit (const tree_constant *args, int nargin, int nargout)
{
  quitting_gracefully = 1;
  clean_up_and_exit (0);
  return NULL_TREE_CONST;
}

/*
 * Random numbers.
 */
tree_constant *
builtin_rand (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if ((nargin > 0 && nargin < 4) && nargout == 1)
    DLD_BUILTIN (args, nargin, nargout, rand,
		 retval = rand_internal (args, nargin, nargout);)
  else
    print_usage ("rand");

  return retval;
}

/*
 * Replot current plot.
 */
tree_constant *
builtin_replot (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin > 1)
    warning ("replot: ignoring extra arguments");

  send_to_plot_stream ("replot\n");

  return retval;
}

/*
 * Formatted reading.
 */
tree_constant *
builtin_scanf (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("scanf");
  else
    retval = do_scanf ("scanf", args, nargin, nargout);

  return retval;
}

/*
 * Convert a vector to a string.
 */
tree_constant *
builtin_setstr (tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    {
      retval = new tree_constant [2];
      retval[0] = args[1].convert_to_str ();
    }
  else
    print_usage ("setstr");

  return retval;
}

/*
 * Execute a shell command.
 */
tree_constant *
builtin_shell_command (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 || nargin == 3)
    {
      if (args[1].is_string_type ())
	{
	  iprocstream cmd (args[1].string_value ());
	  char ch;
	  ostrstream output_buf;
	  while (cmd.get (ch))
	    output_buf.put (ch);

	  output_buf << ends;
	  if (nargin == 2)
	    {
	      maybe_page_output (output_buf);
	    }
	  else
	    {
	      retval = new tree_constant [2];
	      retval[0] = tree_constant (output_buf.str ());
	    }
	}
      else
	error ("shell_cmd: first argument must be a string");
    }
  else
    print_usage ("shell_cmd");

  return retval;
}

/*
 * Report rows and columns.
 */
tree_constant *
builtin_size (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("size");
  else
    {
      if (args != NULL_TREE_CONST && args[1].is_defined ())
	{
	  int nr = args[1].rows ();
	  int nc = args[1].columns ();
	  if (nargout == 1)
	    {
	      Matrix m (1, 2);
	      m.elem (0, 0) = nr;
	      m.elem (0, 1) = nc;
	      retval = new tree_constant [2];
	      retval[0] = tree_constant (m);
	    }
	  else if (nargout == 2)
	    {
	      retval = new tree_constant [3];
	      retval[0] = tree_constant ((double) nr);
	      retval[1] = tree_constant ((double) nc);
	    }
	  else
	    print_usage ("size");
	}
    }

  return retval;
}

/*
 * Sort columns.
 */
tree_constant *
builtin_sort (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    retval = sort (args, nargin, nargout);
  else
    print_usage ("sort");

  return retval;
}

/*
 * Formatted printing to a string.
 */
tree_constant *
builtin_sprintf (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin < 2)
    print_usage ("sprintf");
  else
    retval = do_printf ("sprintf", args, nargin, nargout);

  return retval;
}

/*
 * Matrix sqrt.
 */
tree_constant *
builtin_sqrtm (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2)
    retval = matrix_sqrt (args[1]);
  else
    print_usage ("sqrtm");

  return retval;
}

/*
 * Formatted reading from a string.
 */
tree_constant *
builtin_sscanf (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 3)
    print_usage ("sscanf");
  else
    retval = do_scanf ("sscanf", args, nargin, nargout);

  return retval;
}

/*
 * Sum.
 */
tree_constant *
builtin_sum (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("sum");
  else
    {
      if (args != NULL_TREE_CONST && args[1].is_defined ())
	{
	  retval = new tree_constant [2];
	  retval[0] = args[1].sum ();
	}
    }

  return retval;
}

/*
 * Sum of squares.
 */
tree_constant *
builtin_sumsq (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin != 2)
    print_usage ("sumsq");
  else
    {
      if (args != NULL_TREE_CONST && args[1].is_defined ())
	{
	  retval = new tree_constant [2];
	  retval[0] = args[1].sumsq ();
	}
    }

  return retval;
}

/*
 * Singluar value decomposition.
 */
tree_constant *
builtin_svd (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 && (nargout == 1 || nargout == 3))
    DLD_BUILTIN (args, nargin, nargout, svd,
		 retval = svd (args, nargin, nargout);)
  else
    print_usage ("svd");

  return retval;
}

/*
 * Sylvester equation solver.
 */
tree_constant *
builtin_syl (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if ((nargin == 4) && (nargout == 1))
    DLD_BUILTIN (args, nargin, nargout, syl,
		 retval = syl (args, nargin, nargout);)
  else
    print_usage ("syl");

  return retval;
}

/*
 * Schur Decomposition.
 */
tree_constant *
builtin_schur (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if ((nargin == 3 || nargin == 2) && (nargout == 1 || nargout == 2))
    DLD_BUILTIN (args, nargin, nargout, schur,
		 retval = schur (args, nargin, nargout);)
  else
    print_usage ("schur");

  return retval;
}

/*
 * Givens rotation.
 */
tree_constant *
builtin_givens (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 3 && (nargout == 1 || nargout == 2 ))
    retval = givens (args, nargin, nargout);
  else
    print_usage ("givens");

  return retval;
}

/*
 * Hessenberg Decomposition.
 */
tree_constant *
builtin_hess (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 && (nargout == 1 || nargout == 2))
    DLD_BUILTIN (args, nargin, nargout, hess,
		 retval = hess (args, nargin, nargout);)
  else
    print_usage ("hess");

  return retval;
}

/*
 * Variable argument lists.
 */
tree_constant *
builtin_va_arg (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  if (nargin == 1)
    {
      if (curr_function != (tree_function *) NULL)
	{
	  if (curr_function->takes_varargs ())
	    {
	      retval = new tree_constant [2];
	      retval[0] = curr_function->octave_va_arg ();
	    }
	  else
	    {
	      error ("va_arg only valid within function taking variable");
	      error ("number of arguments");
	    }
	}
      else
	error ("va_arg only valid within function body");
    }
  else
    print_usage ("va_arg");

  return retval;
}

tree_constant *
builtin_va_start (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  if (nargin == 1)
    {
      if (curr_function != (tree_function *) NULL)
	{
	  if (curr_function->takes_varargs ())
	    curr_function->octave_va_start ();
	  else
	    {
	      error ("va_start only valid within function taking variable");
	      error ("number of arguments");
	    }
	}
      else
	error ("va_start only valid within function body");
    }
  else
    print_usage ("va_start");

  return retval;
}

/*
 * Copying information.
 */
tree_constant *
builtin_warranty (const tree_constant *args, int nargin, int nargout)
{
  ostrstream output_buf;
  output_buf << "\n    Octave, version " << version_string
	     << ".  Copyright (C) 1992, 1993, 1994 John W. Eaton\n"
	     << "\n\
    This program is free software; you can redistribute it and/or modify\n\
    it under the terms of the GNU General Public License as published by\n\
    the Free Software Foundation; either version 2 of the License, or\n\
    (at your option) any later version.\n\n\
    This program is distributed in the hope that it will be useful,\n\
    but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
    GNU General Public License for more details.\n\n\
    You should have received a copy of the GNU General Public License\n\
    along with this program. If not, write to the Free Software\n\
    Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.\n\n";

  output_buf << ends;
  maybe_page_output (output_buf);

  return NULL_TREE_CONST;
}

/*
 * A matrix of zeros.
 */
tree_constant *
builtin_zeros (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  switch (nargin)
    {
    case 2:
      retval = new tree_constant [2];
      retval[0] = fill_matrix (args[1], 0.0, "zeros");
      break;
    case 3:
      retval = new tree_constant [2];
      retval[0] = fill_matrix (args[1], args[2], 0.0, "zeros");
      break;
    default:
      print_usage ("zeros");
      break;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
