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
Octave_object
builtin_all (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("all");
  else
    {
      if (args.length () > 0 && args(1).is_defined ())
	{
	  retval.resize (1);
	  retval(0) = args(1).all ();
	}
    }

  return retval;
}

/*
 * Are any elements of a constant nonzero?
 */
Octave_object
builtin_any (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("any");
  else
    {
      if (args.length () > 0 && args(1).is_defined ())
	{
	  retval.resize (1);
	  retval(0) = args(1).any ();
	}
    }

  return retval;
}

/*
 * Balancing for eigenvalue problems
 */
Octave_object
builtin_balance (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin <= 1 || nargin > 4 || nargout < 0 || nargout > 4)
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
Octave_object
builtin_chol (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2 && (nargout == 0 || nargout == 1))
    DLD_BUILTIN (args, nargin, nargout, chol,
		 {
		   retval.resize (1);
		   retval(0) = chol (args(1));
		 })
  else
    usage ("R = chol(A) \n");

  return retval;
}

/*
 * Clear the screen?
 */
Octave_object
builtin_clc (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

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

  return retval;
}

/*
 * Time in a vector.
 */
Octave_object
builtin_clock (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

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

  retval.resize (1);
  retval(0) = tree_constant (m);

  return retval;
}

/*
 * Close the stream to the plotter.
 */
Octave_object
builtin_closeplot (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;
  close_plot_stream ();
  return retval;
}

/*
 * Collocation roots and weights.
 */
Octave_object
builtin_colloc (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

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
Octave_object
builtin_cumprod (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("cumprod");
  else
    {
      if (args.length () > 0 && args(1).is_defined ())
	{
	  retval.resize (1);
	  retval(0) = args(1).cumprod ();
	}
    }

  return retval;
}

Octave_object
builtin_cumsum (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("cumsum");
  else
    {
      if (args.length () > 0 && args(1).is_defined ())
	{
	  retval.resize (1);
	  retval(0) = args(1).cumsum ();
	}
    }

  return retval;
}

/*
 * DAEs.
 */
Octave_object
builtin_dassl (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if ((nargin == 5 || nargin == 6) && nargout >= 0)
    DLD_BUILTIN (args, nargin, nargout, dassl,
		 retval = dassl (args, nargin, nargout);)
  else
    print_usage ("dassl");

  return retval;
}

Octave_object
builtin_dassl_options (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  DLD_BUILTIN (args, nargin, nargout, dassl_options,
	       retval = dassl_options (args, nargin, nargout);)

  return retval;
}

/*
 * Time in a string.
 */
Octave_object
builtin_date (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  time_t now;
  struct tm *tm;

  time (&now);
  tm = localtime (&now);
  char date[32];
  int len = strftime (date, 31, "%d-%b-%y", tm);
  if (len > 0)
    {
      retval.resize (1);
      retval(0) = tree_constant (date);
    }

  return retval;
}

/*
 * Determinant of a matrix.
 */
Octave_object
builtin_det (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    DLD_BUILTIN (args, nargin, nargout, det,
		 {
		   retval.resize (1);
		   retval(0) = determinant (args(1));
		 })
  else
    print_usage ("det");

  return retval;
}

/*
 * Diagonal elements of a matrix.
 */
Octave_object
builtin_diag (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    {
      retval.resize (1);
      retval(0) = args(1).diag ();
    }
  else if (nargin == 3)
    {
      retval.resize (1);
      retval(0) = args(1).diag (args(2));
    }
  else
    print_usage ("diag");

  return retval;
}

/*
 * Display value without trimmings.
 */
Octave_object
builtin_disp (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    args(1).eval (1);
  else
    print_usage ("disp");

  return retval;
}

/*
 * Compute eigenvalues and eigenvectors.
 */
Octave_object
builtin_eig (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2 && (nargout == 0 || nargout == 1 || nargout == 2))
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
Octave_object
builtin_error (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  char *msg = "unspecified_error";

  if (nargin == 2 && args.length () > 0 && args(1).is_defined ())
    {
      if (args(1).is_string_type ())
	{
	  msg = args(1).string_value ();

	  if (msg == (char *) NULL || *msg == '\0')
	    return retval;
	}
      else if (args(1).is_empty ())
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
Octave_object
builtin_eval (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    {
      int parse_status = 0;
      retval.resize (1);
      retval(0) = eval_string (args(1), parse_status);
    }
  else
    print_usage ("eval");

  return retval;
}

/*
 * Check if variable or file exists.
 */
Octave_object
builtin_exist (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2 && args(1).is_string_type ())
    {
      int status = identifier_exists (args(1).string_value ());
      retval.resize (1);
      retval(0) = tree_constant ((double) status);
    }
  else
    print_usage ("exist");

  return retval;
}

/*
 * Matrix exponential.
 */
Octave_object
builtin_expm (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    DLD_BUILTIN (args, nargin, nargout, matrix_exp,
		 {
		   retval.resize (1);
		   retval(0) = matrix_exp (args(1));
		 })
  else
    print_usage ("expm");

  return retval;
}

/*
 * Identity matrix.
 */
Octave_object
builtin_eye (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  switch (nargin)
    {
    case 2:
      retval.resize (1);
      retval(0) = identity_matrix (args(1));
      break;
    case 3:
      retval.resize (1);
      retval(0) = identity_matrix (args(1), args(2));
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
Octave_object
builtin_fclose (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("fclose");
  else
    retval = fclose_internal (args);

  return retval;
}

/*
 * Check file for EOF condition.
 */
Octave_object
builtin_feof (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("feof");
  else
    retval = feof_internal (args, nargin, nargout);

  return retval;
}

/*
 * Check file for error condition.
 */
Octave_object
builtin_ferror (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("ferror");
  else
    retval = ferror_internal (args, nargin, nargout);

  return retval;
}

/*
 * Evaluate first argument as a function.
 */
Octave_object
builtin_feval (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin > 1)
    retval = feval (args, nargin, nargout);
  else
    print_usage ("feval");

  return retval;
}

/*
 * Flushing output to a file.
 */
Octave_object
builtin_fflush (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("fflush");
  else
    retval = fflush_internal (args);

  return retval;
}

/*
 * Fast Fourier Transform.
 */
Octave_object
builtin_fft (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    DLD_BUILTIN (args, nargin, nargout, fft,
		 {
		   retval.resize (1);
		   retval(0) = fft (args(1));
		 })
  else
    print_usage ("fft");

  return retval;
}

/*
 * Get a string from a file.
 */
Octave_object
builtin_fgets (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 3)
    print_usage ("fgets");
  else
    retval = fgets_internal (args, nargout);

  return retval;
}

/*
 * Find nonzero elements.  This should probably only work if
 * do_fortran_indexing is true...
 */
Octave_object
builtin_find (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    {
      retval.resize (1);
      retval(0) = find_nonzero_elem_idx (args(1));
    }
  else
    print_usage ("find");

  return retval;
}

/*
 * Don\'t really count floating point operations.
 */
Octave_object
builtin_flops (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin > 2)
    print_usage ("flops");

  warning ("flops always returns zero");

  retval.resize (1);
  retval(0) = tree_constant (0.0);

  return retval;
}

/*
 * Opening a file.
 */
Octave_object
builtin_fopen (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 3)
    print_usage ("fopen");
  else
    retval = fopen_internal (args);

  return retval;
}

/*
 * Formatted printing to a file.
 */
Octave_object
builtin_fprintf (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin < 3)
    print_usage ("fprintf");
  else
    retval = do_printf ("fprintf", args, nargin, nargout);

  return retval;
}

/*
 * Read binary data from a file.
 */
Octave_object
builtin_fread (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin < 2 || nargin > 4)
    print_usage ("fread");
  else
    retval = fread_internal (args, nargin, nargout);

  return retval;
}

/*
 * Rewind a file.
 */
Octave_object
builtin_frewind (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("frewind");
  else
    retval = frewind_internal (args);

  return retval;
}

/*
 * Report on open files.
 */
Octave_object
builtin_freport (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin > 1)
    warning ("freport: ignoring extra arguments");

  retval = freport_internal ();

  return retval;
}

/*
 * Formatted reading from a file.
 */
Octave_object
builtin_fscanf (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2 && nargin != 3)
    print_usage ("fscanf");
  else
    retval = do_scanf ("fscanf", args, nargin, nargout);

  return retval;
}

/*
 * Seek a point in a file for reading and/or writing.
 */
Octave_object
builtin_fseek (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 3 && nargin != 4)
    print_usage ("fseek");
  else
    retval = fseek_internal (args, nargin);

  return retval;
}

/*
 * Nonlinear algebraic equations.
 */
Octave_object
builtin_fsolve (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin >= 3 && nargin <= 7 && nargout >= 0 && nargout <= 3)
    DLD_BUILTIN (args, nargin, nargout, fsolve,
		 retval = fsolve (args, nargin, nargout);)
  else
    print_usage ("fsolve");

  return retval;
}

Octave_object
builtin_fsolve_options (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  DLD_BUILTIN (args, nargin, nargout, fsolve_options,
	       retval = fsolve_options (args, nargin, nargout);)

  return retval;
}

/*
 * NLPs.
 */
Octave_object
builtin_fsqp (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

#if defined (FSQP_MISSING)
  print_usage ("fsqp");
#else
  if ((nargin == 3 || nargin == 5 || nargin == 6 || nargin == 8
       || nargin == 9 || nargin == 11)
      && (nargout >= 0 && nargout <= 3))
    DLD_BUILTIN (args, nargin, nargout, fsqp,
		 retval = fsqp (args, nargin, nargout);)
  else
    print_usage ("fsolve");
#endif

  return retval;
}

Octave_object
builtin_fsqp_options (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

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
Octave_object
builtin_ftell (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("ftell");
  else
    retval = ftell_internal (args);

  return retval;
}

/*
 * Write binary data to a file.
 */
Octave_object
builtin_fwrite (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin < 3 || nargin > 4)
    print_usage ("fwrite");
  else
    retval = fwrite_internal (args, nargin, nargout);

  return retval;
}

/*
 * Get the value of an environment variable.
 */
Octave_object
builtin_getenv (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2 && args(1).is_string_type ())
    {
      retval.resize (1);
      char *value = getenv (args(1).string_value ());
      if (value != (char *) NULL)
	retval(0) = tree_constant (value);
      else
	retval(0) = tree_constant ("");
    }
  else
    print_usage ("getenv");

  return retval;
}

/*
 * Inverse Fast Fourier Transform.
 */
Octave_object
builtin_ifft (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    DLD_BUILTIN (args, nargin, nargout, ifft,
		 {
		   retval.resize (1);
		   retval(0) = ifft (args(1));
		 })
  else
    print_usage ("ifft");

  return retval;
}

/*
 * Inverse of a square matrix.
 */
Octave_object
builtin_inv (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    DLD_BUILTIN (args, nargin, nargout, inv,
		 {
		   retval.resize (1);
		   retval(0) = inverse (args(1));
		 })
  else
    print_usage ("inv");

  return retval;
}

/*
 * Prompt user for input.
 */
Octave_object
builtin_input (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2 || nargin == 3)
    {
      retval.resize (1);
      retval(0) = get_user_input (args, nargin, nargout);
    }
  else
    print_usage ("input");

  return retval;
}

/*
 * Does the given string name a global variable?
 */
Octave_object
builtin_is_global (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval(1);
  retval(0) = tree_constant (0.0);

  if (nargin == 2 && args(1).is_string_type ())
    {
      char *name = args(1).string_value ();
      if (is_globally_visible (name))
	retval(0) = tree_constant (1.0);
    }
  else
    print_usage ("is_global");

  return retval;
}

/*
 * Is the argument a string?
 */
Octave_object
builtin_isstr (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("isstr");
  else
    {
      if (args.length () > 0 && args(1).is_defined ())
	{
	  retval.resize (1);
	  retval(0) = args(1).isstr ();
	}
    }

  return retval;
}

Octave_object
builtin_kbhit (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

// XXX FIXME XXX -- add timeout and default value args?

  if (interactive)
    {
      int c = kbhit ();
      char *s = new char [2];
      s[0] = c;
      s[1] = '\0';
      retval.resize (1);
      retval(0) = tree_constant (s);
    }

  return retval;
}

/*
 * Maybe help in debugging.
 */
Octave_object
builtin_keyboard (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 1 || nargin == 2)
    {
      retval.resize (1);
      retval(0) = get_user_input (args, nargin, nargout, 1);
    }
  else
    print_usage ("keyboard");

  return retval;
}

/*
 * Matrix logarithm.
 */
Octave_object
builtin_logm (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    retval = matrix_log (args(1));
  else
    print_usage ("logm");

  return retval;
}

/*
 * LPs.
 */
Octave_object
builtin_lpsolve (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

// Force a bad value of inform, and empty matrices for x and phi.
  retval.resize (3);
  Matrix m;
  retval(0) = tree_constant (m);
  retval(1) = tree_constant (m);
  retval(2) = tree_constant (-1.0);

  if (nargin == 0)
    DLD_BUILTIN (args, nargin, nargout, lpsolve,
		 retval = lpsolve (args, nargin, nargout);)
  else
    print_usage ("lp_solve");

  return retval;
}

Octave_object
builtin_lpsolve_options (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  DLD_BUILTIN (args, nargin, nargout, lpsolve_options,
	       retval = lpsolve_options (args, nargin, nargout);)

  return retval;
}

/*
 * ODEs.
 */
Octave_object
builtin_lsode (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if ((nargin == 4 || nargin == 5) && (nargout == 0 || nargout == 1))
    DLD_BUILTIN (args, nargin, nargout, lsode,
		 retval = lsode (args, nargin, nargout);)
  else
    print_usage ("lsode");

  return retval;
}

Octave_object
builtin_lsode_options (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  DLD_BUILTIN (args, nargin, nargout, lsode_options,
	       retval = lsode_options (args, nargin, nargout);)

  return retval;
}

/*
 * LU factorization.
 */
Octave_object
builtin_lu (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2 && nargout < 4)
    DLD_BUILTIN (args, nargin, nargout, lu,
		 retval = lu (args(1), nargout);)
  else
    print_usage ("lu");

  return retval;
}

/*
 * Max values.
 */
Octave_object
builtin_max (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if ((nargin == 2 && (nargout == 0 || nargout == 1 || nargout == 2))
      || (nargin == 3 && (nargout == 0 || nargout == 1)))
    retval = column_max (args, nargin, nargout);
  else
    print_usage ("max");

  return retval;
}

/*
 * Min values.
 */
Octave_object
builtin_min (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if ((nargin == 2 && (nargout == 0 || nargout == 1 || nargout == 2))
      || (nargin == 3 && (nargout == 0 || nargout == 1)))
    retval = column_min (args, nargin, nargout);
  else
    print_usage ("min");

  return retval;
}

/*
 * NLPs.
 */
Octave_object
builtin_npsol (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

#if defined (NPSOL_MISSING)
// Force a bad value of inform, and empty matrices for x, phi, and lambda.
  retval.resize (3);
  Matrix m;
  retval(0) = tree_constant (m);
  retval(1) = tree_constant (m);
  retval(2) = tree_constant (-1.0);
  retval(3) = tree_constant (m);
  print_usage ("npsol");
#else
  if ((nargin == 3 || nargin == 5 || nargin == 6 || nargin == 8
       || nargin == 9 || nargin == 11)
      && (nargout >= 0 && nargout <= 4))
    DLD_BUILTIN (args, nargin, nargout, npsol,
		 retval = npsol (args, nargin, nargout);)
  else
    print_usage ("npsol");
#endif

  return retval;
}

Octave_object
builtin_npsol_options (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

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
Octave_object
builtin_ones (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  switch (nargin)
    {
    case 2:
      retval.resize (1);
      retval(0) = fill_matrix (args(1), 1.0, "ones");
      break;
    case 3:
      retval.resize (1);
      retval(0) = fill_matrix (args(1), args(2), 1.0, "ones");
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
Octave_object
builtin_pause (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (! (nargin == 1 || nargin == 2))
    {
      print_usage ("pause");
      return retval;
    }

  if (interactive)
    {
      switch (nargin)
	{
	case 2:
	  {
	    int delay = NINT (args(1).double_value ());
	    if (delay > 0)
	      {
		sleep (delay);
		break;
	      }
	  }
	default:
	  if (kbhit () == EOF)
	    clean_up_and_exit (0);
	  break;
	}
    }

  return retval;
}

/*
 * Delete turds from /tmp.
 */
Octave_object
builtin_purge_tmp_files (const Octave_object& , int, int)
{
  Octave_object retval;
  cleanup_tmp_files ();
  return retval;
}

/*
 * Formatted printing.
 */
Octave_object
builtin_printf (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin < 2)
    print_usage ("printf");
  else
    retval = do_printf ("printf", args, nargin, nargout);

  return retval;
}

/*
 * Product.
 */
Octave_object
builtin_prod (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("prod");
  else
    {
      if (args.length () > 0 && args(1).is_defined ())
	{
	  retval.resize (1);
	  retval(0) = args(1).prod ();
	}
    }

  return retval;
}

/*
 * Print name of current working directory.
 */
Octave_object
builtin_pwd (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;
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
      retval.resize (1);
      retval(0) = tree_constant (s);
      delete [] s;
    }
  return retval;
}

/*
 * QPs.
 */
Octave_object
builtin_qpsol (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

#if defined (QPSOL_MISSING)
// Force a bad value of inform, and empty matrices for x, phi, and lambda.
  retval.resize (4);
  Matrix m;
  retval(0) = tree_constant (m);
  retval(1) = tree_constant (m);
  retval(2) = tree_constant (-1.0);
  retval(3) = tree_constant (m);
  print_usage ("qpsol");
#else
  if ((nargin == 4 || nargin == 6 || nargin == 7 || nargin == 9)
      && (nargout >= 0 && nargout <= 4))
    DLD_BUILTIN (args, nargin, nargout, qpsol,
		 retval = qpsol (args, nargin, nargout);)
  else
    print_usage ("qpsol");
#endif

  return retval;
}

Octave_object
builtin_qpsol_options (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

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
Octave_object
builtin_qr (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2 && nargout < 3)
    DLD_BUILTIN (args, nargin, nargout, qr,
		 retval = qr (args(1), nargout);)
  else
    print_usage ("qr");

  return retval;
}

/*
 * generalized eigenvalues via qz
 */
Octave_object
builtin_qzval (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

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
Octave_object
builtin_quad (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if ((nargin > 3 && nargin < 7) && (nargout >= 0 && nargout < 5))
    DLD_BUILTIN (args, nargin, nargout, quad,
		 retval = do_quad (args, nargin, nargout);)
  else
    print_usage ("quad");

  return retval;
}

Octave_object
builtin_quad_options (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  DLD_BUILTIN (args, nargin, nargout, quad_options,
	       retval = quad_options (args, nargin, nargout);)

  return retval;
}

/*
 * I'm outta here.
 */
Octave_object
builtin_quit (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;
  quitting_gracefully = 1;
  clean_up_and_exit (0);
  return retval;
}

/*
 * Random numbers.
 */
Octave_object
builtin_rand (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if ((nargin > 0 && nargin < 4) && (nargout == 0 || nargout == 1))
    DLD_BUILTIN (args, nargin, nargout, rand,
		 retval = rand_internal (args, nargin, nargout);)
  else
    print_usage ("rand");

  return retval;
}

/*
 * Formatted reading.
 */
Octave_object
builtin_scanf (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("scanf");
  else
    retval = do_scanf ("scanf", args, nargin, nargout);

  return retval;
}

/*
 * Convert a vector to a string.
 */
Octave_object
builtin_setstr (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    {
      retval.resize (1);
      retval(0) = args(1).convert_to_str ();
    }
  else
    print_usage ("setstr");

  return retval;
}

/*
 * Execute a shell command.
 */
Octave_object
builtin_shell_command (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2 && args(1).is_string_type ())
    {
      iprocstream cmd (args(1).string_value ());
      char ch;
      ostrstream output_buf;
      while (cmd.get (ch))
	output_buf.put (ch);
      output_buf << ends;
      int status = cmd.close ();
      switch (nargout)
	{
	case 1:
	  maybe_page_output (output_buf);
	  retval.resize (1);
	  retval(0) = tree_constant ((double) status);
	  break;
	case 2:
	  retval.resize (2);
	  retval(0) = tree_constant ((double) status);
	  retval(1) = tree_constant (output_buf.str ());
	  break;
	  break;
	}
    }
  else
    print_usage ("shell_cmd");

  return retval;
}

/*
 * Report rows and columns.
 */
Octave_object
builtin_size (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("size");
  else
    {
      if (args.length () > 0 && args(1).is_defined ())
	{
	  int nr = args(1).rows ();
	  int nc = args(1).columns ();
	  if (nargout == 0 || nargout == 1)
	    {
	      Matrix m (1, 2);
	      m.elem (0, 0) = nr;
	      m.elem (0, 1) = nc;
	      retval.resize (1);
	      retval(0) = tree_constant (m);
	    }
	  else if (nargout == 2)
	    {
	      retval.resize (2);
	      retval(0) = tree_constant ((double) nr);
	      retval(1) = tree_constant ((double) nc);
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
Octave_object
builtin_sort (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    retval = sort (args, nargin, nargout);
  else
    print_usage ("sort");

  return retval;
}

/*
 * Formatted printing to a string.
 */
Octave_object
builtin_sprintf (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin < 2)
    print_usage ("sprintf");
  else
    retval = do_printf ("sprintf", args, nargin, nargout);

  return retval;
}

/*
 * Matrix sqrt.
 */
Octave_object
builtin_sqrtm (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2)
    retval = matrix_sqrt (args(1));
  else
    print_usage ("sqrtm");

  return retval;
}

/*
 * Formatted reading from a string.
 */
Octave_object
builtin_sscanf (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 3)
    print_usage ("sscanf");
  else
    retval = do_scanf ("sscanf", args, nargin, nargout);

  return retval;
}

/*
 * Sum.
 */
Octave_object
builtin_sum (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("sum");
  else
    {
      if (args.length () > 0 && args(1).is_defined ())
	{
	  retval.resize (1);
	  retval(0) = args(1).sum ();
	}
    }

  return retval;
}

/*
 * Sum of squares.
 */
Octave_object
builtin_sumsq (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin != 2)
    print_usage ("sumsq");
  else
    {
      if (args.length () > 0 && args(1).is_defined ())
	{
	  retval.resize (1);
	  retval(0) = args(1).sumsq ();
	}
    }

  return retval;
}

/*
 * Singluar value decomposition.
 */
Octave_object
builtin_svd (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2 && (nargout == 0 || nargout == 1 || nargout == 3))
    DLD_BUILTIN (args, nargin, nargout, svd,
		 retval = svd (args, nargin, nargout);)
  else
    print_usage ("svd");

  return retval;
}

/*
 * Sylvester equation solver.
 */
Octave_object
builtin_syl (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if ((nargin == 4) && (nargout == 0 || nargout == 1))
    DLD_BUILTIN (args, nargin, nargout, syl,
		 retval = syl (args, nargin, nargout);)
  else
    print_usage ("syl");

  return retval;
}

/*
 * Schur Decomposition.
 */
Octave_object
builtin_schur (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if ((nargin == 3 || nargin == 2)
      && (nargout == 0 || nargout == 1 || nargout == 2))
    DLD_BUILTIN (args, nargin, nargout, schur,
		 retval = schur (args, nargin, nargout);)
  else
    print_usage ("schur");

  return retval;
}

/*
 * Givens rotation.
 */
Octave_object
builtin_givens (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 3 && (nargout == 0 || nargout == 1 || nargout == 2 ))
    retval = givens (args, nargin, nargout);
  else
    print_usage ("givens");

  return retval;
}

/*
 * Hessenberg Decomposition.
 */
Octave_object
builtin_hess (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 2 && (nargout == 0 || nargout == 1 || nargout == 2))
    DLD_BUILTIN (args, nargin, nargout, hess,
		 retval = hess (args, nargin, nargout);)
  else
    print_usage ("hess");

  return retval;
}

/*
 * Variable argument lists.
 */
Octave_object
builtin_va_arg (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;
  if (nargin == 1)
    {
      if (curr_function != (tree_function *) NULL)
	{
	  if (curr_function->takes_varargs ())
	    {
	      retval.resize (1);
	      retval(0) = curr_function->octave_va_arg ();
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

Octave_object
builtin_va_start (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;
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
Octave_object
builtin_warranty (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

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

  return retval;
}

/*
 * A matrix of zeros.
 */
Octave_object
builtin_zeros (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  switch (nargin)
    {
    case 2:
      retval.resize (1);
      retval(0) = fill_matrix (args(1), 0.0, "zeros");
      break;
    case 3:
      retval.resize (1);
      retval(0) = fill_matrix (args(1), args(2), 0.0, "zeros");
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
