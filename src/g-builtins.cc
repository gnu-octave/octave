// g-builtins.cc                                           -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#ifdef __GNUG__
#pragma implementation
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

// Signal handler return type.
#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif
#ifndef BADSIG
#define BADSIG (RETSIGTYPE (*)())-1
#endif

typedef RETSIGTYPE sig_handler (...);

/*
 * Are all elements of a constant nonzero?
 */
tree_constant *
builtin_all (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  if (nargin != 2)
    usage ("all (M)");
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
    usage ("any (M)");
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
    usage ("[{dd,} aa] = balance (a, {opt}) or \n\
[{cc, dd,} aa, bb] = balance (a, b {,opt}), opt = 'P' or 'S'");
  else
    {
      DLD_BUILTIN (args, nargin, nargout, balance,
		   retval = balance (args, nargin, nargout));
    }
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
  rl_clear_screen ();
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
    usage ("[r, A, B, q] = colloc (n [, \"left\"] [, \"right\"])");
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
    usage ("cumprod (M)");
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
    usage ("cumsum (M)");
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
static void
dassl_usage (void)
{
  usage ("dassl (\"function_name\", x_0, xdot_0, t_out\n\
       dassl (\"function_name\", x_0, xdot_0, t_out, t_crit)\n\
\n\
       The first argument is the name of the function to call to\n\
       compute the vector of residuals.  It must have the form\n\
\n\
         res = f (x, xdot, t)\n\
\n\
       where x, xdot, and res are vectors, and t is a scalar.");
}

tree_constant *
builtin_dassl (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = new tree_constant [2];

  if ((nargin == 5 || nargin == 6) && nargout > 0)
    DLD_BUILTIN (args, nargin, nargout, dassl,
		 retval = dassl (args, nargin, nargout);)
  else
    dassl_usage ();

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
    usage ("det (a)");

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
    usage ("diag (X [, k])");

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
    usage ("disp (X)");

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
    usage ("lambda = eig (A)\n\
       [v, d] = eig (A); d == diag (lambda)");

  return retval;
}

/*
 * Print error message and jump to top level.
 */
tree_constant *
builtin_error (tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 && args != NULL_TREE_CONST && args[1].is_defined ())
    args[1].print_if_string (cerr, 1);
  else
    message ((char *) NULL, "unspecified error, jumping to top level...");

  jump_to_top_level ();

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
    usage ("eval (\"string\")");
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
    usage ("exist (\"string\")");
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
    usage ("expm (A)");

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
      usage ("eye (n)\n       eye (A)\n       eye (n, m)");
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
    usage ("success = fclose (\"filename\" or filenum)");
  else
    retval = fclose_internal (args);
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
    usage ("feval (\"function_name\" [, ...])");
  return retval;
}

/*
 * Flushing output to a file
 */
tree_constant *
builtin_fflush (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  if (nargin != 2)
    usage ("success = fflush (\"filename\" or filenum)");
  else
    retval = fflush_internal (args);
  return retval;
}

/*
 * Fast Fourier Transform
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
    usage ("fft (a)");

  return retval;
}

/*
 * get a string from a file
 */
tree_constant *
builtin_fgets (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  if (nargin != 3  && nargout < 3)
    usage ("string = fgets (\"filename\" or filenum, length)");
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
    usage ("find (x)");
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
    usage ("flops\n       flops (0)");

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
    {
      usage ("filenum = fopen (\"file\", \"mode\")\n\n\
 Legal values for mode include:\n\n\
   r  : open text file for reading\n\
   w  : open text file for writing; discard previous contents if any\n\
   a  : append; open or create text file for writing at end of file\n\
   r+ : open text file for update (i.e., reading and writing)\n\
   w+ : create text file for update; discard previous contents if any\n\
   a+ : append; open or create text file for update, writing at end\n\n\
 Update mode permits reading from and writing to the same file.\n");
      }
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
    usage ("fprintf (\"filename\" or filenum, \"fmt\" [, ...])");
  else
    retval = do_printf ("fprintf", args, nargin, nargout);
  return retval;
}

/*
 * rewind a file
 */
tree_constant *
builtin_frewind (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  if (nargin != 2)
    usage ("success = frewind (\"filename\" or filenum)");
  else
    retval = frewind_internal (args);
  return retval;
}

/*
 * report on open files
 */
tree_constant *
builtin_freport (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  if (nargin > 1)
    warning ("replot: ignoring extra arguments");
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
    usage ("[...] = fscanf (\"file\", \"fmt\")");
  else
    retval = do_scanf ("fscanf", args, nargin, nargout);
  return retval;
}

/*
 * seek a point in a file for reading and/or writing 
 */
tree_constant *
builtin_fseek (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  if (nargin != 3 && nargin != 4)
    usage ("success = fseek (\"filename\" or filenum, offset [,origin])");
  else
    retval = fseek_internal (args, nargin);
  return retval;
}

/*
 * Nonlinear algebraic equations.
 */
static void
fsolve_usage (void)
{
//  usage ("[x, status, path] = fsolve (\"f\", x0 [, opts] [, par] [, \"jac\"] [, scale])");

  usage ("[x, info] = fsolve (\"f\", x0)");
}

tree_constant *
builtin_fsolve (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin >= 3 && nargin <= 7 && nargout >= 1 && nargout <= 3)
    DLD_BUILTIN (args, nargin, nargout, fsolve,
		 retval = fsolve (args, nargin, nargout);)
  else
    fsolve_usage ();

  return retval;
}

/*
 * NLPs.
 */
static void
fsqp_usage (void)
{
#if defined (FSQP_MISSING)
  message ("fsqp", "this function requires FSQP, which is not freely\n\
      redistributable.  For more information, read the file\n\
      libcruft/fsqp/README.MISSING in the source distribution.");
#else
  usage ("[x, phi] = fsqp (x, \"phi\" [, lb, ub] [, lb, A, ub] [, lb, \"g\", ub])\n\n\
  Groups of arguments surrounded in `[]' are optional, but\n\
  must appear in the same relative order shown above.");
#endif
}

tree_constant *
builtin_fsqp (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

#if defined (FSQP_MISSING)
  fsqp_usage ();
#else
  if ((nargin == 3 || nargin == 5 || nargin == 6 || nargin == 8
       || nargin == 9 || nargin == 11)
      && (nargout >= 1 && nargout <= 3))
    DLD_BUILTIN (args, nargin, nargout, fsqp,
		 retval = fsqp (args, nargin, nargout);)
  else
    fsqp_usage ();
#endif

  return retval;
}

/*
 * tell current position of file
 */
tree_constant *
builtin_ftell (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  if (nargin != 2)
    usage ("position = ftell (\"filename\" or filenumber)");
  else
    retval = ftell_internal (args);
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
    usage ("getenv (\"string\")");
  return retval;
}

/*
 * Inverse Fast Fourier Transform
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
    usage ("ifft (a)");

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
    usage ("inv (A)");

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
    usage ("input (\"prompt\" [, \"s\"])");

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
    usage ("isstr (value)");
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
    usage ("keyboard (\"prompt\")");

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
    usage ("logm (A)");

  return retval;
}

/*
 * LPs.
 */
static void
lpsolve_usage (void)
{
  usage ("[x, obj, info] = lpsolve (XXX FIXME XXX)");
}

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
    lpsolve_usage ();

  return retval;
}

/*
 * ODEs.
 */
static void
lsode_usage (void)
{
  usage ("lsode (\"function_name\", x0, t_out\n\
       lsode (\"function_name\", x0, t_out, t_crit)\n\
\n\
       The first argument is the name of the function to call to\n\
       compute the vector of right hand sides.  It must have the form\n\
\n\
         xdot = f (x, t)\n\
\n\
       where xdot and x are vectors and t is a scalar.");
}

tree_constant *
builtin_lsode (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if ((nargin == 4 || nargin == 5) && nargout == 1)
    DLD_BUILTIN (args, nargin, nargout, lsode,
		 retval = lsode (args, nargin, nargout);)
  else
    lsode_usage ();

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
    usage ("[L, U, P] = lu (A)");

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
    usage ("[X, I] = max (A)\n        X = max (A)\n        X = max (A, B)");

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
    usage ("[X, I] = min (A)\n        X = min (A)\n        X = min (A, B)");

  return retval;
}

/*
 * NLPs.
 */
static void
npsol_usage (void)
{
#if defined (NPSOL_MISSING)
  message ("npsol", "this function requires NPSOL, which is not freely\n\
       redistributable.  For more information, read the file\n\
       libcruft/npsol/README.MISSING in the source distribution.");
#else
  usage ("\n\n\
  [x, obj, info, lambda] = npsol (x, \"phi\" [, lb, ub] [, lb, A, ub] [, lb, \"g\", ub])\n\n\
  Groups of arguments surrounded in `[]' are optional, but\n\
  must appear in the same relative order shown above.\n\
\n\
  The second argument is a string containing the name of the objective\n\
  function to call.  The objective function must be of the form\n\
\n\
    y = phi (x)\n\
\n\
  where x is a vector and y is a scalar.");
#endif
}

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
  npsol_usage ();
#else
  if ((nargin == 3 || nargin == 5 || nargin == 6 || nargin == 8
       || nargin == 9 || nargin == 11)
      && (nargout >= 1 && nargout <= 4))
    DLD_BUILTIN (args, nargin, nargout, npsol,
		 retval = npsol (args, nargin, nargout);)
  else
    npsol_usage ();
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
      usage ("ones (n)\n       ones (A)\n       ones (n, m)");
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
      usage ("pause ([delay])");
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
    usage ("printf (\"fmt\" [, ...])");
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
    usage ("prod (M)");
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
	  message ("pwd", "can't find working directory!");
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
static void
qpsol_usage (void)
{
#if defined (QPSOL_MISSING)
  message ("qpsol", "this function requires QPSOL, which is not freely\n\
       redistributable.  For more information, read the file\n\
       libcruft/qpsol/README.MISSING in the source distribution.");
#else
  usage ("[x, obj, info, lambda] = qpsol (x, H, c [, lb, ub] [, lb, A, ub])\n\
\n\
  Groups of arguments surrounded in `[]' are optional, but\n\
  must appear in the same relative order shown above.");
#endif
}

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
  qpsol_usage ();
#else
  if ((nargin == 4 || nargin == 6 || nargin == 7 || nargin == 9)
      && (nargout >= 1 && nargout <= 4))
    DLD_BUILTIN (args, nargin, nargout, qpsol,
		 retval = qpsol (args, nargin, nargout);)
  else
    qpsol_usage ();
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
    usage ("[Q, R] = qr (A)");

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
    usage ("x = qzval (A,B): compute generalized eigenvalues of \n\
  the matrix pencil (A - lambda B).  A and B must be real matrices.\n");

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
    usage ("[v, ier, nfun, err] = quad (\"f\", a, b)\n\
                           = quad (\"f\", a, b, tol)\n\
                           = quad (\"f\", a, b, tol, sing)");

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
    usage ("rand                  -- generate a random value\n\
       rand (n)              -- generate N x N matrix\n\
       rand (A)              -- generate matrix the size of A\n\
       rand (n, m)           -- generate N x M matrix\n\
       rand (\"dist\")         -- get current distribution\n\
       rand (\"distribution\") -- set distribution\n\
       rand (\"seed\")         -- get current seed\n\
       rand (\"seed\", n)      -- set seed");

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
    usage ("[...] = scanf (\"fmt\")");
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
    usage ("setstr (v)");

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
    usage ("shell_cmd (string [, return_output])");

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
    usage ("size (x)");
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
	    usage ("[n, m] = size (A)\n                size (A)");
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
    usage ("[s, i] = sort (x)");

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
    usage ("string = sprintf (\"fmt\" [, ...])");
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
    usage ("sqrtm (A)");

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
    usage ("[...] = sscanf (string, \"fmt\")");
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
    usage ("sum (M)");
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
    usage ("sumsq (M)");
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
    usage ("[U, S, V] = svd (A)\n               S = svd (A)");

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
    usage ("X = syl (A,B,C)");

  return retval;
}

/*
 * Schur Decomposition
 */
tree_constant *
builtin_schur (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if ((nargin == 3 || nargin == 2) && (nargout == 1 || nargout == 2))
    DLD_BUILTIN (args, nargin, nargout, hess,
		 retval = schur (args, nargin, nargout);)
  else
    usage ("[U, S] = schur (A)\n\
            S = schur (A)\n\n\
 or, for ordered Schur:\n\n\
       [U, S] = schur (A, \"A, D, or U\")\n\
            S = schur (A, \"A, D, or U\")\n\
 where:\n\n\
   A = continuous time poles\n\
   D = discrete time poles\n\
   U = unordered schur (default)");

  return retval;
}

/*
 * Givens rotation
 */
tree_constant *
builtin_givens (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 3 && (nargout == 1 || nargout == 2 ))
    retval = givens (args, nargin, nargout);
  else
    usage ("[c, s] = givens (x,y)\n            G = givens (x,y)");

  return retval;
}

/*
 * Hessenberg Decomposition
 */
tree_constant *
builtin_hess (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 2 && (nargout == 1 || nargout == 2))
    DLD_BUILTIN (args, nargin, nargout, hess,
		 retval = hess (args, nargin, nargout);)
  else
    usage ("[P, H] = hess (A)\n            H = hess (A)");

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
	     << ".  Copyright (C) 1992, 1993, John W. Eaton\n"
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
      usage ("zeros (n)\n       zeros (A)\n       zeros (n, m)");
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
