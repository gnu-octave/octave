// builtins.cc                                           -*- C++ -*-
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

#ifdef __GNUG__
#pragma implementation
#endif

#include <iostream.h>
#include <strstream.h>
#include <math.h>
#include <float.h>

#include "tree-const.h"
#include "symtab.h"
#include "t-builtins.h"
#include "g-builtins.h"
#include "builtins.h"
#include "octave.h"
#include "utils.h"
#include "tree.h"
#include "help.h"
#include "pager.h"
#include "mappers.h"
#include "user-prefs.h"
#include "variables.h"

// NOTE: nargin == 1 means that the function takes no arguments (just
// like C, the first argument is (or should be, anyway) the function
// name).  Also, -1 is shorthand for infinity.

// The following initializations may eventually need to be reworked
// like the builtin functions in bash were around version 1.12...

static builtin_mapper_functions mapper_functions[] =
{
  { "abs", 2, 1, 0, fabs, abs, NULL,
    "abs (X): compute abs (X) for each element of X", },

  { "acos", 2, 1, 0, acos, NULL, acos,
    "acos (X): compute acos (X) for each element of X", },

  { "acosh", 2, 1, 0, acosh, NULL, acosh,
    "acosh (X): compute acosh (X) for each element of X", },

  { "angle", 2, 1, 0, arg, arg, NULL,
    "angle (X): compute arg (X) for each element of X", },

  { "arg", 2, 1, 0, arg, arg, NULL,
    "arg (X): compute arg (X) for each element of X", },

  { "asin", 2, 1, 0, asin, NULL, asin,
    "asin (X): compute asin (X) for each element of X", },

  { "asinh", 2, 1, 0, asinh, NULL, asinh,
    "asinh (X): compute asinh (X) for each element of X", },

  { "atan", 2, 1, 0, atan, NULL, atan,
    "atan (X): compute atan (X) for each element of X", },

  { "atanh", 2, 1, 0, atanh, NULL, atanh,
    "atanh (X): compute atanh (X) for each element of X", },

  { "ceil", 2, 1, 0, ceil, NULL, ceil,
    "ceil (X): round elements of X toward +Inf", },

  { "conj", 2, 1, 0, conj, NULL, conj,
    "conj (X): compute complex conjugate for each element of X", },

  { "cos", 2, 1, 0, cos, NULL, cos,
    "cos (X): compute cos (X) for each element of X", },

  { "cosh", 2, 1, 0, cosh, NULL, cosh,
    "cosh (X): compute cosh (X) for each element of X", },

  { "exp", 2, 1, 0, exp, NULL, exp,
    "exp (X): compute exp (X) for each element of X", },

  { "finite", 2, 1, 0, xfinite, xfinite, NULL,
    "finite (X): return 1 for finite elements of X", },

  { "fix", 2, 1, 0, fix, NULL, fix,
    "fix (X): round elements of X toward zero", },

  { "floor", 2, 1, 0, floor, NULL, floor,
    "floor (X): round elements of X toward -Inf", },

  { "isinf", 2, 1, 0, xisinf, xisinf, NULL,
    "isinf (X): return 1 for elements of X infinite", },

  { "imag", 2, 1, 0, imag, imag, NULL,
    "imag (X): return imaginary part for each elements of X", },

#ifdef HAVE_ISNAN
  { "isnan", 2, 1, 0, xisnan, xisnan, NULL,
    "isnan (X): return 1 where elements of X are NaNs", },
#endif

  { "log", 2, 1, 1, log, NULL, log,
    "log (X): compute log (X) for each element of X", },

  { "log10", 2, 1, 1, log10, NULL, log10,
    "log10 (X): compute log10 (X) for each element of X", },

  { "real", 2, 1, 0, real, real, NULL,
    "real (X): return real part for each element of X", },

  { "round", 2, 1, 0, round, NULL, round,
    "round (X): round elements of X to nearest integer", },

  { "sign", 2, 1, 0, signum, NULL, signum,
    "sign (X): apply signum function to elements of X", },

  { "sin", 2, 1, 0, sin, NULL, sin,
    "sin (X): compute sin (X) for each element of X", },

  { "sinh", 2, 1, 0, sinh, NULL, sinh,
    "sinh (X): compute sinh (X) for each element of X", },

  { "sqrt", 2, 1, 1, sqrt, NULL, sqrt,
    "sqrt (X): compute sqrt (X) for each element of X", },

  { "tan", 2, 1, 0, tan, NULL, tan,
    "tan (X): compute tan (X) for each element of X", },

  { "tanh", 2, 1, 0, tanh, NULL, tanh,
    "tanh (X): compute tanh (X) for each element of X", },

  { NULL, -1, -1, -1, NULL, NULL, NULL, NULL, },
};

static builtin_text_functions text_functions[] =
{
  { "casesen", 2, builtin_casesen,
    "casesen [on|off]", },

  { "cd", 2, builtin_cd,
    "cd [dir]\n\n\
change current working directory\n\
if no arguments are given, the current directory is changed to the\n\
users home directory", },

  { "clear", -1, builtin_clear,
    "clear [name ...]\n\n\
clear symbol(s) matching a list of regular expressions\n\
if no arguments are given, clear all user-defined variables and functions", },

  { "dir", -1, builtin_ls,
    "dir [options]\n\nprint a directory listing", },

  { "document", -1, builtin_document,
    "document symbol string ...\n\n", },

  { "edit_history", -1, builtin_edit_history,
    "edit_history [first] [last]\n\nedit commands from the history list", },

  { "format", -1, builtin_format,
    "format [style]\n\nset output formatting style", },

  { "help", -1, builtin_help,
    "help [-i] [topic ...]\n\nprint cryptic yet witty messages", },

  { "history", -1, builtin_history,
    "history [N] [-w file] [-r file] [-q]\n\n\
display, save, or load command history", },

  { "load", -1, builtin_load,
    "load [-force] file\n\nload variables from a file", },

  { "ls", -1, builtin_ls,
    "ls [options]\n\nprint a directory listing", },

  { "run_history", -1, builtin_run_history,
    "run_history [first] [last]\n\nrun commands from the history list", },

  { "save", -1, builtin_save,
    "save file [var ...]\n\nsave variables in a file", },

  { "set", -1, builtin_set,
    "set [options]\n\nset plotting options", },

  { "show", -1, builtin_show,
    "show [options]\n\nshow plotting options", },

  { "who", -1, builtin_who,
    "who [-global] [-top] [-local] [-all] [-fcn] [name ...]\n\n\
list symbol(s) matching a list of regular expressions\n\
if no arguments are given, the list of local symbols is printed", },

  { NULL, -1, NULL, NULL, },
};

static builtin_general_functions general_functions[] =
{
  { "all", 2, 1, builtin_all,
    "all (X): are all elements of X nonzero?", },

  { "any", 2, 1, builtin_any,
    "any (X): are any elements of X nonzero?", },

  { "balance", 4, 4, builtin_balance,
    "aa = balance (a [, opt]) or [[dd,] aa] =  balance (a [, opt])\n\
  generalized eigenvalue problem:\n\
    [cc, dd, aa, bb] = balance (a, b [, opt])\n\
where \"opt\" is an optional single character argument as follows: \n\
  \"N\" or \"n\": no balancing; arguments copied, transformation(s) \n\
              set to identity\n\
  \"P\" or \"p\": permute argument(s) to isolate eigenvalues where possible\n\
  \"S\" or \"s\": scale to improve accuracy of computed eigenvalues\n\
  \"B\" or \"b\": (default) permute and scale, in that order. Rows/columns of a \n\
              (and b) that are isolated by permutation are not scaled\n\
[dd, aa] = balance (a, opt) returns aa = dd\a*dd,\n\
[cc, dd, aa, bb] = balance (a, b, opt) returns aa (bb) = cc*a*dd (cc*b*dd)", },

  { "chol", 2, 1, builtin_chol,
    "chol (X): cholesky factorization", },

  { "clc", 1, 0, builtin_clc,
    "clc (): clear screen", },

  { "clock", 1, 0, builtin_clock,
    "clock (): return current date and time in vector", },

  { "closeplot", 1, 0, builtin_closeplot,
    "closeplot (): close the stream to plotter", },

  { "colloc", 7, 4, builtin_colloc,
    "[r, A, B, q] = colloc (n [, \"left\"] [, \"right\"]): collocation weights", },

  { "cumprod", 2, 1, builtin_cumprod,
    "cumprod (X): cumulative products", },

  { "cumsum", 2, 1, builtin_cumsum,
    "cumsum (X): cumulative sums", },

  { "dassl", 5, 1, builtin_dassl,
    "dassl (\"function_name\", x_0, xdot_0, t_out)\n\
  dassl (\"function_name\", x_0, xdot_0, t_out, t_crit)\n\
\n\
The first argument is the name of the function to call to\n\
compute the vector of residuals.  It must have the form\n\
\n\
  res = f (x, xdot, t)\n\
\n\
where x, xdot, and res are vectors, and t is a scalar.", },

  { "date", 1, 0, builtin_date,
    "date (): return current date in a string", },

  { "det", 2, 1, builtin_det,
    "det (X): determinant of a square matrix", },

  { "diag", 3, 1, builtin_diag,
    "diag (X [,k]): form/extract diagonals", },

  { "disp", 3, 1, builtin_disp,
    "disp (X): display value", },

  { "eig", 2, 1, builtin_eig,
    "eig (x) or [v, d] = eig (x): compute eigenvalues and eigenvectors of x", },

  { "error", 2, 1, builtin_error,
    "error (\"message\"): print message and jump to top level", },

  { "eval", 2, 1, builtin_eval,
    "eval (\"string\"): evaluate text as octave source", },

  { "exist", 2, 1, builtin_exist,
    "exist (\"name\"): check if variable or file exists", },

  { "exit", 1, 0, builtin_quit,
    "exit (): exit Octave gracefully", },

  { "expm", 2, 1, builtin_expm,
    "expm (X): matrix exponential, e^A", },

  { "eye", 3, 1, builtin_eye,
    "eye (n), eye (n, m), eye (X): create an identity matrix", },

  { "fclose", 2, 1, builtin_fclose,
    "fclose (\"filename\" or filenum): close a file", },

  { "feval", -1, 1, builtin_feval,
    "feval (\"name\", args, ...): evaluate first argument as function", },

  { "fflush", 2, 1, builtin_fflush,
    "fflush (\"filename\" or filenum): flush buffered data to output file", },

  { "fft", 2, 1, builtin_fft,
    "fft (X): fast fourier transform of a vector", },

  { "fgets",3, 2, builtin_fgets,
    "[string, length] = fgets (\"filename\" or filenum, length): read a string from a file", },

  { "find", -1, 1, builtin_find,
    "find (x): return vector of indices of nonzero elements", },

  { "flops", 2, 1, builtin_flops,
    "flops (): count floating point operations", },

  { "fopen", 3, 1, builtin_fopen,
    "filenum = fopen (\"filename\", \"mode\"): open a file\n\n\
 Legal values for mode include:\n\n\
   r  : open text file for reading\n\
   w  : open text file for writing; discard previous contents if any\n\
   a  : append; open or create text file for writing at end of file\n\
   r+ : open text file for update (i.e., reading and writing)\n\
   w+ : create text file for update; discard previous contents if any\n\
   a+ : append; open or create text file for update, writing at end\n\n\
 Update mode permits reading from and writing to the same file.\n", },

  { "fprintf", -1, 1, builtin_fprintf,
    "fprintf (\"file\", \"fmt\", ...)", },

  { "freport", 1, 1, builtin_freport,
    "freport (): list open files and their status", },

  { "frewind", 2, 1, builtin_frewind,
    "frewind (\"filename\" or filenum): set file position at beginning of file", },

  { "fscanf", 3, -1, builtin_fscanf,
    "[a, b, c, ...] = fscanf (\"file\", \"fmt\")", },

  { "fseek", 4, 1, builtin_fseek,
    "fseek (\"filename\" or filenum, offset [, origin]): set file position for reading or writing", },

  { "fsolve", 5, 1, builtin_fsolve,
    "Solve nonlinear equations using Minpack.  Usage:\n\
\n\
  [x, info] = fsolve (\"f\", x0)\n\
\n\
Where the first argument is the name of the  function to call to\n\
compute the vector of function values.  It must have the form\n\
\n\
  y = f (x)
\n\
where y and x are vectors.", },

  { "fsqp", 11, 3, builtin_fsqp,
#if defined (FSQP_MISSING)
    "This function requires FSQP, which is not freely\n\
       redistributable.  For more information, read the file\n\
       libcruft/fsqp/README.MISSING in the source distribution.", },
#else
  "[x, phi] = fsqp (x, \"phi\" [, lb, ub] [, lb, A, ub] [, lb, \"g\", ub])\n\n\
  Groups of arguments surrounded in `[]' are optional, but\n\
  must appear in the same relative order shown above.", },
#endif

  { "ftell", 2, 1, builtin_ftell,
    "position = ftell (\"filename\" or filenum): returns the current file position", },

  { "getenv", 2, 1, builtin_getenv,
    "getenv (\"string\"): get environment variable values", },

  { "givens", 3, 2, builtin_givens,
    "G = givens (x, y): compute orthogonal matrix G = [c s; -conj (s) c]\n\
      such that G [x; y] = [*; 0]  (x, y scalars)\n\n\
      [c, s] = givens (x, y) returns the (c, s) values themselves.", },
  

  { "hess", 2, 2, builtin_hess,
    "[P, H] = hess (A) or H = hess (A): Hessenberg decomposition", },

  { "home", 1, 0, builtin_clc,
    "home (): clear screen", },

  { "input", 3, 1, builtin_input,
    "input (\"prompt\" [, \"s\"]): prompt user for [string] input", },

  { "ifft", 2, 1, builtin_ifft,
    "ifft (X): inverse fast fourier transform of a vector", },

  { "inv", 2, 1, builtin_inv,
    "inv (X): inverse of a square matrix", },

  { "inverse", 2, 1, builtin_inv,
    "inverse (X): inverse of a square matrix", },

  { "isstr", 2, 1, builtin_isstr,
    "isstr (X): return 1 if X is a string", },

  { "keyboard", 2, 1, builtin_keyboard,
    "keyboard (\"prompt\"): maybe help in debugging M-files", },

  { "logm", 2, 1, builtin_logm,
    "logm (x): matrix logarithm", },

  { "lp_solve", 11, 3, builtin_lpsolve,
    "lp_solve (): solve linear programs using lp_solve.", },

  { "lsode", 6, 1, builtin_lsode,
    "lsode (\"function_name\", x0, t_out, t_crit)\n\
\n\
The first argument is the name of the function to call to\n\
compute the vector of right hand sides.  It must have the form\n\
\n\
  xdot = f (x, t)\n\
\n\
where xdot and x are vectors and t is a scalar.\n", },

  { "lu", 2, 3, builtin_lu,
    "[L, U, P] = lu (A): LU factorization", },

  { "max", 3, 2, builtin_max,
    "max (x): maximum value(s) of a vector (matrix)", },

  { "min", 3, 2, builtin_min,
    "min (x): minimum value(s) of a vector (matrix)", },

  { "npsol", 11, 3, builtin_npsol,
#if defined (NPSOL_MISSING)
    "This function requires NPSOL, which is not freely\n\
       redistributable.  For more information, read the file\n\
       libcruft/npsol/README.MISSING in the source distribution.", },
#else
    "[x, obj, info, lambda] = npsol (x, \"phi\" [, lb, ub] [, lb, A, ub] [, lb, \"g\", ub])\n\n\
  Groups of arguments surrounded in `[]' are optional, but\n\
  must appear in the same relative order shown above.\n\
\n\
  The second argument is a string containing the name of the objective\n\
  function to call.  The objective function must be of the form\n\
\n\
    y = phi (x)\n\
\n\
  where x is a vector and y is a scalar.", },
#endif

  { "ones", 3, 1, builtin_ones,
    "ones (n), ones (n, m), ones (x): create a matrix of all ones", },

  { "pause", 1, 0, builtin_pause,
    "pause (seconds): suspend program execution", },

  { "purge_tmp_files", 5, 1, builtin_purge_tmp_files,
    "delete temporary data files used for plotting", },

  { "printf", -1, 1, builtin_printf,
    "printf (\"fmt\", ...)", },

  { "prod", 2, 1, builtin_prod,
    "prod (X): products", },

  { "pwd", 1, 0, builtin_pwd,
    "pwd (): print current working directory", },

  { "qpsol", 9, 3, builtin_qpsol,
#if defined (QPSOL_MISSING)
    "This function requires QPSOL, which is not freely\n\
       redistributable.  For more information, read the file\n\
       libcruft/qpsol/README.MISSING in the source distribution.", },
#else
    "[x, obj, info, lambda] = qpsol (x, H, c [, lb, ub] [, lb, A, ub])\n\
\n\
  Groups of arguments surrounded in `[]' are optional, but\n\
  must appear in the same relative order shown above.", },
#endif

  { "qr", 2, 2, builtin_qr,
    "[q, r] = qr (X): form QR factorization of X", },

  { "quad", 6, 3, builtin_quad,
    "[v, ier, nfun] = quad (\"f\", a, b [, tol] [, sing])\n\
\n\
Where the first argument is the name of the  function to call to\n\
compute the value of the integrand.  It must have the form\n\
\n\
  y = f (x)
\n\
where y and x are scalars.\n\
\n\
The second and third arguments are limits of integration.  Either or\n\
both may be infinite.  The optional argument tol specifies the desired\n\
accuracy of the result.  The optional argument sing is a vector of\n\
at which the integrand is singular.\n", },

  { "quit", 1, 0, builtin_quit,
    "quit (): exit Octave gracefully", },

  { "qzval", 3, 1, builtin_qzval,
    "x = qzval (A,B): compute generalized eigenvalues of \n\
  the matrix pencil (A - lambda B).  A and B must be real matrices.", },

  { "rand", 2, 1, builtin_rand,
    "rand                  -- generate a random value\n\
       rand (n)              -- generate N x N matrix\n\
       rand (A)              -- generate matrix the size of A\n\
       rand (n, m)           -- generate N x M matrix\n\
       rand (\"dist\")         -- get current distribution\n\
       rand (\"distribution\") -- set distribution\n\
       rand (\"seed\")         -- get current seed\n\
       rand (\"seed\", n)      -- set seed", },

  { "replot", 1, 0, builtin_replot,
    "replot (): redisplay current plot", },

  { "scanf", 2, -1, builtin_scanf,
    "[a, b, c, ...] = scanf (\"fmt\")", },

  { "setstr", 2, 1, builtin_setstr,
    "setstr (v): convert a vector to a string", },

  { "shell_cmd", 2, 1, builtin_shell_command,
    "shell_cmd (string [, return_output]): execute shell commands", },

  { "schur", 3, 2, builtin_schur,
    "[U, S] = schur (A) or S = schur (A)\n\n\
 or, for ordered Schur:\n\n\
       [U, S] = schur (A, \"A, D, or U\") or S = schur (A, \"A, D, or U\")\n\
 where:\n\n\
   A = continuous time poles\n\
   D = discrete time poles\n\
   U = unordered schur (default)", },


  { "size", 2, 1, builtin_size,
    "[m, n] = size (x): return rows and columns of X", },

  { "sort", 2, 2, builtin_sort,
    "[s, i] = sort (x): sort the columns of x, optionally return sort index", },

  { "sqrtm", 2, 1, builtin_sqrtm,
    "sqrtm (x): matrix sqrt", },

  { "sprintf", -1, 1, builtin_sprintf,
    "s = sprintf (\"fmt\", ...)", },

  { "sscanf", 3, -1, builtin_sscanf,
    "[a, b, c, ...] = sscanf (string, \"fmt\")", },

  { "sum", 2, 1, builtin_sum,
    "sum (X): sum of elements", },

  { "sumsq", 2, 1, builtin_sumsq,
    "sumsq (X): sum of squares of elements", },

  { "svd", 2, 3, builtin_svd,
    "s = svd (x) or [u, s, v] = svd (x): return SVD of x", },

  { "syl", 4, 1, builtin_syl,
    "X = syl (A, B, C): solve the Sylvester equation A X + X B + C = 0", },

  { "warranty", 1, 0, builtin_warranty,
    "warranty (): describe copying conditions", },

  { "zeros", 3, 1, builtin_zeros,
    "zeros (n), zeros (n, m), zeros (x): create a matrix of all zeros", },

  { NULL, -1, -1, NULL, NULL, },
};

// This is a lie.  Some of these get reassigned to be numeric
// variables.  See below.

static builtin_string_variables string_variables[] =
{
  { "I", "??", NULL,
    "sqrt (-1)", },

  { "Inf", "??", NULL,
    "infinity", },

  { "INFO_FILE", "??", sv_info_file,
    "name of the Octave info file", },

  { "J", "??", NULL,
    "sqrt (-1)", },

#if defined (HAVE_ISNAN)
  { "NaN", "??", NULL,
    "not a number", },
#endif

  { "LOADPATH", "??", sv_loadpath,
    "colon separated list of directories to search for scripts", },

  { "PAGER", "??", sv_pager_binary,
    "path to pager binary", },

  { "PS1", "\\s:\\#> ", sv_ps1,
    "primary prompt string", },

  { "PS2", "> ", sv_ps2,
    "secondary prompt string", },

  { "PWD", "??PWD??", sv_pwd,
    "current working directory", },

  { "SEEK_SET", "??", NULL,
    "used with fseek to position file relative to the beginning", },

  { "SEEK_CUR", "??", NULL,
    "used with fseek to position file relative to the current position", },

  { "SEEK_END", "??", NULL,
    "used with fseek to position file relative to the end", },

  { "do_fortran_indexing", "false", do_fortran_indexing,
    "allow single indices for matrices", },

  { "empty_list_elements_ok", "warn", empty_list_elements_ok,
    "ignore the empty element in expressions like `a = [[], 1]'", },

  { "eps", "??", NULL,
    "machine precision", },

  { "gnuplot_binary", "gnuplot", sv_gnuplot_binary,
    "path to gnuplot binary", },

  { "i", "??", NULL,
    "sqrt (-1)", },

  { "implicit_str_to_num_ok", "false", implicit_str_to_num_ok,
    "allow implicit string to number conversion", },

  { "inf", "??", NULL,
    "infinity", },

  { "j", "??", NULL,
    "sqrt (-1)", },

#if defined (HAVE_ISNAN)
  { "nan", "??", NULL,
    "not a number", },
#endif

  { "ok_to_lose_imaginary_part", "warn", ok_to_lose_imaginary_part,
    "silently convert from complex to real by dropping imaginary part", },

  { "output_max_field_width", "??", set_output_max_field_width,
    "maximum width of an output field for numeric output", },

  { "output_precision", "??", set_output_precision,
    "number of significant figures to display for numeric output", },

  { "page_screen_output", "true", page_screen_output,
    "if possible, send output intended for the screen through the pager", },

  { "pi", "??", NULL,
    "ratio of the circumference of a circle to its diameter", },

  { "prefer_column_vectors", "true", prefer_column_vectors,
    "prefer column/row vectors", },

  { "prefer_zero_one_indexing", "false", prefer_zero_one_indexing,
    "when there is a conflict, prefer zero-one style indexing", },

  { "print_answer_id_name", "true", print_answer_id_name,
    "set output style to print `var_name = ...'", },

  { "print_empty_dimensions", "true", print_empty_dimensions,
    "also print dimensions of empty matrices", },

  { "propagate_empty_matrices", "true", propagate_empty_matrices,
    "operations on empty matrices return an empty matrix, not an error", },

  { "resize_on_range_error", "true", resize_on_range_error,
    "enlarge matrices on assignment", },

  { "return_last_computed_value", "false", return_last_computed_value,
    "if a function does not return any values explicitly, return the\n\
last computed value", },

  { "silent_functions", "false", silent_functions,
    "suppress printing results in called functions", },

  { "split_long_rows", "true", split_long_rows,
    "split long matrix rows instead of wrapping", },

  { "stdin", "??", NULL,
    "file number of the standard input stream", },

  { "stdout", "??", NULL,
    "file number of the standard output stream", },

  { "stderr", "??", NULL,
    "file number of the standard error stream", },

  { "treat_neg_dim_as_zero", "false", treat_neg_dim_as_zero,
    "convert negative dimensions to zero", },

  { "warn_assign_as_truth_value", "true", warn_assign_as_truth_value,
    "produce warning for assignments used as truth values", },

  { "warn_comma_in_global_decl", "true", warn_comma_in_global_decl,
    "produce warning for commas in global declarations", },

  { "warn_divide_by_zero", "true", warn_divide_by_zero,
    "on IEEE machines, allow divide by zero errors to be suppressed", },

  { NULL, NULL, NULL, NULL, },
};

static void
make_eternal (const char *s)
{
  symbol_record *sym_rec = curr_sym_tab->lookup (s, 0, 0);
  if (sym_rec != (symbol_record *) NULL)
    sym_rec->make_eternal ();
}

void
install_builtins (void)
{
  symbol_record *sym_rec;

  tree_builtin *tb_tmp;

// So that the clear function can't delete other builtin variables and
// functions, they are given eternal life.

  builtin_mapper_functions *mfptr = mapper_functions;
  while (mfptr->name != (char *) NULL)
    {
      sym_rec = curr_sym_tab->lookup (mfptr->name, 1);
      sym_rec->unprotect ();

      Mapper_fcn mfcn;
      mfcn.neg_arg_complex = mfptr->neg_arg_complex;
      mfcn.d_d_mapper = mfptr->d_d_mapper;
      mfcn.d_c_mapper = mfptr->d_c_mapper;
      mfcn.c_c_mapper = mfptr->c_c_mapper;

      tb_tmp = new tree_builtin (mfptr->nargin_max, mfptr->nargout_max,
				 mfcn, sym_rec);

      sym_rec->define (tb_tmp);
      sym_rec->document (mfptr->help_string);
      sym_rec->make_eternal ();
      sym_rec->protect ();
      mfptr++;
    }

  builtin_text_functions *tfptr = text_functions;
  while (tfptr->name != (char *) NULL)
    {
      sym_rec = curr_sym_tab->lookup (tfptr->name, 1);
      sym_rec->unprotect ();

      tb_tmp = new tree_builtin (tfptr->nargin_max, 1,
				 tfptr->text_fcn, sym_rec);

      sym_rec->define (tb_tmp);
      sym_rec->document (tfptr->help_string);
      sym_rec->make_eternal ();
      sym_rec->protect ();
      tfptr++;
    }

  builtin_general_functions *gfptr = general_functions;
  while (gfptr->name != (char *) NULL)
    {
      sym_rec = curr_sym_tab->lookup (gfptr->name, 1);
      sym_rec->unprotect ();

      tb_tmp = new tree_builtin (gfptr->nargin_max, gfptr->nargout_max,
				 gfptr->general_fcn, sym_rec);

      sym_rec->define (tb_tmp);
      sym_rec->document (gfptr->help_string);
      sym_rec->make_eternal ();
      sym_rec->protect ();
      gfptr++;
    }

// Most built-in variables are not protected because the user should
// be able to redefine them.

  builtin_string_variables *svptr = string_variables;
  while (svptr->name != (char *) NULL)
    {
      sym_rec = curr_sym_tab->lookup (svptr->name, 1);
      sym_rec->unprotect ();

      tree_constant *tmp = new tree_constant (svptr->value);

      sym_rec->set_sv_function (svptr->sv_function);
      sym_rec->define (tmp);
      sym_rec->document (svptr->help_string);
      sym_rec->make_eternal ();
      svptr++;
    }

// XXX FIXME XXX -- Need a convenient way to document these variables.

// IMPORTANT: Always create a new tree_constant for each variable.

  tree_constant *tmp = NULL_TREE_CONST;
  bind_variable ("ans", tmp);

  Complex ctmp (0.0, 1.0);
  tmp = new tree_constant (ctmp);
  bind_protected_variable ("I", tmp);
  make_eternal ("I");

  tmp = new tree_constant (ctmp);
  bind_protected_variable ("J", tmp);
  make_eternal ("J");

// Let i and j be functions so they can be redefined without being
// wiped out.

  char *tmp_help;

  tmp = new tree_constant (ctmp);
  sym_rec = curr_sym_tab->lookup ("i", 1);
  tmp_help = sym_rec->help ();
  sym_rec->define_as_fcn (tmp);
  sym_rec->document (tmp_help);
  sym_rec->protect ();
  sym_rec->make_eternal ();

  tmp = new tree_constant (ctmp);
  sym_rec = curr_sym_tab->lookup ("j", 1);
  tmp_help = sym_rec->help ();
  sym_rec->define_as_fcn (tmp);
  sym_rec->document (tmp_help);
  sym_rec->protect ();
  sym_rec->make_eternal ();

  tmp = new tree_constant (get_working_directory ("initialize_globals"));
  bind_protected_variable ("PWD", tmp);
  make_eternal ("PWD");

  tmp = new tree_constant (load_path);
  bind_variable ("LOADPATH", tmp);
  make_eternal ("LOADPATH");

  tmp = new tree_constant (info_file);
  bind_variable ("INFO_FILE", tmp);
  make_eternal ("INFO_FILE");

  tmp = new tree_constant (default_pager ());
  bind_variable ("PAGER", tmp);
  make_eternal ("PAGER");

  tmp = new tree_constant (0.0);
  bind_variable ("SEEK_SET", tmp);
  make_eternal ("SEEK_SET");

  tmp = new tree_constant (1.0);
  bind_variable ("SEEK_CUR", tmp);
  make_eternal ("SEEK_CUR");

  tmp = new tree_constant (2.0);
  bind_variable ("SEEK_END", tmp);
  make_eternal ("SEEK_END");

  tmp = new tree_constant (DBL_EPSILON);
  bind_protected_variable ("eps", tmp);
  make_eternal ("eps");

  tmp =  new tree_constant (10.0);
  bind_variable ("output_max_field_width", tmp);
  make_eternal ("output_max_field_width");

  tmp =  new tree_constant (5.0);
  bind_variable ("output_precision", tmp);
  make_eternal ("output_precision");

  tmp =  new tree_constant (4.0 * atan (1.0));
  bind_protected_variable ("pi", tmp);
  make_eternal ("pi");

  tmp =  new tree_constant (0.0);
  bind_protected_variable ("stdin", tmp);
  make_eternal ("stdin");

  tmp =  new tree_constant (1.0);
  bind_protected_variable ("stdout", tmp);
  make_eternal ("stdout");

  tmp =  new tree_constant (2.0);
  bind_protected_variable ("stderr", tmp);
  make_eternal ("stderr");

// If using 1.0 / 0.0 doesn't work, you might also try using a very
// large constant like 1.0e100000.

#if defined (HAVE_ISINF) || defined (HAVE_FINITE)
#ifdef linux
  double tmp_inf = HUGE_VAL;
#else
  double tmp_inf = 1.0 / 0.0;
#endif

  tmp = new tree_constant (tmp_inf);
  bind_protected_variable ("Inf", tmp);
  make_eternal ("Inf");

  tmp = new tree_constant (tmp_inf);
  bind_protected_variable ("inf", tmp);
  make_eternal ("inf");

#else

// This is sort of cheesy, but what can we do, other than blowing it
// off completely, or writing an entire IEEE emulation package?

  tmp = new tree_constant (DBL_MAX);
  bind_protected_variable ("Inf", tmp);
  make_eternal ("Inf");

  tmp = new tree_constant (DBL_MAX);
  bind_protected_variable ("inf", tmp);
  make_eternal ("inf");
#endif

// If tmp_inf / tmp_inf fails to produce a NaN, you might also try
// something like 0.0 / 0.0.

#if defined (HAVE_ISNAN)
#ifdef linux
  double tmp_nan = NAN;
#else
  double tmp_nan = tmp_inf / tmp_inf;
#endif

  tmp = new tree_constant (tmp_nan);
  bind_protected_variable ("NaN", tmp);
  make_eternal ("NaN");

  tmp = new tree_constant (tmp_nan);
  bind_protected_variable ("nan", tmp);
  make_eternal ("nan");
#endif
}

int
is_text_function_name (const char *s)
{
  int retval = 0;

  builtin_text_functions *tfptr = text_functions;
  while (tfptr->name != (char *) NULL)
    {
      if (strcmp (tfptr->name, s) == 0)
	{
	  retval = 1;
	  break;
	}
      tfptr++;
    }

  return retval;
}

help_list *
builtin_mapper_functions_help (void)
{
  int count = 0;
  builtin_mapper_functions *mfptr;

  mfptr = mapper_functions;
  while (mfptr->name != (char *) NULL)
    {
      count++;
      mfptr++;
    }

  if (count == 0)
    return (help_list *) NULL;

  help_list *hl = new help_list [count+1];

  int i = 0;
  mfptr = mapper_functions;
  while (mfptr->name != (char *) NULL)
    {
      hl[i].name = mfptr->name;
      hl[i].help = mfptr->help_string;
      i++;
      mfptr++;
    }

  hl[count].name = (char *) NULL;
  hl[count].help = (char *) NULL;

  return hl;
}

help_list *
builtin_general_functions_help (void)
{
  int count = 0;
  builtin_general_functions *gfptr;

  gfptr = general_functions;
  while (gfptr->name != (char *) NULL)
    {
      count++;
      gfptr++;
    }

  if (count == 0)
    return (help_list *) NULL;

  help_list *hl = new help_list [count+1];

  int i = 0;
  gfptr = general_functions;
  while (gfptr->name != (char *) NULL)
    {
      hl[i].name = gfptr->name;
      hl[i].help = gfptr->help_string;
      i++;
      gfptr++;
    }

  hl[count].name = (char *) NULL;
  hl[count].help = (char *) NULL;

  return hl;
}

help_list *
builtin_text_functions_help (void)
{
  int count = 0;
  builtin_text_functions *tfptr;

  tfptr = text_functions;
  while (tfptr->name != (char *) NULL)
    {
      count++;
      tfptr++;
    }

  if (count == 0)
    return (help_list *) NULL;

  help_list *hl = new help_list [count+1];

  int i = 0;
  tfptr = text_functions;
  while (tfptr->name != (char *) NULL)
    {
      hl[i].name = tfptr->name;
      hl[i].help = tfptr->help_string;
      i++;
      tfptr++;
    }

  hl[count].name = (char *) NULL;
  hl[count].help = (char *) NULL;

  return hl;
}

help_list *
builtin_variables_help (void)
{
  int count = 0;
  builtin_string_variables *svptr;

  svptr = string_variables;
  while (svptr->name != (char *) NULL)
    {
      count++;
      svptr++;
    }

  if (count == 0)
    return (help_list *) NULL;

  help_list *hl = new help_list [count+1];

  int i = 0;
  svptr = string_variables;
  while (svptr->name != (char *) NULL)
    {
      hl[i].name = svptr->name;
      hl[i].help = svptr->help_string;
      i++;
      svptr++;
    }

  hl[count].name = (char *) NULL;
  hl[count].help = (char *) NULL;

  return hl;
}

int
help_from_list (ostrstream& output_buf, const help_list *list,
		const char *string, int usage)
{
  char *name;
  while ((name = list->name) != (char *) NULL)
    {
      if (strcmp (name, string) == 0)
	{
	  if (usage)
	    output_buf << "\nusage: ";
	  else
	    {
	      output_buf << "\n*** " << string << ":\n\n";
	    }

	  output_buf << list->help << "\n";

	  return 1;
	}
      list++;
    }
  return 0;
}

void
additional_help_message (ostrstream& output_buf)
{
  output_buf
    << "\n"
    << "Additional help for builtin functions, operators, and variables\n"
    << "is available in the on-line version of the manual.\n"
    << "\n"
    << "Use the command `help -i <topic>' to search the manual index.\n";
}

void
print_usage (const char *string)
{
  ostrstream output_buf;

  help_list *gf_help_list = builtin_general_functions_help ();
  help_list *tf_help_list = builtin_text_functions_help ();
  help_list *mf_help_list = builtin_mapper_functions_help ();

  if (help_from_list (output_buf, gf_help_list, string, 1)
      || help_from_list (output_buf, tf_help_list, string, 1)
      || help_from_list (output_buf, mf_help_list, string, 1))
    {
      additional_help_message (output_buf);
      output_buf << ends;
      maybe_page_output (output_buf);
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
