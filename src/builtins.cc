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
    "compute abs(X) for each element of X\n", },

  { "acos", 2, 1, 0, acos, NULL, acos,
    "compute acos(X) for each element of X\n", },

  { "acosh", 2, 1, 0, acosh, NULL, acosh,
    "compute acosh(X) for each element of X\n", },

  { "angle", 2, 1, 0, arg, arg, NULL,
    "compute arg(X) for each element of X\n", },

  { "arg", 2, 1, 0, arg, arg, NULL,
    "compute arg(X) for each element of X\n", },

  { "asin", 2, 1, 0, asin, NULL, asin,
    "compute asin(X) for each element of X\n", },

  { "asinh", 2, 1, 0, asinh, NULL, asinh,
    "compute asinh(X) for each element of X\n", },

  { "atan", 2, 1, 0, atan, NULL, atan,
    "compute atan(X) for each element of X\n", },

  { "atanh", 2, 1, 0, atanh, NULL, atanh,
    "compute atanh(X) for each element of X\n", },

  { "ceil", 2, 1, 0, ceil, NULL, ceil,
    "ceil(X): round elements of X toward +Inf\n", },

  { "conj", 2, 1, 0, conj, NULL, conj,
    "compute complex conjugate for each element of X\n", },

  { "cos", 2, 1, 0, cos, NULL, cos,
    "compute cos(X) for each element of X\n", },

  { "cosh", 2, 1, 0, cosh, NULL, cosh,
    "compute cosh(X) for each element of X\n", },

  { "exp", 2, 1, 0, exp, NULL, exp,
    "compute exp(X) for each element of X\n", },

  { "finite", 2, 1, 0, xfinite, xfinite, NULL,
    "finite(X): return 1 for finite elements of X\n", },

  { "fix", 2, 1, 0, fix, NULL, fix,
    "fix(X): round elements of X toward zero\n", },

  { "floor", 2, 1, 0, floor, NULL, floor,
    "floor(X): round elements of X toward -Inf\n", },

  { "isinf", 2, 1, 0, xisinf, xisinf, NULL,
    "isinf(X): return 1 for elements of X infinite\n", },

  { "imag", 2, 1, 0, imag, imag, NULL,
    "return imaginary part for each elements of X\n", },

#ifdef HAVE_ISNAN
  { "isnan", 2, 1, 0, xisnan, xisnan, NULL,
    "isnan(X): return 1 where elements of X are NaNs\n", },
#endif

  { "log", 2, 1, 1, log, NULL, log,
    "compute log(X) for each element of X\n", },

  { "log10", 2, 1, 1, log10, NULL, log10,
    "compute log10(X) for each element of X\n", },

  { "real", 2, 1, 0, real, real, NULL,
    "return real part for each element of X\n", },

  { "round", 2, 1, 0, round, NULL, round,
    "round(X): round elements of X to nearest integer\n", },

  { "sign", 2, 1, 0, signum, NULL, signum,
    "sign(X): apply signum function to elements of X\n", },

  { "sin", 2, 1, 0, sin, NULL, sin,
    "compute sin(X) for each element of X\n", },

  { "sinh", 2, 1, 0, sinh, NULL, sinh,
    "compute sinh(X) for each element of X\n", },

  { "sqrt", 2, 1, 1, sqrt, NULL, sqrt,
    "compute sqrt(X) for each element of X\n", },

  { "tan", 2, 1, 0, tan, NULL, tan,
    "compute tan(X) for each element of X\n", },

  { "tanh", 2, 1, 0, tanh, NULL, tanh,
    "compute tanh(X) for each element of X\n", },

  { NULL, -1, -1, -1, NULL, NULL, NULL, NULL, },
};

static builtin_text_functions text_functions[] =
{
  { "casesen", 2, builtin_casesen,
    "print warning if user tries to change case sensitivity\n", },

  { "cd", 2, builtin_cd,
    "change current working directory\n", },

  { "clear", -1, builtin_clear,
    "clear symbol(s) from symbol table\n", },

  { "dir", -1, builtin_ls,
    "print a directory listing\n", },

  { "document", -1, builtin_document,
    "define help string for symbol\n", },

  { "edit_history", -1, builtin_edit_history,
    "usage: edit_history [first] [last]\n", },

  { "format", -1, builtin_format,
    "set output formatting style\n", },

  { "help", -1, builtin_help,
    "print cryptic yet witty messages\n", },

  { "history", -1, builtin_history,
    "print/save/load command history\n", },

  { "load", -1, builtin_load,
    "load variables from a file\n", },

  { "ls", -1, builtin_ls,
    "print a directory listing\n", },

  { "save", -1, builtin_save,
    "save variables to a file\n", },

  { "set", -1, builtin_set,
    "set plotting options\n", },

  { "show", -1, builtin_show,
    "show plotting options\n", },

  { "who", -1, builtin_who,
    "list symbols\n", },

  { NULL, -1, NULL, NULL, },
};

static builtin_general_functions general_functions[] =
{
  { "all", 2, 1, builtin_all,
    "all(X): are all elements of X nonzero?\n", },

  { "any", 2, 1, builtin_any,
    "any(X): are any elements of X nonzero?\n", },

  { "clc", 1, 0, builtin_clc,
    "clear screen\n", },

  { "clock", 1, 0, builtin_clock,
    "return current date and time in vector\n", },

  { "closeplot", 1, 0, builtin_closeplot,
    "close the stream to plotter\n", },

  { "colloc", 7, 4, builtin_colloc,
    "[r, A, B, q] = colloc (n [, 'left'] [, 'right']): collocation weights\n", },

  { "cumprod", 2, 1, builtin_cumprod,
    "cumprod (X): cumulative products\n", },

  { "cumsum", 2, 1, builtin_cumsum,
    "cumsum (X): cumulative sums\n", },

  { "dassl", 5, 1, builtin_dassl,
    "solve DAEs using Petzold's DASSL.  Usage:\n\
\n\
  dassl ('function_name', x_0, xdot_0, t_out)\n\
  dassl ('function_name', x_0, xdot_0, t_out, t_crit)\n\
\n\
The first argument is the name of the function to call to\n\
compute the vector of residuals.  It must have the form\n\
\n\
  res = f (x, xdot, t)\n\
\n\
where x, xdot, and res are vectors, and t is a scalar.\n\n", },

  { "date", 1, 0, builtin_date,
    "return current date in a string\n", },

  { "det", 2, 1, builtin_det,
    "det(X): determinant of a square matrix\n", },

  { "diag", 3, 1, builtin_diag,
    "diag(X [,k]): form/extract diagonals\n", },

  { "disp", 3, 1, builtin_disp,
    "disp(X): display value\n", },

  { "eig", 2, 1, builtin_eig,
    "eig (x): compute eigenvalues and eigenvectors of x\n", },

  { "error", 2, 1, builtin_error,
    "print message and jump to top level\n", },

  { "eval", 2, 1, builtin_eval,
    "evaluate text as octave source\n", },

  { "exist", 2, 1, builtin_exist,
    "check if variable or file exists\n", },

  { "exit", 1, 0, builtin_quit,
    "exit Octave gracefully\n", },

  { "expm", 2, 1, builtin_expm,
    "Matrix exponential, e^A\n", },

  { "eye", 3, 1, builtin_eye,
    "create an identity matrix\n", },

  { "fclose", 2, 1, builtin_fclose,
    "fclose('filename' or filenum): close a file\n", },

  { "feval", -1, 1, builtin_feval,
    "evaluate argument as function\n", },

  { "fflush", 2, 1, builtin_fflush,
    "fflush('filename' or filenum): flush buffered data to output file\n", },

  { "fft", 2, 1, builtin_fft,
    "fft(X): fast fourier transform of a vector\n", },

  { "fgets",3, 2, builtin_fgets,
    "[string, length] = fgets('filename' or filenum, length): read a string from a file\n", },

  { "find", -1, 1, builtin_find,
    "find (x): return vector of indices of nonzero elements\n", },

  { "flops", 2, 1, builtin_flops,
    "count floating point operations\n", },

  { "fopen", 3, 1, builtin_fopen,
    "filenum = fopen('filename','mode'): open a file\n", },

  { "fprintf", -1, 1, builtin_fprintf,
    "fprintf ('file', 'fmt', ...)\n", },

  { "freport", 1, 1, builtin_freport,
    "freport: list open files and their status\n", },

  { "frewind", 2, 1, builtin_frewind,
    "frewind('filename' or filenum): set file position at beginning of file\n", },

  { "fscanf", 3, -1, builtin_fscanf,
    "[a,b,c...] = fscanf ('file', 'fmt')\n", },

  { "fseek", 4, 1, builtin_fseek,
    "fseek('filename' or filenum): set file position for reading or writing\n", },

  { "fsolve", 5, 1, builtin_fsolve,
    "Solve nonlinear equations using Minpack.  Usage:\n\
\n\
  [x, info] = fsolve ('f', x0)\n\
\n\
Where the first argument is the name of the  function to call to\n\
compute the vector of function values.  It must have the form\n\
\n\
  y = f (x)
\n\
where y and x are vectors.\n\n", },

  { "fsqp", 11, 3, builtin_fsqp,
    "solve NLPs\n", },

  { "ftell", 2, 1, builtin_ftell,
    "position = ftell ('filename' or filenum): returns the current file position\n", },

  { "getenv", 2, 1, builtin_getenv,
    "get environment variable values\n", },

  { "hess", 2, 2, builtin_hess,
    "Hessenburg decomposition\n",},

  { "home", 1, 0, builtin_clc,
    "clear screen\n", },

  { "input", 3, 1, builtin_input,
    "input('prompt' [,'s']): prompt user for [string] input\n", },

  { "ifft", 2, 1, builtin_ifft,
    "ifft(X): inverse fast fourier transform of a vector\n", },

  { "inv", 2, 1, builtin_inv,
    "inv(X): inverse of a square matrix\n", },

  { "inverse", 2, 1, builtin_inv,
    "inverse(X): inverse of a square matrix\n", },

  { "isstr", 2, 1, builtin_isstr,
    "isstr(X): return 1 if X is a string\n", },

  { "keyboard", 2, 1, builtin_keyboard,
    "maybe help in debugging M-files\n", },

  { "logm", 2, 1, builtin_logm,
    "Matrix logarithm, log (A)\n", },

  { "lpsolve", 11, 3, builtin_lpsolve,
    "Solve linear programs using lp_solve.\n", },

  { "lsode", 6, 1, builtin_lsode,
    "solve ODEs using Hindmarsh's LSODE.  Usage:\n\
\n\
  lsode ('function_name', x0, t_out\n\
  lsode ('function_name', x0, t_out, t_crit)\n\
\n\
The first argument is the name of the function to call to\n\
compute the vector of right hand sides.  It must have the form\n\
\n\
  xdot = f (x, t)\n\
\n\
where xdot and x are vectors and t is a scalar.\n\n", },

  { "lu", 2, 3, builtin_lu,
    "[L, U, P] = lu (A) -- LU factorization\n", },

  { "max", 3, 2, builtin_max,
    "maximum value(s) of a vector (matrix)\n", },

  { "min", 3, 2, builtin_min,
    "minimum value(s) of a vector (matrix)\n", },

  { "npsol", 11, 3, builtin_npsol,
    "Solve nonlinear programs using Gill and Murray's NPSOL.  Usage:\n\
\n\
  [x, obj, info, lambda] = npsol (x, 'phi' [, lb, ub] [, lb, A, ub] [, lb, 'g', ub])\n\n\
Groups of arguments surrounded in `[]' are optional, but\n\
must appear in the same relative order shown above.\n\
\n\
The second argument is a string containing the name of the objective\n\
function to call.  The objective function must be of the form\n\
\n\
  y = phi (x)\n\
\n\
where x is a vector and y is a scalar.\n\n", },

  { "ones", 3, 1, builtin_ones,
    "create a matrix of all ones\n", },

  { "pause", 1, 0, builtin_pause,
    "suspend program execution\n", },

  { "purge_tmp_files", 5, 1, builtin_purge_tmp_files,
    "delete temporary data files used for plotting\n", },

  { "printf", -1, 1, builtin_printf,
    "printf ('fmt', ...)\n", },

  { "prod", 2, 1, builtin_prod,
    "prod (X): products\n", },

  { "pwd", 1, 0, builtin_pwd,
    "print current working directory\n", },

  { "qpsol", 9, 3, builtin_qpsol,
    "Solve nonlinear programs using Gill and Murray's QPSOL.  Usage:\n\
\n\
  [x, obj, info, lambda] = qpsol (x, H, c [, lb, ub] [, lb, A, ub])\n\
\n\
  Groups of arguments surrounded in `[]' are optional, but\n\
  must appear in the same relative order shown above.", },

  { "qr", 2, 2, builtin_qr,
    "[q, r] = qr (X): form QR factorization of X\n", },

  { "quad", 6, 3, builtin_quad,
    "Integrate a nonlinear function of one variable using Quadpack.\n\
Usage:\n\
\n\
  [v, ier, nfun] = quad ('f', a, b [, tol] [, sing])\n\
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
at which the integrand is singular.\n\n", },

  { "quit", 1, 0, builtin_quit,
    "exit Octave gracefully\n", },

  { "rand", 2, 1, builtin_rand,
    "matrices with random elements\n", },

  { "replot", 1, 0, builtin_replot,
    "redisplay current plot\n", },

  { "scanf", 2, -1, builtin_scanf,
    "[a,b,c...] = scanf ('fmt')\n", },

  { "setstr", 2, 1, builtin_setstr,
    "setstr (v): convert a vector to a string\n", },

  { "shell_cmd", 2, 1, builtin_shell_command,
    "shell_cmd (string [, return_output]): execute shell commands\n", },

  { "schur", 3, 2, builtin_schur,
    "returns the Schur (straight or ordered) decomposition of matrix\n", },

  { "size", 2, 1, builtin_size,
    "size(X): return rows and columns of X\n", },

  { "sort", 2, 2, builtin_sort,
    "[s,i] = sort(x): sort the columns of x, optionally return sort index\n", },

  { "sqrtm", 2, 1, builtin_sqrtm,
    "Matrix sqrt, sqrt (A)\n", },

  { "sprintf", -1, 1, builtin_sprintf,
    "s = sprintf ('fmt', ...)\n", },

  { "sscanf", 3, -1, builtin_sscanf,
    "[a,b,c...] = sscanf (string, 'fmt')\n", },

  { "sum", 2, 1, builtin_sum,
    "sum (X): sum of elements\n", },

  { "sumsq", 2, 1, builtin_sumsq,
    "sumsq (X): sum of squares of elements\n", },

  { "svd", 2, 3, builtin_svd,
    "[U,S,V] = svd(X): return SVD of X\n", },

  { "warranty", 1, 0, builtin_warranty,
    "describe copying conditions\n", },

  { "zeros", 3, 1, builtin_zeros,
    "create a matrix of all zeros\n", },

  { NULL, -1, -1, NULL, NULL, },
};

// This is a lie.  Some of these get reassigned to be numeric
// variables.  See below.

static builtin_string_variables string_variables[] =
{
  { "I", "??", NULL,
    "sqrt (-1)\n", },

  { "Inf", "??", NULL,
    "infinity\n", },

  { "J", "??", NULL,
    "sqrt (-1)\n", },

#if defined (HAVE_ISNAN)
  { "NaN", "??", NULL,
    "not a number\n", },
#endif

  { "LOADPATH", "??", sv_loadpath,
    "colon separated list of directories to search for scripts\n", },

  { "PAGER", "??", sv_pager_binary,
    "path to pager binary\n", },

  { "PS1", "\\s:\\#> ", sv_ps1,
    "primary prompt string\n", },

  { "PS2", "> ", sv_ps2,
    "secondary prompt string\n", },

  { "PWD", "??PWD??", sv_pwd,
    "current working directory\n", },

  { "SEEK_SET", "??", NULL,
    "used with fseek to position file relative to the beginning\n", },

  { "SEEK_CUR", "??", NULL,
    "used with fseek to position file relative to the current position\n", },

  { "SEEK_END", "??", NULL,
    "used with fseek to position file relative to the end\n", },

  { "do_fortran_indexing", "false", do_fortran_indexing,
    "allow single indices for matrices\n", },

  { "empty_list_elements_ok", "warn", empty_list_elements_ok,
    "ignore the empty element in expressions like `a = [[], 1]'\n", },

  { "eps", "??", NULL,
      "machine precision\n", },

  { "gnuplot_binary", "gnuplot", sv_gnuplot_binary,
    "path to gnuplot binary\n", },

  { "i", "??", NULL,
    "sqrt (-1)\n", },

  { "implicit_str_to_num_ok", "false", implicit_str_to_num_ok,
    "allow implicit string to number conversion\n", },

  { "inf", "??", NULL,
    "infinity\n", },

  { "j", "??", NULL,
    "sqrt (-1)\n", },

#if defined (HAVE_ISNAN)
  { "nan", "??", NULL,
    "not a number\n", },
#endif

  { "ok_to_lose_imaginary_part", "warn", ok_to_lose_imaginary_part,
    "silently convert from complex to real by dropping imaginary part\n", },

  { "output_max_field_width", "??", set_output_max_field_width,
    "maximum width of an output field for numeric output\n", },

  { "output_precision", "??", set_output_precision,
    "number of significant figures to display for numeric output\n", },

  { "page_screen_output", "true", page_screen_output,
    "if possible, send output intended for the screen through the pager\n", },

  { "pi", "??", NULL,
    "ratio of the circumference of a circle to its diameter\n", },

  { "prefer_column_vectors", "true", prefer_column_vectors,
    "prefer column/row vectors\n", },

  { "prefer_zero_one_indexing", "false", prefer_zero_one_indexing,
    "when there is a conflict, prefer zero-one style indexing\n", },

  { "print_answer_id_name", "true", print_answer_id_name,
    "set output style to print `var_name = ...'\n", },

  { "print_empty_dimensions", "true", print_empty_dimensions,
    "also print dimensions of empty matrices\n", },

  { "propagate_empty_matrices", "true", propagate_empty_matrices,
    "operations on empty matrices return an empty matrix, not an error\n", },

  { "resize_on_range_error", "true", resize_on_range_error,
    "enlarge matrices on assignment\n", },

  { "return_last_computed_value", "false", return_last_computed_value,
    "if a function does not return any values explicitly, return the\n\
last computed value\n", },

  { "silent_functions", "false", silent_functions,
    "suppress printing results in called functions\n", },

  { "split_long_rows", "true", split_long_rows,
    "split long matrix rows instead of wrapping\n", },

  { "stdin", "??", NULL,
    "file number of the standard input stream\n", },

  { "stdout", "??", NULL,
    "file number of the standard output stream\n", },

  { "stderr", "??", NULL,
    "file number of the standard error stream\n", },

  { "treat_neg_dim_as_zero", "false", treat_neg_dim_as_zero,
    "convert negative dimensions to zero\n", },

  { "warn_assign_as_truth_value", "true", warn_assign_as_truth_value,
    "produce warning for assignments used as truth values\n", },

  { "warn_comma_in_global_decl", "true", warn_comma_in_global_decl,
    "produce warning for commas in global declarations\n", },

  { "warn_divide_by_zero", "true", warn_divide_by_zero,
    "on IEEE machines, allow divide by zero errors to be suppressed\n", },

  { NULL, NULL, NULL, NULL, },
};

void
make_eternal (char *s)
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

#if defined (HAVE_ISINF) || defined (HAVE_FINITE)
#ifdef linux
  tmp = new tree_constant (HUGE_VAL);
#else
  tmp = new tree_constant (1.0/0.0);
#endif
  bind_protected_variable ("Inf", tmp);
  make_eternal ("Inf");

#ifdef linux
  tmp = new tree_constant (HUGE_VAL);
#else
  tmp = new tree_constant (1.0/0.0);
#endif
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

#if defined (HAVE_ISNAN)
#ifdef linux
  tmp = new tree_constant (NAN);
#else
  tmp = new tree_constant (0.0/0.0);
#endif
  bind_protected_variable ("NaN", tmp);
  make_eternal ("NaN");

#ifdef linux
  tmp = new tree_constant (NAN);
#else
  tmp = new tree_constant (0.0/0.0);
#endif
  bind_protected_variable ("nan", tmp);
  make_eternal ("nan");
#endif
}

int
is_text_function_name (char *s)
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
