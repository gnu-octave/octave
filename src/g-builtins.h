// Builtin general function support.                          -*- C++ -*-
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

#if !defined (octave_g_builtins_h)
#define octave_g_builtins_h 1

#include "tree-const.h"

struct builtin_general_functions
{
  char *name;
  int nargin_max;
  int nargout_max;
  General_fcn general_fcn;
  char *help_string;
};

extern tree_constant *builtin_all (const tree_constant *, int, int);
extern tree_constant *builtin_any (const tree_constant *, int, int);
extern tree_constant *builtin_balance (const tree_constant *, int, int);
extern tree_constant *builtin_chol (const tree_constant *, int, int);
extern tree_constant *builtin_clc (const tree_constant *, int, int);
extern tree_constant *builtin_clock (const tree_constant *, int, int);
extern tree_constant *builtin_closeplot (const tree_constant *, int, int);
extern tree_constant *builtin_colloc (const tree_constant *, int, int);
extern tree_constant *builtin_cumprod (const tree_constant *, int, int);
extern tree_constant *builtin_cumsum (const tree_constant *, int, int);
extern tree_constant *builtin_dassl (const tree_constant *, int, int);
extern tree_constant *builtin_dassl_options (const tree_constant *, int, int);
extern tree_constant *builtin_date (const tree_constant *, int, int);
extern tree_constant *builtin_det (const tree_constant *, int, int);
extern tree_constant *builtin_diag (const tree_constant *, int, int);
extern tree_constant *builtin_disp (tree_constant *, int, int);
extern tree_constant *builtin_eig (const tree_constant *, int, int);
extern tree_constant *builtin_error (tree_constant *, int, int);
extern tree_constant *builtin_eval (const tree_constant *, int, int);
extern tree_constant *builtin_exist (const tree_constant *, int, int);
extern tree_constant *builtin_expm (const tree_constant *, int, int);
extern tree_constant *builtin_eye (const tree_constant *, int, int);
extern tree_constant *builtin_fclose (const tree_constant *, int, int);
extern tree_constant *builtin_feval (const tree_constant *, int, int);
extern tree_constant *builtin_fflush (const tree_constant *, int, int);
extern tree_constant *builtin_fft (const tree_constant *, int, int);
extern tree_constant *builtin_fgets (const tree_constant *, int, int);
extern tree_constant *builtin_find (const tree_constant *, int, int);
extern tree_constant *builtin_flops (const tree_constant *, int, int);
extern tree_constant *builtin_fopen (const tree_constant *, int, int);
extern tree_constant *builtin_fprintf (const tree_constant *, int, int);
extern tree_constant *builtin_frewind (const tree_constant *, int, int);
extern tree_constant *builtin_freport (const tree_constant *, int, int);
extern tree_constant *builtin_fscanf (const tree_constant *, int, int);
extern tree_constant *builtin_fseek (const tree_constant *, int, int);
extern tree_constant *builtin_fsolve (const tree_constant *, int, int);
extern tree_constant *builtin_fsolve_options (const tree_constant *, int, int);
extern tree_constant *builtin_fsqp (const tree_constant *, int, int);
extern tree_constant *builtin_fsqp_options (const tree_constant *, int, int);
extern tree_constant *builtin_ftell (const tree_constant *, int, int);
extern tree_constant *builtin_getenv (const tree_constant *, int, int);
extern tree_constant *builtin_givens (const tree_constant *, int, int);
extern tree_constant *builtin_hess (const tree_constant *, int, int);
extern tree_constant *builtin_input (const tree_constant *, int, int);
extern tree_constant *builtin_ifft (const tree_constant *, int, int);
extern tree_constant *builtin_inv (const tree_constant *, int, int);
extern tree_constant *builtin_is_global (const tree_constant *, int, int);
extern tree_constant *builtin_isstr (const tree_constant *, int, int);
extern tree_constant *builtin_keyboard (const tree_constant *, int, int);
extern tree_constant *builtin_logm (const tree_constant *, int, int);
extern tree_constant *builtin_lpsolve (const tree_constant *, int, int);
extern tree_constant *builtin_lpsolve_options (const tree_constant *, int, int);
extern tree_constant *builtin_lsode (const tree_constant *, int, int);
extern tree_constant *builtin_lsode_options (const tree_constant *, int, int);
extern tree_constant *builtin_lu (const tree_constant *, int, int);
extern tree_constant *builtin_max (const tree_constant *, int, int);
extern tree_constant *builtin_min (const tree_constant *, int, int);
extern tree_constant *builtin_npsol (const tree_constant *, int, int);
extern tree_constant *builtin_npsol_options (const tree_constant *, int, int);
extern tree_constant *builtin_ones (const tree_constant *, int, int);
extern tree_constant *builtin_pause (const tree_constant *, int, int);
extern tree_constant *builtin_purge_tmp_files (const tree_constant *,
					       int, int);
extern tree_constant *builtin_printf (const tree_constant *, int, int);
extern tree_constant *builtin_prod (const tree_constant *, int, int);
extern tree_constant *builtin_pwd (const tree_constant *, int, int);
extern tree_constant *builtin_qpsol (const tree_constant *, int, int);
extern tree_constant *builtin_qpsol_options (const tree_constant *, int, int);
extern tree_constant *builtin_qr (const tree_constant *, int, int);
extern tree_constant *builtin_quad (const tree_constant *, int, int);
extern tree_constant *builtin_quad_options (const tree_constant *, int, int);
extern tree_constant *builtin_quit (const tree_constant *, int, int);
extern tree_constant *builtin_qzval (const tree_constant *, int, int);
extern tree_constant *builtin_rand (const tree_constant *, int, int);
extern tree_constant *builtin_replot (const tree_constant *, int, int);
extern tree_constant *builtin_setstr (tree_constant *, int, int);
extern tree_constant *builtin_scanf (const tree_constant *, int, int);
extern tree_constant *builtin_schur (const tree_constant *, int, int);
extern tree_constant *builtin_shell_command (const tree_constant *, int, int);
extern tree_constant *builtin_size (const tree_constant *, int, int);
extern tree_constant *builtin_sort (const tree_constant *, int, int);
extern tree_constant *builtin_sprintf (const tree_constant *, int, int);
extern tree_constant *builtin_sqrtm (const tree_constant *, int, int);
extern tree_constant *builtin_sscanf (const tree_constant *, int, int);
extern tree_constant *builtin_sum (const tree_constant *, int, int);
extern tree_constant *builtin_sumsq (const tree_constant *, int, int);
extern tree_constant *builtin_svd (const tree_constant *, int, int);
extern tree_constant *builtin_syl (const tree_constant *, int, int);
extern tree_constant *builtin_va_arg (const tree_constant *, int, int);
extern tree_constant *builtin_va_start (const tree_constant *, int, int);
extern tree_constant *builtin_warranty (const tree_constant *, int, int);
extern tree_constant *builtin_zeros (const tree_constant *, int, int);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
