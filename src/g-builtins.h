// Builtin general function support.                          -*- C++ -*-
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

#if !defined (_g_builtins_h)
#define _g_builtins_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include "tree-const.h"

struct builtin_general_functions
{
  char *name;
  int nargin_max;
  int nargout_max;
  General_fcn general_fcn;
  char *help_string;
};

extern tree_constant *builtin_all (tree_constant *, int, int);
extern tree_constant *builtin_any (tree_constant *, int, int);
extern tree_constant *builtin_clc (tree_constant *, int, int);
extern tree_constant *builtin_clock (tree_constant *, int, int);
extern tree_constant *builtin_closeplot (tree_constant *, int, int);
extern tree_constant *builtin_colloc (tree_constant *, int, int);
extern tree_constant *builtin_cumprod (tree_constant *, int, int);
extern tree_constant *builtin_cumsum (tree_constant *, int, int);
extern tree_constant *builtin_dassl (tree_constant *, int, int);
extern tree_constant *builtin_date (tree_constant *, int, int);
extern tree_constant *builtin_det (tree_constant *, int, int);
extern tree_constant *builtin_diag (tree_constant *, int, int);
extern tree_constant *builtin_disp (tree_constant *, int, int);
extern tree_constant *builtin_eig (tree_constant *, int, int);
extern tree_constant *builtin_error (tree_constant *, int, int);
extern tree_constant *builtin_eval (tree_constant *, int, int);
extern tree_constant *builtin_exist (tree_constant *, int, int);
extern tree_constant *builtin_expm (tree_constant *, int, int);
extern tree_constant *builtin_eye (tree_constant *, int, int);
extern tree_constant *builtin_fclose (tree_constant *, int, int);
extern tree_constant *builtin_feval (tree_constant *, int, int);
extern tree_constant *builtin_fflush (tree_constant *, int, int);
extern tree_constant *builtin_fft (tree_constant *, int, int);
extern tree_constant *builtin_fgets (tree_constant *, int, int);
extern tree_constant *builtin_find (tree_constant *, int, int);
extern tree_constant *builtin_flops (tree_constant *, int, int);
extern tree_constant *builtin_fopen (tree_constant *, int, int);
extern tree_constant *builtin_fprintf (tree_constant *, int, int);
extern tree_constant *builtin_frewind (tree_constant *, int, int);
extern tree_constant *builtin_freport (tree_constant *, int, int);
extern tree_constant *builtin_fscanf (tree_constant *, int, int);
extern tree_constant *builtin_fseek (tree_constant *, int, int);
extern tree_constant *builtin_fsolve (tree_constant *, int, int);
extern tree_constant *builtin_fsqp (tree_constant *, int, int);
extern tree_constant *builtin_ftell (tree_constant *, int, int);
extern tree_constant *builtin_getenv (tree_constant *, int, int);
extern tree_constant *builtin_hess (tree_constant *, int, int);
extern tree_constant *builtin_input (tree_constant *, int, int);
extern tree_constant *builtin_ifft (tree_constant *, int, int);
extern tree_constant *builtin_inv (tree_constant *, int, int);
extern tree_constant *builtin_isstr (tree_constant *, int, int);
extern tree_constant *builtin_keyboard (tree_constant *, int, int);
extern tree_constant *builtin_logm (tree_constant *, int, int);
extern tree_constant *builtin_lpsolve (tree_constant *, int, int);
extern tree_constant *builtin_lsode (tree_constant *, int, int);
extern tree_constant *builtin_lu (tree_constant *, int, int);
extern tree_constant *builtin_max (tree_constant *, int, int);
extern tree_constant *builtin_min (tree_constant *, int, int);
extern tree_constant *builtin_npsol (tree_constant *, int, int);
extern tree_constant *builtin_ones (tree_constant *, int, int);
extern tree_constant *builtin_pause (tree_constant *, int, int);
extern tree_constant *builtin_purge_tmp_files (tree_constant *, int, int);
extern tree_constant *builtin_printf (tree_constant *, int, int);
extern tree_constant *builtin_prod (tree_constant *, int, int);
extern tree_constant *builtin_pwd (tree_constant *, int, int);
extern tree_constant *builtin_qpsol (tree_constant *, int, int);
extern tree_constant *builtin_qr (tree_constant *, int, int);
extern tree_constant *builtin_quad (tree_constant *, int, int);
extern tree_constant *builtin_quit (tree_constant *, int, int);
extern tree_constant *builtin_rand (tree_constant *, int, int);
extern tree_constant *builtin_replot (tree_constant *, int, int);
extern tree_constant *builtin_setstr (tree_constant *, int, int);
extern tree_constant *builtin_scanf (tree_constant *, int, int);
extern tree_constant *builtin_schur (tree_constant *, int, int);
extern tree_constant *builtin_shell_command (tree_constant *, int, int);
extern tree_constant *builtin_size (tree_constant *, int, int);
extern tree_constant *builtin_sort (tree_constant *, int, int);
extern tree_constant *builtin_sprintf (tree_constant *, int, int);
extern tree_constant *builtin_sqrtm (tree_constant *, int, int);
extern tree_constant *builtin_sscanf (tree_constant *, int, int);
extern tree_constant *builtin_sum (tree_constant *, int, int);
extern tree_constant *builtin_sumsq (tree_constant *, int, int);
extern tree_constant *builtin_svd (tree_constant *, int, int);
extern tree_constant *builtin_warranty (tree_constant *, int, int);
extern tree_constant *builtin_zeros (tree_constant *, int, int);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
