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

extern Octave_object builtin_all (const Octave_object&, int, int);
extern Octave_object builtin_any (const Octave_object&, int, int);
extern Octave_object builtin_balance (const Octave_object&, int, int);
extern Octave_object builtin_chol (const Octave_object&, int, int);
extern Octave_object builtin_clc (const Octave_object&, int, int);
extern Octave_object builtin_clock (const Octave_object&, int, int);
extern Octave_object builtin_closeplot (const Octave_object&, int, int);
extern Octave_object builtin_colloc (const Octave_object&, int, int);
extern Octave_object builtin_cumprod (const Octave_object&, int, int);
extern Octave_object builtin_cumsum (const Octave_object&, int, int);
extern Octave_object builtin_dassl (const Octave_object&, int, int);
extern Octave_object builtin_dassl_options (const Octave_object&, int, int);
extern Octave_object builtin_date (const Octave_object&, int, int);
extern Octave_object builtin_det (const Octave_object&, int, int);
extern Octave_object builtin_diag (const Octave_object&, int, int);
extern Octave_object builtin_disp (const Octave_object&, int, int);
extern Octave_object builtin_eig (const Octave_object&, int, int);
extern Octave_object builtin_error (const Octave_object&, int, int);
extern Octave_object builtin_eval (const Octave_object&, int, int);
extern Octave_object builtin_exist (const Octave_object&, int, int);
extern Octave_object builtin_expm (const Octave_object&, int, int);
extern Octave_object builtin_eye (const Octave_object&, int, int);
extern Octave_object builtin_fclose (const Octave_object&, int, int);
extern Octave_object builtin_feof (const Octave_object&, int, int);
extern Octave_object builtin_ferror (const Octave_object&, int, int);
extern Octave_object builtin_feval (const Octave_object&, int, int);
extern Octave_object builtin_fflush (const Octave_object&, int, int);
extern Octave_object builtin_fft (const Octave_object&, int, int);
extern Octave_object builtin_fgets (const Octave_object&, int, int);
extern Octave_object builtin_find (const Octave_object&, int, int);
extern Octave_object builtin_flops (const Octave_object&, int, int);
extern Octave_object builtin_fopen (const Octave_object&, int, int);
extern Octave_object builtin_fprintf (const Octave_object&, int, int);
extern Octave_object builtin_fread (const Octave_object&, int, int);
extern Octave_object builtin_frewind (const Octave_object&, int, int);
extern Octave_object builtin_freport (const Octave_object&, int, int);
extern Octave_object builtin_fscanf (const Octave_object&, int, int);
extern Octave_object builtin_fseek (const Octave_object&, int, int);
extern Octave_object builtin_fsolve (const Octave_object&, int, int);
extern Octave_object builtin_fsolve_options (const Octave_object&, int, int);
extern Octave_object builtin_fsqp (const Octave_object&, int, int);
extern Octave_object builtin_fsqp_options (const Octave_object&, int, int);
extern Octave_object builtin_ftell (const Octave_object&, int, int);
extern Octave_object builtin_fwrite (const Octave_object&, int, int);
extern Octave_object builtin_getenv (const Octave_object&, int, int);
extern Octave_object builtin_givens (const Octave_object&, int, int);
extern Octave_object builtin_hess (const Octave_object&, int, int);
extern Octave_object builtin_input (const Octave_object&, int, int);
extern Octave_object builtin_ifft (const Octave_object&, int, int);
extern Octave_object builtin_inv (const Octave_object&, int, int);
extern Octave_object builtin_is_global (const Octave_object&, int, int);
extern Octave_object builtin_isstr (const Octave_object&, int, int);
extern Octave_object builtin_kbhit (const Octave_object&, int, int);
extern Octave_object builtin_keyboard (const Octave_object&, int, int);
extern Octave_object builtin_logm (const Octave_object&, int, int);
extern Octave_object builtin_lpsolve (const Octave_object&, int, int);
extern Octave_object builtin_lpsolve_options (const Octave_object&, int, int);
extern Octave_object builtin_lsode (const Octave_object&, int, int);
extern Octave_object builtin_lsode_options (const Octave_object&, int, int);
extern Octave_object builtin_lu (const Octave_object&, int, int);
extern Octave_object builtin_max (const Octave_object&, int, int);
extern Octave_object builtin_min (const Octave_object&, int, int);
extern Octave_object builtin_npsol (const Octave_object&, int, int);
extern Octave_object builtin_npsol_options (const Octave_object&, int, int);
extern Octave_object builtin_ones (const Octave_object&, int, int);
extern Octave_object builtin_pause (const Octave_object&, int, int);
extern Octave_object builtin_purge_tmp_files (const Octave_object&, int, int);
extern Octave_object builtin_printf (const Octave_object&, int, int);
extern Octave_object builtin_prod (const Octave_object&, int, int);
extern Octave_object builtin_pwd (const Octave_object&, int, int);
extern Octave_object builtin_qpsol (const Octave_object&, int, int);
extern Octave_object builtin_qpsol_options (const Octave_object&, int, int);
extern Octave_object builtin_qr (const Octave_object&, int, int);
extern Octave_object builtin_quad (const Octave_object&, int, int);
extern Octave_object builtin_quad_options (const Octave_object&, int, int);
extern Octave_object builtin_quit (const Octave_object&, int, int);
extern Octave_object builtin_qzval (const Octave_object&, int, int);
extern Octave_object builtin_rand (const Octave_object&, int, int);
extern Octave_object builtin_setstr (const Octave_object&, int, int);
extern Octave_object builtin_scanf (const Octave_object&, int, int);
extern Octave_object builtin_schur (const Octave_object&, int, int);
extern Octave_object builtin_shell_command (const Octave_object&, int, int);
extern Octave_object builtin_size (const Octave_object&, int, int);
extern Octave_object builtin_sort (const Octave_object&, int, int);
extern Octave_object builtin_sprintf (const Octave_object&, int, int);
extern Octave_object builtin_sqrtm (const Octave_object&, int, int);
extern Octave_object builtin_sscanf (const Octave_object&, int, int);
extern Octave_object builtin_sum (const Octave_object&, int, int);
extern Octave_object builtin_sumsq (const Octave_object&, int, int);
extern Octave_object builtin_svd (const Octave_object&, int, int);
extern Octave_object builtin_syl (const Octave_object&, int, int);
extern Octave_object builtin_va_arg (const Octave_object&, int, int);
extern Octave_object builtin_va_start (const Octave_object&, int, int);
extern Octave_object builtin_warranty (const Octave_object&, int, int);
extern Octave_object builtin_zeros (const Octave_object&, int, int);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
