//                                                       -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Array.h"

// These have to be defined somewhere, but only once.

#if defined (HEAVYWEIGHT_INDEXING)
#if defined (OCTAVE_SOURCE)
#include "user-prefs.h"
int& dfi_flag = user_pref.do_fortran_indexing;
int& pcv_flag = user_pref.prefer_column_vectors;
int& pzo_flag = user_pref.prefer_zero_one_indexing;
int& rre_flag = user_pref.resize_on_range_error;
#else
int dfi_flag = 0;
int pcv_flag = 0;
int pzo_flag = 0;
int rre_flag = 0;
#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
