// octave-hist.h                                        -*- C++ -*-
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

#if !defined (_octave_hist_h)
#define _octave_hist_h 1

#ifdef __GNUG__
#pragma interface
#endif

extern void initialize_history (void);
extern void clean_up_history (void);
extern void maybe_save_history (char*);
extern void do_history (int, char**);
extern void do_edit_history (int, char**);
extern void do_run_history (int, char**);
extern int current_history_number (void);

// Nonzero means we are saving history lines.
extern int saving_history;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
