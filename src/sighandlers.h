// sighandlers.h                                               -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

// This file should always be included after config.h!

#if !defined (octave_sighandlers_h)
#define octave_sighandlers_h 1

// Signal handler return type.
#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif
#ifndef BADSIG
#define BADSIG (RETSIGTYPE (*)(int))-1
#endif

typedef RETSIGTYPE sig_handler (int);

// Nonzero means we have already printed a message for this series of
// SIGPIPES.  We assume that the writer will eventually give up.
extern int pipe_handler_error_count;

// Nonzero means we can be interrupted.
extern int can_interrupt;

extern void install_signal_handlers (void);

// This is taken directly from Emacs 19:

#ifndef SYS_SIGLIST_DECLARED
extern char *sys_siglist[];
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
