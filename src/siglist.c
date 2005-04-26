/*

Copyright (C) 2000 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <signal.h>

#include "siglist.h"

#ifndef HAVE_SYS_SIGLIST
char *sys_siglist[NSIG + 1] =
{
#ifdef AIX
/* AIX has changed the signals a bit */
  "bogus signal",			/* 0 */
  "hangup",				/* 1  SIGHUP */
  "interrupt",				/* 2  SIGINT */
  "quit",				/* 3  SIGQUIT */
  "illegal instruction",		/* 4  SIGILL */
  "trace trap",				/* 5  SIGTRAP */
  "IOT instruction",			/* 6  SIGIOT */
  "crash likely",			/* 7  SIGDANGER */
  "floating point exception",		/* 8  SIGFPE */
  "kill",				/* 9  SIGKILL */
  "bus error",				/* 10 SIGBUS */
  "segmentation violation",		/* 11 SIGSEGV */
  "bad argument to system call",	/* 12 SIGSYS */
  "write on a pipe with no one to read it", /* 13 SIGPIPE */
  "alarm clock",			/* 14 SIGALRM */
  "software termination signum",	/* 15 SIGTERM */
  "user defined signal 1",		/* 16 SIGUSR1 */
  "user defined signal 2",		/* 17 SIGUSR2 */
  "death of a child",			/* 18 SIGCLD */
  "power-fail restart",			/* 19 SIGPWR */
  "bogus signal",			/* 20 */
  "bogus signal",			/* 21 */
  "bogus signal",			/* 22 */
  "bogus signal",			/* 23 */
  "bogus signal",			/* 24 */
  "LAN I/O interrupt",			/* 25 SIGAIO */
  "PTY I/O interrupt",			/* 26 SIGPTY */
  "I/O intervention required",		/* 27 SIGIOINT */
  "HFT grant",				/* 28 SIGGRANT */
  "HFT retract",			/* 29 SIGRETRACT */
  "HFT sound done",			/* 30 SIGSOUND */
  "HFT input ready",			/* 31 SIGMSG */
#else /* not AIX */
#ifdef __EMX__
  "bogus signal #0",                    /* 0 */
  "hangup",                             /* 1  SIGHUP */
  "interrupt (Ctrl-C)",                 /* 2  SIGINT (Ctrl-C) */
  "quit",                               /* 3  SIGQUIT */
  "illegal instruction",                /* 4  SIGILL */
  "single step",                        /* 5  SIGTRAP */
  "abort",                              /* 6  SIGABRT */
  "EMT instruction",                    /* 7  SIGEMT */
  "floating point exception",           /* 8  SIGFPE */
  "kill",                               /* 9  SIGKILL */
  "bus error",                          /* 10 SIGBUS */
  "segmentation violation",             /* 11 SIGSEGV */
  "bad argument to system call",        /* 12 SIGSYS */
  "broken pipe",                        /* 13 SIGPIPE */
  "alarm clock",                        /* 14 SIGALRM */
  "software termination signum",        /* 15 SIGTERM */
  "user defined signal 1",              /* 16 SIGUSR1 */
  "user defined signal 2",              /* 17 SIGUSR2 */
  "death of a child",                   /* 18 SIGCHLD */
  "bogus signal #20",                   /* 19 */
  "bogus signal #21",                   /* 20 */
  "break (Ctrl-Break)",                 /* 21 SIGBREAK */
#else /* not __EMX__ */
  "bogus signal",			/* 0 */
  "hangup",				/* 1  SIGHUP */
  "interrupt",				/* 2  SIGINT */
  "quit",				/* 3  SIGQUIT */
  "illegal instruction",		/* 4  SIGILL */
  "trace trap",				/* 5  SIGTRAP */
  "IOT instruction",			/* 6  SIGIOT */
  "EMT instruction",			/* 7  SIGEMT */
  "floating point exception",		/* 8  SIGFPE */
  "kill",				/* 9  SIGKILL */
  "bus error",				/* 10 SIGBUS */
  "segmentation violation",		/* 11 SIGSEGV */
  "bad argument to system call",	/* 12 SIGSYS */
  "write on a pipe with no one to read it", /* 13 SIGPIPE */
  "alarm clock",			/* 14 SIGALRM */
  "software termination signum",	/* 15 SIGTERM */
  "user defined signal 1",		/* 16 SIGUSR1 */
  "user defined signal 2",		/* 17 SIGUSR2 */
  "death of a child",			/* 18 SIGCLD */
  "power-fail restart",			/* 19 SIGPWR */
#ifdef sun
  "window size change",			    /* 20 SIGWINCH */
  "urgent socket condition",		    /* 21 SIGURG */
  "pollable event occured",		    /* 22 SIGPOLL */
  "stop (cannot be caught or ignored)", /*  23 SIGSTOP */
  "user stop requested from tty",	    /* 24 SIGTSTP */
  "stopped process has been continued",	/* 25 SIGCONT */
  "background tty read attempted",	    /* 26 SIGTTIN */
  "background tty write attempted",    /* 27 SIGTTOU */
  "virtual timer expired",		    /* 28 SIGVTALRM */
  "profiling timer expired",		    /* 29 SIGPROF */
  "exceeded cpu limit",			    /* 30 SIGXCPU */
  "exceeded file size limit",		    /* 31 SIGXFSZ */
  "process's lwps are blocked",	    /*  32 SIGWAITING */
  "special signal used by thread library", /* 33 SIGLWP */
#ifdef SIGFREEZE
  "Special Signal Used By CPR",	    /* 34 SIGFREEZE */
#endif
#ifdef SIGTHAW
  "Special Signal Used By CPR",	    /* 35 SIGTHAW */
#endif
#endif /* sun */
#endif /* __EMX__ */
#endif /* AIX */
  0
  };
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
