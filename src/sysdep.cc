// sysdep.cc                                              -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <math.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

#include "tree-const.h"
#include "octave.h"
#include "help.h"
#include "input.h"
#include "utils.h"
#include "oct-obj.h"
#include "error.h"
#include "sysdep.h"
#include "defun.h"

extern "C"
{
#include <readline/readline.h>

extern char *term_clrpag;
extern void _rl_output_character_function ();

#if defined (HAVE_TERMIOS_H)
#include <termios.h>
#elif defined (HAVE_TERMIO_H)
#include <termio.h>
#elif defined (HAVE_SGTTY_H)
#include <sgtty.h>
#else
LOSE! LOSE!
#endif

extern int ioctl ();
}

#ifndef STDIN_FILENO
#define STDIN_FILENO 1
#endif

// Octave's idea of infinity.
double octave_Inf;

// Octave's idea of not a number.
double octave_NaN;

#if defined (__386BSD__) && defined (HAVE_FLOATINGPOINT_H)
#include <floatingpoint.h>
#endif

#ifdef NeXT
extern "C"
{
  typedef void (*_cplus_fcn_int) (int);
  extern void (*malloc_error (_cplus_fcn_int)) (int);
}

static void
malloc_handler (int code)
{
  if (code == 5)
    warning ("hopefully recoverable malloc error: freeing wild pointer");
  else
    {
      panic ("probably irrecoverable malloc error: code %d", code);
    }
}

static void
NeXT_init (void)
{
  malloc_error (malloc_handler);
}
#endif

static void
octave_ieee_init (void)
{
#if defined (HAVE_ISINF) || defined (HAVE_FINITE)

// Some version of gcc on some old version of Linux used to crash when
// trying to make Inf and NaN.

#if defined (HAVE_INFINITY)
  octave_Inf = (double) infinity ();
#else
#ifdef linux
  octave_Inf = HUGE_VAL;
#else
#ifdef __alpha__
  extern unsigned int DINFINITY[2];
  octave_Inf =  (*((double *) (DINFINITY)));
#else
  double tmp = 1e+10;
  octave_Inf = tmp;
  for (;;)
    {
      octave_Inf *= 1e+10;
      if (octave_Inf == tmp)
	break;
      tmp = octave_Inf;
    }
#endif
#endif
#endif



#if defined (HAVE_QUIET_NAN)
  octave_NaN = (double) quiet_nan ();
#else
#ifdef linux
  octave_NaN = NAN;
#else
#ifdef __alpha__
  extern unsigned int DQNAN[2];
  octave_NaN = (*((double *) (DQNAN)));
#else
  octave_NaN = octave_Inf / octave_Inf;
#endif
#endif
#endif

#else

// This is sort of cheesy, but what can we do, other than blowing it
// off completely, or writing an entire IEEE emulation package?

  octave_Inf = DBL_MAX;
  octave_NaN = DBL_MAX;

#endif
}


#if defined (EXCEPTION_IN_MATH)
extern "C"
{
int
matherr (struct exception *x)
{
// Possibly print our own message someday.  Should probably be
// user-switchable.

  switch (x->type)
    {
    case DOMAIN:
    case SING:
    case OVERFLOW:
    case UNDERFLOW:
    case TLOSS:
    case PLOSS:
    default:
      break;
    }

// But don't print the system message.

  return 1;
}
}
#endif

void
sysdep_init (void)
{
#if defined (__386BSD__) && defined (HAVE_FLOATINGPOINT_H)
// Disable trapping on common exceptions.
  fpsetmask (~(FP_X_OFL|FP_X_INV|FP_X_DZ|FP_X_DNML|FP_X_UFL|FP_X_IMP));
#endif

#ifdef NeXT
  NeXT_init ();
#endif

  octave_ieee_init ();
}

/*
 * Set terminal in raw mode.  From less-177.
 *
 * Change terminal to "raw mode", or restore to "normal" mode.
 * "Raw mode" means 
 *	1. An outstanding read will complete on receipt of a single keystroke.
 *	2. Input is not echoed.  
 *	3. On output, \n is mapped to \r\n.
 *	4. \t is NOT expanded into spaces.
 *	5. Signal-causing characters such as ctrl-C (interrupt),
 *	   etc. are NOT disabled.
 * It doesn't matter whether an input \n is mapped to \r, or vice versa.
 */
void
raw_mode (int on)
{
  static int curr_on = 0;

  int tty_fd = STDIN_FILENO;
  if (! isatty (tty_fd))
    {
      if (interactive)
	error ("stdin is not a tty!");
      return;
    }

  if (on == curr_on)
    return;

#if defined (HAVE_TERMIOS_H)
  {
    struct termios s;
    static struct termios save_term;

    if (on)
      {
// Get terminal modes.

	tcgetattr (tty_fd, &s);

// Save modes and set certain variables dependent on modes.

	save_term = s;
//	ospeed = s.c_cflag & CBAUD;
//	erase_char = s.c_cc[VERASE];
//	kill_char = s.c_cc[VKILL];

// Set the modes to the way we want them.

	s.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
	s.c_oflag |=  (OPOST|ONLCR);
#if defined (OCRNL)
	s.c_oflag &= ~(OCRNL);
#endif
#if defined (ONOCR)
	s.c_oflag &= ~(ONOCR);
#endif
#if defined (ONLRET)
	s.c_oflag &= ~(ONLRET);
#endif
	s.c_cc[VMIN] = 1;
	s.c_cc[VTIME] = 0;
      }      
    else
      {
// Restore saved modes.
	s = save_term;
      }
    tcsetattr (tty_fd, TCSAFLUSH, &s);
  }
#elif defined (HAVE_TERMIO_H)
  {
    struct termio s;
    static struct termio save_term;

    if (on)
      {
// Get terminal modes.

	ioctl (tty_fd, TCGETA, &s);

// Save modes and set certain variables dependent on modes.

	save_term = s;
//	ospeed = s.c_cflag & CBAUD;
//	erase_char = s.c_cc[VERASE];
//	kill_char = s.c_cc[VKILL];

// Set the modes to the way we want them.

	s.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
	s.c_oflag |=  (OPOST|ONLCR);
#if defined (OCRNL)
	s.c_oflag &= ~(OCRNL);
#endif
#if defined (ONOCR)
	s.c_oflag &= ~(ONOCR);
#endif
#if defined (ONLRET)
	s.c_oflag &= ~(ONLRET);
#endif
	s.c_cc[VMIN] = 1;
	s.c_cc[VTIME] = 0;
      }      
    else
      {
// Restore saved modes.
	s = save_term;
      }
    ioctl (tty_fd, TCSETAW, &s);
  }
#elif defined (HAVE_SGTTY_H)
  {
    struct sgttyb s;
    static struct sgttyb save_term;

    if (on)
      {
// Get terminal modes.

	ioctl (tty_fd, TIOCGETP, &s);

// Save modes and set certain variables dependent on modes.

	save_term = s;
//	ospeed = s.sg_ospeed;
//	erase_char = s.sg_erase;
//	kill_char = s.sg_kill;

// Set the modes to the way we want them.

	s.sg_flags |= CBREAK;
	s.sg_flags &= ~(ECHO);
      } 
    else
      {
// Restore saved modes.
	s = save_term;
      }
    ioctl (tty_fd, TIOCSETN, &s);
  }
#else
LOSE! LOSE!
#endif

  curr_on = on;
}

/*
 * Read one character from the terminal.
 */
int
kbhit (void)
{
  int c;
  raw_mode (1);
  c = cin.get ();
  raw_mode (0);
  return c;
}

DEFUN ("clc", Fclc, Sclc, 1, 0,
  "clc (): clear screen")
{
  Octave_object retval;

  rl_beg_of_line ();
  rl_kill_line (1);

#if ! defined (_GO32_)
  if (term_clrpag)
    tputs (term_clrpag, 1, _rl_output_character_function);
  else
    crlf ();
#else
  crlf ();
#endif

  fflush (rl_outstream);

  return retval;
}

DEFALIAS (home, clc);

DEFUN ("getenv", Fgetenv, Sgetenv, 2, 1,
  "getenv (STRING): get environment variable values")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 2 && args(1).is_string_type ())
    {
      char *value = getenv (args(1).string_value ());
      if (value)
	retval = value;
      else
	retval = "";
    }
  else
    print_usage ("getenv");

  return retval;
}

DEFUN ("kbhit", Fkbhit, Skbhit, 1, 1,
  "kbhit: get a single character from the terminal")
{
  Octave_object retval;

// XXX FIXME XXX -- add timeout and default value args?

  if (interactive)
    {
      int c = kbhit ();
      char *s = new char [2];
      s[0] = c;
      s[1] = '\0';
      retval = s;
    }

  return retval;
}

DEFUN ("pause", Fpause, Spause, 1, 1,
  "pause (seconds): suspend program execution")
{
  Octave_object retval;

  int nargin = args.length ();

  if (! (nargin == 1 || nargin == 2))
    {
      print_usage ("pause");
      return retval;
    }

  if (interactive)
    {
      switch (nargin)
	{
	case 2:
	  {
	    int delay = NINT (args(1).double_value ());
	    if (delay > 0)
	      {
		sleep (delay);
		break;
	      }
	  }
	default:
	  if (kbhit () == EOF)
	    clean_up_and_exit (0);
	  break;
	}
    }

  return retval;
}

#if !defined (HAVE_GETHOSTNAME) && defined (HAVE_SYS_UTSNAME_H)
extern "C"
{
#include <sys/utsname.h>
int
gethostname (char *name, int namelen)
{
  int i;
  struct utsname ut;

  --namelen;

  uname (&ut);
  i = strlen (ut.nodename) + 1;
  strncpy (name, ut.nodename, i < namelen ? i : namelen);
  name[namelen] = '\0';

  return 0;
}
}
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
