/*

Copyright (C) 1996, 1997 John W. Eaton

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

#include <cfloat>
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <string>

#include <iostream.h>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#if defined (HAVE_TERMIOS_H)
#include <termios.h>
#elif defined (HAVE_TERMIO_H)
#include <termio.h>
#elif defined (HAVE_SGTTY_H)
#include <sgtty.h>
#else
LOSE! LOSE!
#endif

#if defined (HAVE_FLOATINGPOINT_H)
#include <floatingpoint.h>
#endif

#if defined (HAVE_IEEEFP_H)
#include <ieeefp.h>
#endif

#if !defined (HAVE_GETHOSTNAME) && defined (HAVE_SYS_UTSNAME_H)
#include <sys/utsname.h>
#endif

#include "cmd-edit.h"
#include "file-ops.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-env.h"

#include "defun.h"
#include "error.h"
#include "input.h"
#include "oct-obj.h"
#include "ov.h"
#include "sysdep.h"
#include "toplev.h"
#include "utils.h"

#ifndef STDIN_FILENO
#define STDIN_FILENO 1
#endif

#if defined (__386BSD__) || defined (__FreeBSD__)
static void
BSD_init (void)
{
#if defined (HAVE_FLOATINGPOINT_H)
  // Disable trapping on common exceptions.
  fpsetmask (~(FP_X_OFL|FP_X_INV|FP_X_DZ|FP_X_DNML|FP_X_UFL|FP_X_IMP));
#endif
}
#endif

#if defined (NeXT)
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
    panic ("probably irrecoverable malloc error: code %d", code);
}

static void
NeXT_init (void)
{
  malloc_error (malloc_handler);
}
#endif

#if defined (__EMX__)
OS2_init (void)
{
  _control87 ((EM_INVALID | EM_DENORMAL | EM_ZERODIVIDE | EM_OVERFLOW
	       | EM_UNDERFLOW | EM_INEXACT), MCW_EM);
}
#endif

#if defined (SCO)
static void
SCO_init (void)
{
#if defined (HAVE_IEEEFP_H)
  // Disable trapping on common exceptions.
  fpsetmask (~(FP_X_OFL|FP_X_INV|FP_X_DZ|FP_X_DNML|FP_X_UFL|FP_X_IMP));
#endif
}
#endif

void
sysdep_init (void)
{
#if defined (__386BSD__) || defined (__FreeBSD__)
  BSD_init ();
#elif defined (NeXT)
  NeXT_init ();
#elif defined (__EMX__)
  OS2_init ();
#elif defined (SCO)
  SCO_init ();
#endif

  octave_ieee_init ();
}

// Set terminal in raw mode.  From less-177.
//
// Change terminal to "raw mode", or restore to "normal" mode.
// "Raw mode" means 
//	1. An outstanding read will complete on receipt of a single keystroke.
//	2. Input is not echoed.  
//	3. On output, \n is mapped to \r\n.
//	4. \t is NOT expanded into spaces.
//	5. Signal-causing characters such as ctrl-C (interrupt),
//	   etc. are NOT disabled.
// It doesn't matter whether an input \n is mapped to \r, or vice versa.

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

// Read one character from the terminal.

int
kbhit (void)
{
  int c;
  raw_mode (1);
  c = cin.get ();
  raw_mode (0);
  return c;
}

DEFUN (clc, , ,
  "clc (): clear screen")
{
  command_editor::clear_screen ();

  return octave_value_list ();
}

DEFALIAS (home, clc);

DEFUN (getenv, args, ,
  "getenv (STRING): get environment variable values")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      string name = args(0).string_value ();

      if (! error_state)
	retval = octave_env::getenv (name);
    }
  else
    print_usage ("getenv");

  return retval;
}

DEFUN (putenv, args, ,
  "putenv (VAR, VALUE): define environment variable VAR=VALUE")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      string var = args(0).string_value (); 

      if (! error_state)
	{
	  string val = args(1).string_value (); 

	  if (! error_state)
	    octave_env::putenv (var, val);
	  else
	    error ("putenv: second argument should be a string");
	}
      else
	error ("putenv: first argument should be a string");
    }
  else
    print_usage ("putenv");

  return retval;
}

DEFUN (kbhit, , ,
  "kbhit: get a single character from the terminal")
{
  octave_value_list retval;

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

DEFUN (pause, args, ,
  "pause (seconds): suspend program execution")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (! (nargin == 0 || nargin == 1))
    {
      print_usage ("pause");
      return retval;
    }

  if (nargin == 1)
    {
      double dval = args(0).double_value ();

      if (! error_state)
	{
	  if (xisnan (dval))
	    warning ("pause: NaN is an invalid delay");
	  else if (xisinf (dval))
	    kbhit ();
	  else
	    {
	      int delay = NINT (dval);
	      if (delay > 0)
		sleep (delay);
	    }
	}
    }
  else
    kbhit ();

  return retval;
}

DEFUN (sleep, args, ,
  "sleep (seconds): suspend program execution")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      double dval = args(0).double_value ();

      if (! error_state)
	{
	  if (xisnan (dval))
	    warning ("sleep: NaN is an invalid delay");
	  else
	    {
	      int delay = NINT (dval);
	      if (delay > 0)
		sleep (delay);
	    }
	}
    }
  else
    print_usage ("sleep");

  return retval;
}

DEFUN (usleep, args, ,
  "usleep (microseconds): suspend program execution")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      double dval = args(0).double_value ();

      if (! error_state)
	{
	  if (xisnan (dval))
	    warning ("usleep: NaN is an invalid delay");
	  else
	    {
#if defined (HAVE_USLEEP)
	      int delay = NINT (dval);

	      if (delay > 0)
		usleep (delay);
#else
	      int delay = NINT (dval / 1e6);

	      if (delay > 0)
		sleep (delay);
#endif
	    }
	}
    }
  else
    print_usage ("usleep");

  return retval;
}

// XXX FIXME XXX -- maybe this should only return 1 if IEEE floating
// point functions really work.

DEFUN (isieee, , ,
  "isieee (): return 1 if host uses IEEE floating point")
{
  oct_mach_info::float_format flt_fmt =
    oct_mach_info::native_float_format ();

  return static_cast<double> (flt_fmt == oct_mach_info::ieee_little_endian
			      || flt_fmt == oct_mach_info::ieee_big_endian);
}

DEFUN (tilde_expand, args, ,
  "tilde_expand (STRING): perform tilde expansion on STRING")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    retval = file_ops::tilde_expand (args(0).all_strings ());
  else
    print_usage ("tilde_expand");

  return retval;
}

#if defined (__EMX__) && defined (OS2)

DEFUN_TEXT (extproc, , ,
  "extproc: ignored by Octave")
{
  return octave_value_list ();
}

DEFALIAS (EXTPROC, extproc);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
