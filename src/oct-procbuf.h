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

// This class is based on the procbuf class from libg++, written by
// Per Bothner, Copyright (C) 1993 Free Software Foundation.

#if !defined (octave_octave_procbuf_h)
#define octave_octave_procbuf_h 1

#include <streambuf.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

class
octave_procbuf : public filebuf
{
public:

  octave_procbuf (void)
    : filebuf (), proc_pid (-1), next (0) { }

  octave_procbuf (const char *command, int mode)
    : filebuf (), proc_pid (-1), next (0) { open (command, mode); }

  ~octave_procbuf (void) { close (); }

  octave_procbuf *open (const char *command, int mode);

  octave_procbuf *close (void)
    { return static_cast<octave_procbuf *> (filebuf::close ()); }

  virtual int sys_close (void);

  pid_t pid (void) { return proc_pid; }

protected:

  pid_t proc_pid;

  octave_procbuf *next;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
