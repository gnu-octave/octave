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

#if !defined (octave_procstream_h)
#define octave_procstream_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <iostream.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "oct-procbuf.h"

class
procstreambase : virtual public ios
{
public:

  procstreambase (void) { pb_init (); }

  procstreambase (const char *name, int mode);

  ~procstreambase (void) { close (); }

  void open (const char *name, int mode);

  int is_open (void) const { return pb.is_open (); }

  int close (void);

  pid_t pid (void) { return pb.pid (); }

private:

  octave_procbuf pb;

  void pb_init (void) { init (&pb); }

  procstreambase (const procstreambase&);

  procstreambase& operator = (const procstreambase&);
};

class
iprocstream : public procstreambase, public istream
{
public:

  iprocstream (void) : procstreambase () { }

  iprocstream (const char *name, int mode=ios::in)
    : procstreambase(name, mode) { }

  ~iprocstream (void) { }

  void open (const char *name, int mode=ios::in)
    {
      procstreambase::open (name, mode);
    }

private:

  iprocstream (const iprocstream&);

  iprocstream& operator = (const iprocstream&);
};

class
oprocstream : public procstreambase, public ostream
{
public:
 
  oprocstream (void) : procstreambase () { }

  oprocstream (const char *name, int mode=ios::out)
    : procstreambase(name, mode) { }

  ~oprocstream (void) { }

  void open (const char *name, int mode=ios::out)
    {
      procstreambase::open(name, mode);
    }

private:

  oprocstream (const oprocstream&);

  oprocstream& operator = (const oprocstream&);
};

class
procstream : public procstreambase, public iostream
{
public:

  procstream (void) : procstreambase () { }

  procstream (const char *name, int mode)
    : procstreambase(name, mode) { }


  ~procstream (void) { }

  void open (const char *name, int mode)
    {
      procstreambase::open(name, mode);
    }

private:

  procstream (const procstream&);

  procstream& operator = (const procstream&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
