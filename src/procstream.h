// procstream.h                                         -*- C++ -*-
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

#if !defined (octave_procstream_h)
#define octave_procstream_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <iostream.h>
#include <procbuf.h>

class
procstreambase : virtual public ios
{
 public:
  procstreambase (void);
  procstreambase (const char *command, int mode = ios::out);

  procbuf *rdbuf (void) const { return (procbuf *) _strbuf; }

  void open (const char *command, int mode = ios::out);
  int is_open (void) { return rdbuf()->is_open (); }
  void close (void);
};

class
iprocstream : public procstreambase, public istream
{
 public:
  iprocstream (void) : procstreambase () {}
  iprocstream (const char *command) : procstreambase (command, ios::in) {}

  void open (const char *command) { procstreambase::open (command, ios::in); }
};

class
oprocstream : public procstreambase, public ostream
{
 public:
  oprocstream (void) : procstreambase () {}
  oprocstream (const char *command) : procstreambase (command, ios::out) {}

  void open (const char *command) { procstreambase::open (command, ios::out); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/



