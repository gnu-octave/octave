// procstream.h                                         -*- C++ -*-
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

#if !defined (octave_procstream_h)
#define octave_procstream_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <fstream.h>

class procbuf;

class
iprocstream : public ifstream
{
public:
  iprocstream (void);
  iprocstream (const char *command, int mode=ios::in);

  ~iprocstream (void);

  void open (const char *command, int mode=ios::in);

  int is_open (void);

  int close (void);

private:
  procbuf *pbuf;
};

class
oprocstream : public ofstream
{
public:
  oprocstream (void);
  oprocstream (const char *command, int mode=ios::out);

  ~oprocstream (void);

  void open (const char *command, int mode=ios::out);

  int is_open (void);

  int close (void);

private:
  procbuf *pbuf;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
