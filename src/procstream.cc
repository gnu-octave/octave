// procstream.cc                                          -*- C++ -*-
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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <fstream.h>
#include <procbuf.h>

#include "procstream.h"

iprocstream::iprocstream (const char *command, int mode)
{
  pbuf = new procbuf ();

  init (pbuf);

  if (! pbuf->open (command, mode))
    set (ios::badbit);
}

iprocstream::~iprocstream (void)
{
  delete pbuf;
}

void
iprocstream::open (const char *command, int mode)
{
  clear ();

  if (pbuf)
    delete pbuf;

  pbuf = new procbuf ();

  init (pbuf);

  if (! pbuf->open (command, mode))
    set (ios::badbit);
}

int
iprocstream::is_open (void)
{
  return pbuf && pbuf->is_open ();
}

int
iprocstream::close (void)
{
  int status = 0;

  if (is_open ())
    {
      status = pbuf->sys_close ();

      if (! pbuf->close ())
	set (ios::failbit);
    }

  return status;
}

void
cleanup_iprocstream (void *buf)
{
  delete (iprocstream *) buf;
}

oprocstream::oprocstream (const char *command, int mode)
{
  pbuf = new procbuf ();

  init (pbuf);

  if (! pbuf->open (command, mode))
    set (ios::badbit);
}

oprocstream::~oprocstream (void)
{
  delete pbuf;
}

void
oprocstream::open (const char *command, int mode)
{
  clear ();

  if (pbuf)
    delete pbuf;
    
  pbuf = new procbuf ();

  init (pbuf);

  if (! pbuf->open (command, mode))
    set (ios::badbit);
}

int
oprocstream::is_open (void)
{
  return pbuf && pbuf->is_open ();
}

int
oprocstream::close (void)
{
  int status = 0;

  if (is_open ())
    {
      if (! pbuf->close ())
	set (ios::failbit);
    }

  return status;
}

void
cleanup_oprocstream (void *buf)
{
  delete (oprocstream *) buf;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
