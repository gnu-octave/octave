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

// Written by John C. Campbell <jcc@bevo.che.wisc.edu>
//
// Thomas Baier <baier@ci.tuwien.ac.at> added the following functions:
//
//   popen    pclose    execute  sync_system  async_system
//   waitpid  mkfifo   unlink


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>

#include "file-info.h"

file_info::file_info (void)
{
  file_number = -1;
  file_fptr = 0;
}

file_info::file_info (int n, const string& nm, FILE *t, const string& md)
{
  file_number = n;
  file_name = nm;
  file_fptr = t;
  file_mode = md;
}

file_info::file_info (const file_info& f)
{
  file_number = f.file_number;
  file_name = f.file_name;
  file_fptr = f.file_fptr;
  file_mode = f.file_mode;
}

file_info&
file_info::operator = (const file_info& f)
{
  if (this != & f)
    {
      file_number = f.file_number;
      file_name = f.file_name;
      file_fptr = f.file_fptr;
      file_mode = f.file_mode;
    }
  return *this;
}

file_info::~file_info (void)
{
}

int
file_info::number (void) const
{
  return file_number;
}

string
file_info::name (void) const
{
  return file_name;
}

FILE *
file_info::fptr (void) const
{
  return file_fptr;
}

string
file_info::mode (void) const
{
  return file_mode;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
