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

#if !defined (octave_file_info_h)
#define octave_file_info_h 1

#include <cstdio>

#include <string>

#include <Pix.h>

class
file_info
{
public:
  file_info (void);
  file_info (int num, const string& nm, FILE *t, const string& md);
  file_info (const file_info& f);

  file_info& operator = (const file_info& f);

  ~file_info (void);

  int number (void) const;
  string name (void) const;
  FILE *fptr (void) const;
  string mode (void) const;

  int eof (void) const;
  int error (void) const;

private:
  int file_number;
  string file_name;
  FILE *file_fptr;
  string file_mode;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
