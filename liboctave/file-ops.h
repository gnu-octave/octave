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

#if !defined (octave_file_ops_h)
#define octave_file_ops_h 1

#include <string>

class string_vector;

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

struct
file_ops
{
  static int mkdir (const string&, mode_t);
  static int mkdir (const string&, mode_t, string&);

  static int mkfifo (const string&, mode_t);
  static int mkfifo (const string&, mode_t, string&);

  static int rename (const string&, const string&);
  static int rename (const string&, const string&, string&);

  static int rmdir (const string&);
  static int rmdir (const string&, string&);

  static string tempnam (const string&, const string&);
  static string tempnam (const string&, const string&, string&);

  static string tilde_expand (const string&);
  static string_vector tilde_expand (const string_vector&);

  static int umask (mode_t);

  static int unlink (const string&);
  static int unlink (const string&, string&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
