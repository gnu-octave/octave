// dir-ops.h                                            -*- C++ -*-
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

#if !defined (octave_dir_ops_h)
#define octave_dir_ops_h 1

#include <string>

#include "str-vec.h"

struct DIR;

class
dir_entry
{
public:

  dir_entry (const string& n = string ()) : name (n), dir (0)
      {
	if (! name.empty ())
	  open ();
      }

  dir_entry (const dir_entry& d) { copy (d); }

  dir_entry& operator = (const dir_entry& d)
    {
      copy (d);
      return *this;
    }

  ~dir_entry (void) { close (); }

  bool open (const string& = string ());

  string_vector read (void);

  void close (void);

  bool ok (void) const { return dir && ! fail; }

  operator void* () const { return ok () ? (void *) -1 : (void *) 0; }

  string error (void) const { return ok () ? string () : errmsg; }

private:

  // Name of the directory.
  string name;

  // A pointer to the contents of the directory.
  DIR *dir;

  // TRUE means the open for this directory failed.
  bool fail;

  // If a failure occurs, this contains the system error text.
  string errmsg;

  void copy (const dir_entry&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
