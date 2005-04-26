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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
#include <cstdlib>
#include <cstring>

#include "sysdir.h"

#include "dir-ops.h"
#include "lo-error.h"
#include "lo-sysdep.h"
#include "str-vec.h"

bool
dir_entry::open (const std::string& n)
{
  fail = true;

  if (! n.empty ())
    name = n;

  if (! name.empty ())
    {
      close ();

      dir = static_cast<void *> (opendir (name.c_str ()));

      if (dir)
	fail = false;
      else
	{
	  using namespace std;
	  errmsg = strerror (errno);
	}
    }
  else
    errmsg = "dir_entry::open: empty file name";

  return ! fail;
}

string_vector
dir_entry::read (void)
{
  string_vector dirlist;

  if (ok ())
    {
      int count = 0;

      struct dirent *dir_ent;

      while ((dir_ent = readdir (static_cast<DIR *> (dir))))
	count++;

      rewinddir (static_cast<DIR *> (dir));

      dirlist.resize (count);

      for (int i = 0; i < count; i++)
	{
	  dir_ent = readdir (static_cast<DIR *> (dir));

	  if (dir_ent)
	    dirlist[i] = dir_ent->d_name;
	  else
	    break;
	}
    }

  return dirlist;
}

void
dir_entry::close (void)
{
  if (dir)
    closedir (static_cast<DIR *> (dir));

  dir = 0;
}

void
dir_entry::copy (const dir_entry& de)
{
  name = de.name;
  dir = de.dir;
  fail = de.fail;
  errmsg = de.errmsg;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
