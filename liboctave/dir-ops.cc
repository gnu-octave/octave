/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
#include <cstdlib>
#include <cstring>

#include "sysdir.h"

#include "dir-ops.h"
#include "file-ops.h"
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
      
      std::string fullname = file_ops::tilde_expand (name);

      dir = static_cast<void *> (opendir (fullname.c_str ()));

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
  static octave_idx_type grow_size = 100;

  octave_idx_type len = 0;

  string_vector dirlist;

  if (ok ())
    {
      int count = 0;

      struct dirent *dir_ent;

      while ((dir_ent = readdir (static_cast<DIR *> (dir))))
	{
	  if (dir_ent)
	    {
	      if (count >= len)
		{
		  len += grow_size;
		  dirlist.resize (len);
		}

	      dirlist[count] = dir_ent->d_name;

	      count++;
	    }
	  else
	    break;
	}

      dirlist.resize (count);
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
