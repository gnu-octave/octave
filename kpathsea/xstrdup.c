/* xstrdup.c: strdup with error checking.

Copyright (C) 1992, 93 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  */

#include <kpathsea/config.h>


/* Return a copy of S in new storage.  */

string
xstrdup P1C(const_string, s)
{
  string new_string = (string) xmalloc (strlen (s) + 1);
  return strcpy (new_string, s);
}
