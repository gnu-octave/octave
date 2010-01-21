/*

Copyright (C) 1996, 1997, 1998, 2000, 2002, 2004, 2005, 2006, 2007
              John W. Eaton

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

#include <new>

#include "oct-alloc.h"

void *
octave_allocator::alloc (size_t size)
{
  if (size != item_size)
    return ::new char [size];

  if (! head)
    {
      if (! grow ())
	return 0;
    }

  link *tmp = head;
  head = head->next;
  return tmp;
}

// FIXME -- if we free the last item on the list, shouldn't we
// also free the underlying character array used for storage?

void
octave_allocator::free (void *p, size_t size)
{
  if (size != item_size)
    ::delete [] (static_cast<char *> (p));
  else
    {
      link *tmp = static_cast<link *> (p);
      tmp->next = head;
      head = tmp;
    }
}

// Return TRUE for successful allocation, FALSE otherwise.

bool
octave_allocator::grow (void)
{
  bool retval = true;

  char *start = new char [grow_size * item_size];

  if (start)
    {
      char *last = &start[(grow_size - 1) * item_size];

      char *p = start;
      while (p < last)
	{
	  char *next = p + item_size;
	  (reinterpret_cast<link *> (p)) -> next
	    = reinterpret_cast<link *> (next);
	  p = next;
	}

      (reinterpret_cast<link *> (last)) -> next = 0;

      head = reinterpret_cast<link *> (start);
    }
  else
    {
      typedef void (*error_handler_function) (void);

      error_handler_function f = std::set_new_handler (0);
      std::set_new_handler (f);

      if (f)
	f ();

      retval = false;
    }

  return retval;
}
