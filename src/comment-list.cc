/*

Copyright (C) 2000 John W. Eaton

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

#include "lo-utils.h"

#include "comment-list.h"
#include "error.h"

octave_comment_buffer *octave_comment_buffer::instance = 0;

bool
octave_comment_buffer::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    instance = new octave_comment_buffer ();

  if (! instance)
    {
      ::error ("unable to create comment buffer object");

      retval = false;
    }

  return retval;
}

void
octave_comment_buffer::append (const std::string& s,
			       octave_comment_elt::comment_type t)
{
  if (instance_ok ())
    instance->do_append (s, t);
}

octave_comment_list *
octave_comment_buffer::get_comment (void)
{
  return (instance_ok ()) ? instance->do_get_comment () : 0;
}

void
octave_comment_buffer::do_append (const std::string& s,
				  octave_comment_elt::comment_type t)
{
  comment_list->append(s, t);
}

octave_comment_list *
octave_comment_buffer::do_get_comment (void)
{
  octave_comment_list *retval = 0;

  if (comment_list && comment_list->length () > 0)
    {
      retval = comment_list;
      comment_list = new octave_comment_list ();
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
