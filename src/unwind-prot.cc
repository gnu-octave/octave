/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2004,
              2005, 2006, 2007, 2008 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#include <cstddef>
#include <cstring>

#include "error.h"
#include "unwind-prot.h"
#include "utils.h"

std::stack<unwind_protect::elem *> unwind_protect::elt_list;

std::stack<std::pair <std::string, unwind_protect::frame_id_t> > unwind_protect::tag_list;

void
unwind_protect::begin_frame (const std::string& tag)
{
  tag_list.push (std::make_pair (tag, begin_frame ()));
}

void
unwind_protect::run_frame (const std::string& tag)
{
  while (! tag_list.empty ())
    {
      std::pair<std::string, frame_id_t> top = tag_list.top ();
      tag_list.pop ();

      run_frame (top.second);
      if (top.first == tag)
        break;
    }
}

void
unwind_protect::discard_frame (const std::string& tag)
{
  while (! tag_list.empty ())
    {
      std::pair<std::string, frame_id_t> top = tag_list.top ();
      tag_list.pop ();

      run_frame (top.second);
      if (top.first == tag)
        break;
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
