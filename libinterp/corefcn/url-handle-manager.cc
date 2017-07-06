/*

Copyright (C) 2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// Extracted from urlwrite.cc.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cmath>

#include "url-handle-manager.h"

static double
make_handle_fraction (void)
{
  static double maxrand = RAND_MAX + 2.0;

  return (rand () + 1.0) / maxrand;
}

namespace octave
{
  url_handle url_handle_manager::get_handle (void)
  {
    url_handle retval;

    // Curl handles are negative integers plus some random fractional
    // part.  To avoid running out of integers, we recycle the integer
    // part but tack on a new random part each time.

    free_list_iterator p = handle_free_list.begin ();

    if (p != handle_free_list.end ())
      {
        retval = *p;
        handle_free_list.erase (p);
      }
    else
      {
        retval = url_handle (next_handle);

        next_handle = std::ceil (next_handle) - 1.0 - make_handle_fraction ();
      }

    return retval;
  }

  void url_handle_manager::free (const url_handle& h)
  {
    if (h.ok ())
      {
        iterator p = handle_map.find (h);

        if (p == handle_map.end ())
          error ("url_handle_manager::free: invalid object %g", h.value ());

        // Curl handles are negative integers plus some random
        // fractional part.  To avoid running out of integers, we
        // recycle the integer part but tack on a new random part
        // each time.

        handle_map.erase (p);

        if (h.value () < 0)
          handle_free_list.insert
            (std::ceil (h.value ()) - make_handle_fraction ());
      }
  }
}
