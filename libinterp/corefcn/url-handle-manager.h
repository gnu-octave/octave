////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

// Extracted from urlwrite.cc.

#if ! defined (octave_url_handle_manager)
#define octave_url_handle_manager 1

#include "octave-config.h"

#include <cmath>

#include <map>
#include <set>

#include "lo-mappers.h"

#include "oct-handle.h"
#include "url-transfer.h"

OCTAVE_BEGIN_NAMESPACE(octave)

typedef octave_handle url_handle;

class OCTINTERP_API url_handle_manager
{
public:

  url_handle_manager (void)
    : m_handle_map (), m_handle_free_list (),
      m_next_handle (-1.0 - (rand () + 1.0) / (RAND_MAX + 2.0)) { }

  url_handle_manager (const url_handle_manager&) = delete;

  url_handle_manager& operator = (const url_handle_manager&) = delete;

  ~url_handle_manager (void) = default;

  url_handle get_handle (void);

  void free (const url_handle& h);

  url_handle lookup (double val)
  {
    iterator p = (math::isnan (val) ? m_handle_map.end ()
                  : m_handle_map.find (val));

    return (p != m_handle_map.end ()) ? p->first : url_handle ();
  }

  url_handle lookup (const octave_value& val)
  {
    return val.is_real_scalar () ? lookup (val.double_value ())
           : url_handle ();
  }

  url_transfer get_object (double val)
  {
    return get_object (lookup (val));
  }

  url_transfer get_object (const octave_value& val)
  {
    return get_object (lookup (val));
  }

  url_transfer get_object (const url_handle& h)
  {
    iterator p = (h.ok () ? m_handle_map.find (h) : m_handle_map.end ());

    return (p != m_handle_map.end ()) ? p->second : url_transfer ();
  }

  url_handle make_url_handle (const std::string& host,
                              const std::string& user,
                              const std::string& passwd,
                              std::ostream& os)
  {
    url_handle h = get_handle ();

    url_transfer obj (host, user, passwd, os);

    if (! obj.is_valid ())
      error ("support for URL transfers was disabled when Octave was built");

    m_handle_map[h] = obj;

    return h;
  }

  Matrix handle_list (void)
  {
    Matrix retval (1, m_handle_map.size ());

    octave_idx_type i = 0;
    for (const auto& h_obj : m_handle_map)
      {
        url_handle h = h_obj.first;

        retval(i++) = h.value ();
      }

    return retval;
  }


private:

  typedef std::map<url_handle, url_transfer>::iterator iterator;
  typedef std::map<url_handle, url_transfer>::const_iterator const_iterator;

  typedef std::set<url_handle>::iterator free_list_iterator;
  typedef std::set<url_handle>::const_iterator const_free_list_iterator;

  // A map of handles to curl objects.
  std::map<url_handle, url_transfer> m_handle_map;

  // The available curl handles.
  std::set<url_handle> m_handle_free_list;

  // The next handle available if handle_free_list is empty.
  double m_next_handle;
};

OCTAVE_END_NAMESPACE(octave)

#endif
