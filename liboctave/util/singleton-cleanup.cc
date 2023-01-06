////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "lo-error.h"
#include "singleton-cleanup.h"

singleton_cleanup_list *singleton_cleanup_list::s_instance = nullptr;

singleton_cleanup_list::~singleton_cleanup_list (void)
{
  for (fptr fcn : m_fcn_list)
    {
      fcn ();
    }
}

bool
singleton_cleanup_list::instance_ok (void)
{
  bool retval = true;

  if (! s_instance)
    s_instance = new singleton_cleanup_list ();

  return retval;
}
