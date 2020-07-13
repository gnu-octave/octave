////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020 The Octave Project Developers
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

#if ! defined (octave_qt_utils_h)
#define octave_qt_utils_h 1

#include <list>

#include <QList>

namespace octave
{
  template <typename T>
  QList<T>
  std_list_to_qt_list (const std::list<T>& lst)
  {
#if defined (HAVE_QLIST_ITERATOR_CONSTRUCTOR)
    return QList<T> (lst.begin (), lst.end ());
#else
    return QList<T>::fromStdList (lst);
#endif
  }
}

#endif
