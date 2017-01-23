/*

Copyright (C) 2011-2016 John W. Eaton

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

#if ! defined (octave_singleton_cleanup_h)
#define octave_singleton_cleanup_h 1

#include "octave-config.h"

#include <set>

class
OCTAVE_API
singleton_cleanup_list
{
protected:

  singleton_cleanup_list (void) : fcn_list () { }

public:

  typedef void (*fptr) (void);

  // No copying!

  singleton_cleanup_list (const singleton_cleanup_list&) = delete;

  singleton_cleanup_list& operator = (const singleton_cleanup_list&) = delete;

  ~singleton_cleanup_list (void);

  static void add (fptr f)
  {
    if (instance_ok ())
      instance->do_add (f);
  }

  static void cleanup (void) { delete instance; instance = 0; }

private:

  static singleton_cleanup_list *instance;

  static bool instance_ok (void);

  static void cleanup_instance (void) { delete instance; instance = 0; }

  std::set<fptr> fcn_list;

  void do_add (fptr f)
  {
    fcn_list.insert (f);
  }
};

#endif
