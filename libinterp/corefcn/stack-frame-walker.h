/*

Copyright (C) 2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_stack_frame_walker_h)
#define octave_stack_frame_walker_h 1

#include "octave-config.h"

namespace octave
{
  class compiled_fcn_stack_frame;
  class script_stack_frame;
  class user_fcn_stack_frame;
  class scope_stack_frame;

  class stack_frame_walker
  {
  protected:

    stack_frame_walker (void) { }

    virtual ~stack_frame_walker (void) = default;

  public:

    // No copying!

    stack_frame_walker (const stack_frame_walker&) = delete;

    stack_frame_walker& operator = (const stack_frame_walker&) = delete;

    virtual void
    visit_compiled_fcn_stack_frame (compiled_fcn_stack_frame&) = 0;

    virtual void
    visit_script_stack_frame (script_stack_frame&) = 0;

    virtual void
    visit_user_fcn_stack_frame (user_fcn_stack_frame&) = 0;

    virtual void
    visit_scope_stack_frame (scope_stack_frame&) = 0;
  };
}

#endif
