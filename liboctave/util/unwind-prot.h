/*

Copyright (C) 1993-2018 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

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

#if ! defined (octave_unwind_prot_h)
#define octave_unwind_prot_h 1

#include "octave-config.h"

#include <cstddef>

#include <stack>
#include <memory>

#include "action-container.h"

namespace octave
{
  class
  OCTAVE_API
  unwind_protect : public action_container
  {
  public:

    unwind_protect (void) : lifo () { }

    // No copying!

    unwind_protect (const unwind_protect&) = delete;

    unwind_protect& operator = (const unwind_protect&) = delete;

    // Destructor should not raise an exception, so all actions
    // registered should be exception-safe.  If you're not sure, see
    // unwind_protect_safe.

    ~unwind_protect (void) { run (); }

    operator bool (void) const { return ! empty (); }

    void run_first (void)
    {
      if (! empty ())
        {
          // No leak on exception!
          std::unique_ptr<elem> ptr (lifo.top ());
          lifo.pop ();
          ptr->run ();
        }
    }

    void discard_first (void)
    {
      if (! empty ())
        {
          elem *ptr = lifo.top ();
          lifo.pop ();
          delete ptr;
        }
    }

    size_t size (void) const { return lifo.size (); }

  protected:

    virtual void add_action (elem *new_elem)
    {
      lifo.push (new_elem);
    }

    std::stack<elem *> lifo;
  };

  // Like unwind_protect, but this one will guard against the possibility
  // of seeing an exception (or interrupt) in the cleanup actions.
  // Not that we can do much about it, but at least we won't crash.

  class
  OCTAVE_API
  unwind_protect_safe : public unwind_protect
  {
  private:

    void warn_unhandled_exception (void) const;

  public:

    unwind_protect_safe (void) : unwind_protect () { }

    // No copying!

    unwind_protect_safe (const unwind_protect_safe&) = delete;

    unwind_protect_safe& operator = (const unwind_protect_safe&) = delete;

    ~unwind_protect_safe (void)
    {
      while (! empty ())
        {
          try
            {
              run_first ();
            }
          catch (...) // Yes, the black hole.  Remember we're in a destructor.
            {
              warn_unhandled_exception ();
            }
        }
    }
  };
}

#endif
