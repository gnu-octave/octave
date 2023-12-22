////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2024 The Octave Project Developers
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

#if ! defined (octave_unwind_prot_h)
#define octave_unwind_prot_h 1

#include "octave-config.h"

#include <cstddef>

#include <stack>
#include <memory>

#include "action-container.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class OCTAVE_API unwind_protect : public action_container
{
public:

  unwind_protect () : m_lifo () { }

  OCTAVE_DISABLE_COPY_MOVE (unwind_protect)

  // Destructor should not raise an exception, so all actions
  // registered should be exception-safe.  If you're not sure, see
  // unwind_protect_safe.

  ~unwind_protect () { run (); }

  operator bool () const { return ! empty (); }

  void run_first ()
  {
    if (! empty ())
      {
        // No leak on exception!
        std::unique_ptr<elem> ptr (m_lifo.top ());
        m_lifo.pop ();
        ptr->run ();
      }
  }

  void discard_first ()
  {
    if (! empty ())
      {
        elem *ptr = m_lifo.top ();
        m_lifo.pop ();
        delete ptr;
      }
  }

  std::size_t size () const { return m_lifo.size (); }

protected:

  virtual void add_action (elem *new_elem)
  {
    m_lifo.push (new_elem);
  }

  std::stack<elem *> m_lifo;
};

// Like unwind_protect, but this one will guard against the possibility
// of seeing an exception (or interrupt) in the cleanup actions.
// Not that we can do much about it, but at least we won't crash.

class OCTAVE_API unwind_protect_safe : public unwind_protect
{
private:

  void warn_unhandled_exception () const;

public:

  unwind_protect_safe () : unwind_protect () { }

  OCTAVE_DISABLE_COPY_MOVE (unwind_protect_safe)

  ~unwind_protect_safe ()
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

// In most cases, the following are preferred for efficiency.  Some
// cases may require the flexibility of the general unwind_protect
// mechanism defined above.

// Perform action at end of the current scope when unwind_action
// object destructor is called.
//
// For example:
//
//   void fcn (int val) { ... }
//
// ...
//
//   {
//     int val = 42;
//
//     // template parameters, std::bind and std::function provide
//     // flexibility in calling forms (function pointer or lambda):
//
//     unwind_action act1 (fcn, val);
//     unwind_action act2 ([val] () { fcn (val); });
//   }
//
// NOTE: Don't forget to provide a name for the unwind_action
// variable.  If you write
//
//   unwind_action /* NO NAME! */ (...);
//
// then the destructor for the temporary anonymous object will be
// called immediately after the object is constructed instead of at
// the end of the current scope.

class OCTAVE_API unwind_action
{
public:

  unwind_action () : m_fcn () { }

  // FIXME: Do we need to apply std::forward to the arguments to
  // std::bind here?

  template <typename F, typename... Args>
  unwind_action (F&& fcn, Args&& ... args)
    : m_fcn (std::bind (fcn, args...))
  { }

  OCTAVE_DISABLE_COPY_MOVE (unwind_action)

  ~unwind_action () { run (); }

  // FIXME: Do we need to apply std::forward to the arguments to
  // std::bind here?

  template <typename F, typename... Args>
  void set (F&& fcn, Args&& ... args)
  {
    m_fcn = std::bind (fcn, args...);
  }

  void set () { m_fcn = nullptr; }

  // Alias for set() which is clearer about programmer intention.
  void discard () { set (); }

  void run ()
  {
    if (m_fcn)
      m_fcn ();

    // Invalidate so action won't run again when object is deleted.
    discard ();
  }

private:

  std::function<void ()> m_fcn;
};

// Like unwind_action, but this one will guard against the possibility
// of seeing an exception (or interrupt) in the cleanup actions.
// Not that we can do much about it, but at least we won't crash.

class OCTAVE_API unwind_action_safe
{
private:

  void warn_unhandled_exception () const;

public:

  unwind_action_safe () : m_fcn () { }

  // FIXME: Do we need to apply std::forward to the arguments to
  // std::bind here?

  template <typename F, typename... Args>
  unwind_action_safe (F&& fcn, Args&& ... args)
    : m_fcn (std::bind (fcn, args...))
  { }

  OCTAVE_DISABLE_COPY_MOVE (unwind_action_safe)

  ~unwind_action_safe () { run (); }

  // FIXME: Do we need to apply std::forward to the arguments to
  // std::bind here?

  template <typename F, typename... Args>
  void set (F&& fcn, Args&& ... args)
  {
    m_fcn = std::bind (fcn, args...);
  }

  void set () { m_fcn = nullptr; }

  // Alias for set() which is clearer about programmer intention.
  void discard () { set (); }

  void run ()
  {
    try
      {
        if (m_fcn)
          m_fcn ();
      }
    catch (...) // Yes, the black hole.  Remember we're in a destructor.
      {
        warn_unhandled_exception ();
      }

    // Invalidate so action won't run again when object is deleted.
    discard ();
  }

private:

  std::function<void ()> m_fcn;
};

// Reset a variable value at the end of the current scope when
// unwind_protect_var object destructor is called.
//
// For example:
//
//   {
//     int x = 42;
//     unwind_protect_var<int> upv (x);  // X will be reset at end of scope
//     x = 13;                           // Set temporary value.
//   }
//
// Temporary value may be set at construction:
//
//   {
//     int x = ...;
//     unwind_protect_var<int> upv (x, 13);  // X will be reset.
//                                           // temporary value is 13.
//   }
//
// NOTE: Don't forget to provide a name for the unwind_protect_var
// variable.  If you write
//
//   unwind_protect_var<type> /* NO NAME! */ (...);
//
// then the destructor for the temporary anonymous object will be
// called immediately after the object is constructed instead of at
// the end of the current scope.
//
// FIXME: Once we are able to use C++17, class template argument
// deduction will allow us to omit the explicit template type from the
// constructor expression:
//
//   unwind_protect_var upv (...);

template <typename T>
class unwind_protect_var
{
public:

  // Ensure that the value referenced by REF will be reset when this
  // unwind_protect_var object goes out of scope.

  explicit unwind_protect_var (T& ref)
    : m_ref (ref), m_val (ref)
  { }

  // Set the value referenced by REF to NEW_VAL and ensure that it
  // will be reset to its original value when this
  // unwind_protect_var object goes out of scope.

  unwind_protect_var (T& ref, const T& new_val)
    : m_ref (ref), m_val (ref)
  {
    m_ref = new_val;
  }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (unwind_protect_var)

  ~unwind_protect_var ()
  {
    m_ref = m_val;
  }

private:

  T& m_ref;
  T m_val;
};

OCTAVE_END_NAMESPACE(octave)

#endif
