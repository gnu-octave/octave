////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_action_container_h)
#define octave_action_container_h 1

#include "octave-config.h"

#include <atomic>
#include <cstddef>
#include <functional>

// This class allows registering actions in a list for later
// execution, either explicitly or when the container goes out of
// scope.

// FIXME: is there a better name for this class?

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(util)

template <typename T>
struct atomic_traits
{
  typedef T type;
};

template <typename T>
struct atomic_traits<std::atomic<T>>
{
  typedef T type;
};

OCTAVE_END_NAMESPACE(util)

class
action_container
{
public:

  // A generic unwind_protect element.  Knows how to run itself and
  // discard itself.  Also, contains a pointer to the next element.
  class elem
  {
  public:

    friend class action_container;

    elem () { }

    OCTAVE_DISABLE_COPY_MOVE (elem)

    virtual ~elem () = default;

    virtual void run () { }
  };

  // An element that merely runs a void (*)() function.

  class fcn_elem : public elem
  {
  public:

    // FIXME: Do we need to apply std::forward to the arguments to
    // std::bind here?

    template <typename F, typename... Args>
    fcn_elem (F&& fcn, Args&& ... args)
      : m_fcn (std::bind (fcn, args...))
    { }

    void run () { m_fcn (); }

  private:

    std::function<void ()> m_fcn;
  };

  // An element that stores arbitrary variable, and restores it.

  template <typename T>
  class restore_var_elem : public elem
  {
  public:

    restore_var_elem (T& ref, const T& val)
      : m_ptr (&ref), m_val (val) { }

    OCTAVE_DISABLE_COPY_MOVE (restore_var_elem)

    void run () { *m_ptr = m_val; }

  private:

    T *m_ptr;
    typename util::atomic_traits<T>::type m_val;
  };

  // Deletes a class allocated using new.

  template <typename T>
  class delete_ptr_elem : public elem
  {
  public:

    delete_ptr_elem (T *ptr)
      : m_ptr (ptr) { }

    OCTAVE_DISABLE_COPY_MOVE (delete_ptr_elem)

    void run () { delete m_ptr; }

  private:

    T *m_ptr;
  };

  action_container () { }

  OCTAVE_DISABLE_COPY_MOVE (action_container)

  virtual ~action_container () = default;

  template <typename F, typename... Args>
  void add (F&& fcn, Args&& ... args)
  {
    add_action (new fcn_elem (std::forward<F> (fcn),
                              std::forward<Args> (args)...));
  }

  // Use separate template types for function pointer parameter
  // declarations and captured arguments so that differences in
  // const are handled properly.

  template <typename... Params, typename... Args>
  void add_fcn (void (*fcn) (Params...), Args&& ... args)
  {
    add_action (new fcn_elem (fcn, std::forward<Args> (args)...));
  }

  template <typename T, typename... Params, typename... Args>
  void add_method (T *obj, void (T::*method) (Params...), Args&& ... args)
  {
    add_action (new fcn_elem (method, obj, std::forward<Args> (args)...));
  }

  template <typename T, typename... Params, typename... Args>
  void add_method (T& obj, void (T::*method) (Params...), Args&& ... args)
  {
    add_action (new fcn_elem (method, &obj, std::forward<Args> (args)...));
  }

  // Call to delete (T*).

  template <typename T>
  void add_delete (T *obj)
  {
    add_action (new delete_ptr_elem<T> (obj));
  }

  // Protect any variable.
  template <typename T>
  void protect_var (T& var)
  {
    add_action (new restore_var_elem<T> (var, var));
  }

  // Protect any variable, value given.
  template <typename T>
  void protect_var (T& var, const T& val)
  {
    add_action (new restore_var_elem<T> (var, val));
  }

  operator bool () const { return ! empty (); }

  virtual void run_first () = 0;

  OCTAVE_API void run (std::size_t num);

  void run () { run (size ()); }

  virtual void discard_first () = 0;

  void discard (std::size_t num)
  {
    if (num > size ())
      num = size ();

    for (std::size_t i = 0; i < num; i++)
      discard_first ();
  }

  void discard () { discard (size ()); }

  virtual std::size_t size () const = 0;

  bool empty () const { return size () == 0; }

protected:

  virtual void add_action (elem *new_elem) = 0;
};

OCTAVE_END_NAMESPACE(octave)

#endif
