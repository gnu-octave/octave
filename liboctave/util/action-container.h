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

#if ! defined (octave_action_container_h)
#define octave_action_container_h 1

#include "octave-config.h"

#include <functional>

// This class allows registering actions in a list for later
// execution, either explicitly or when the container goes out of
// scope.

// FIXME: is there a better name for this class?

namespace octave
{
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

      elem (void) { }

      // No copying!

      elem (const elem&) = delete;

      elem& operator = (const elem&) = delete;

      virtual ~elem (void) = default;

      virtual void run (void) { }
    };

    // An element that merely runs a void (*)(void) function.

    class fcn_elem : public elem
    {
    public:

      template <typename F, typename... Args>
      fcn_elem (F&& fcn, Args&&... args)
        : m_fcn (std::bind (fcn, args...))
      { }

      void run (void) { m_fcn (); }

    private:

      std::function<void (void)> m_fcn;
    };

    // An element that stores arbitrary variable, and restores it.

    template <typename T>
    class restore_var_elem : public elem
    {
    public:

      restore_var_elem (T& ref, const T& val)
        : e_ptr (&ref), e_val (val) { }

      // No copying!

      restore_var_elem (const restore_var_elem&) = delete;

      restore_var_elem& operator = (const restore_var_elem&) = delete;

      void run (void) { *e_ptr = e_val; }

    private:

      T *e_ptr, e_val;
    };

    // Deletes a class allocated using new.

    template <typename T>
    class delete_ptr_elem : public elem
    {
    public:

      delete_ptr_elem (T *ptr)
        : e_ptr (ptr) { }

      // No copying!

      delete_ptr_elem (const delete_ptr_elem&) = delete;

      delete_ptr_elem operator = (const delete_ptr_elem&) = delete;

      void run (void) { delete e_ptr; }

    private:

      T *e_ptr;
    };

    action_container (void) { }

    // No copying!

    action_container (const action_container&) = delete;

    action_container& operator = (const action_container&) = delete;

    virtual ~action_container (void) = default;

    template <typename F, typename... Args>
    void add (F&& fcn, Args&&... args)
    {
      add_action (new fcn_elem (std::forward<F> (fcn),
                                std::forward<Args> (args)...));
    }

    // Use separate template types for function pointer parameter
    // declarations and captured arguments so that differences in
    // const are handled properly.

    template <typename... Params, typename... Args>
    void add_fcn (void (*fcn) (Params...), Args&&... args)
    {
      add_action (new fcn_elem (fcn, std::forward<Args> (args)...));
    }

    template <typename T, typename... Params, typename... Args>
    void add_method (T *obj, void (T::*method) (Params...), Args&&... args)
    {
      add_action (new fcn_elem (method, obj, std::forward<Args> (args)...));
    }

    template <typename T, typename... Params, typename... Args>
    void add_method (T& obj, void (T::*method) (Params...), Args&&... args)
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

    operator bool (void) const { return ! empty (); }

    virtual void run_first (void) = 0;

    void run (size_t num)
    {
      if (num > size ())
        num = size ();

      for (size_t i = 0; i < num; i++)
        run_first ();
    }

    void run (void) { run (size ()); }

    virtual void discard_first (void) = 0;

    void discard (size_t num)
    {
      if (num > size ())
        num = size ();

      for (size_t i = 0; i < num; i++)
        discard_first ();
    }

    void discard (void) { discard (size ()); }

    virtual size_t size (void) const = 0;

    bool empty (void) const { return size () == 0; }

  protected:

    virtual void add_action (elem *new_elem) = 0;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::action_container' instead")
typedef octave::action_container action_container;

#endif

#endif
