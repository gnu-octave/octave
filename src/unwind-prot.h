/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2002, 2004,
              2005, 2006, 2007, 2008 John W. Eaton
Copyright (C) 2009, 2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_unwind_prot_h)
#define octave_unwind_prot_h 1

#include <cstddef>

#include <string>
#include <memory>

// This class allows registering cleanup actions.
class
OCTINTERP_API
unwind_protect
{
public:

  // A generic unwind_protect element. Knows how to run itself and discard itself.
  // Also, contains a pointer to the next element.
  class elem
  {
    elem *next;

  public:
    virtual void run (void) { }
    virtual ~elem (void) { }

    friend class unwind_protect;
  };

  // An element that merely runs a void (*)(void) function.
  
  class fcn_elem : public elem
  {
  public:
    fcn_elem (void (*fptr) (void))
      : e_fptr (fptr) { }

    void run (void) { e_fptr (); }

  private:
    void (*e_fptr) (void);
  };

  // An element that stores a variable of type T along with a void (*) (T)
  // function pointer, and calls the function with the parameter.

  template <class T>
  class fcn_arg_elem : public elem
  {
  public:
    fcn_arg_elem (void (*fcn) (T), T arg)
      : e_fcn (fcn), e_arg (arg) { }

    void run (void) { e_fcn (e_arg); }

  private:
    void (*e_fcn) (T);
    T e_arg;
  };

  // An element for calling a member function.

  template <class T>
  class method_elem : public elem
  {
  public:
    method_elem (T *obj, void (T::*method) (void))
      : e_obj (obj), e_method (method) { }

    void run (void) { (e_obj->*e_method) (); }

  private:
    T *e_obj;
    void (T::*e_method) (void);
  };

  // An element that stores arbitrary variable, and restores it.

  template <class T>
  class restore_var_elem : public elem
  {
  public:
    restore_var_elem (T& ref, const T& val)
      : e_ptr (&ref), e_val (val) { }

    void run (void) { *e_ptr = e_val; }

  private:
    T *e_ptr, e_val;
  };

  // Deletes a class allocated using new.

  template <class T>
  class delete_ptr_elem : public elem
  {
  public:
    delete_ptr_elem (T *ptr)
      : e_ptr (ptr) { }

    void run (void) { delete e_ptr; }

  private:
    T *e_ptr;
  };

  unwind_protect (void) : head () { }

  void add (elem *new_elem)
    {
      new_elem->next = head;
      head = new_elem;
    }

  // For backward compatibility.
  void add (void (*fcn) (void *), void *ptr = 0)
    {
      add (new fcn_arg_elem<void *> (fcn, ptr));
    }

  // Call to void func (void).
  void add_fcn (void (*fcn) (void))
    {
      add (new fcn_elem (fcn));
    }

  // Call to void func (T).
  template <class T>
  void add_fcn (void (*action) (T), T val)
    {
      add (new fcn_arg_elem<T> (action, val));
    }

  // Call to T::method (void).
  template <class T>
  void add_method (T *obj, void (T::*method) (void))
    {
      add (new method_elem<T> (obj, method));
    }

  // Call to delete (T*).

  template <class T>
  void add_delete (T *obj)
    {
      add (new delete_ptr_elem<T> (obj));
    }

  // Protect any variable.
  template <class T>
  void protect_var (T& var)
    {
      add (new restore_var_elem<T> (var, var));
    }

  // Protect any variable, value given.
  template <class T>
  void protect_var (T& var, const T& val)
    {
      add (new restore_var_elem<T> (var, val));
    }

  operator bool (void) const 
    { 
      return head != 0; 
    }

  void run_top (void) 
    { 
      if (head)
        {
          // No leak on exception!
          std::auto_ptr<elem> ptr (head);
          head = ptr->next;
          ptr->run ();
        }
    }

  void run_top (int num) 
    { 
      while (num-- > 0)
        run_top ();
    }

  void discard_top (void)
    {
      if (head)
        {
          elem *ptr = head;
          head = ptr->next;
          delete ptr;
        }
    }

  void discard_top (int num) 
    { 
      while (num-- > 0)
        discard_top ();
    }

  void run (void)
    {
      while (head)
        run_top ();
    }

  void discard (void)
    {
      while (head)
        discard_top ();
    }

  // Destructor should not raise an exception, so all actions registered should
  // be exception-safe (but setting error_state is allowed). If you're not sure,
  // see unwind_protect_safe.
  ~unwind_protect (void)
    {
      run ();
    }

private:

  elem *head;
};

// Like unwind_protect, but this one will guard against the possibility of seeing
// an exception (or interrupt) in the cleanup actions. Not that we can do much about
// it, but at least we won't crash.

class
OCTINTERP_API
unwind_protect_safe : public unwind_protect
{
  static void gripe_exception (void);

public:
  ~unwind_protect_safe (void)
    {
      while (*this)
        {
          try
            {
              run_top ();
            }
          catch (...) // Yes, the black hole. Remember we're in a dtor.
            {
              gripe_exception ();
            }
        }
    }
};

#endif
