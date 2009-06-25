/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2002, 2004,
              2005, 2006, 2007, 2008 John W. Eaton
Copyright (C) 2009 VZLU Prague

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
#include <stack>
#include <memory>

class
OCTINTERP_API
unwind_protect
{
public:

  // A generic unwind_protect element. Knows how to run itself and discard itself.
  class elem
  {
  public:
    virtual void run (void) { }
    virtual ~elem (void) { }
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

  typedef size_t frame_id_t;

  // Generic. Users may subclass elem to provide their own cleanup.
  static void add (elem *el)
    {
      elt_list.push (el);
    }

  static bool empty (void)
    { return elt_list.empty (); }

  static void run (void)
    {
      // Use auto_ptr, so that even if the following run () call throws an
      // exception, we still clean up the element.
      std::auto_ptr<elem> elt (elt_list.top ());
      elt_list.pop ();

      elt->run ();
    }

  static void discard (void)
    {
      // No need to use ato_ptr here.
      elem *elt = elt_list.top ();
      elt_list.pop ();

      delete elt;
    }

  static frame_id_t begin_frame ()
    {
      return elt_list.size ();
    }

  static void run_frame (frame_id_t frame_id)
    {
      while (elt_list.size () > frame_id)
        run ();
    }

  static void discard_frame (frame_id_t frame_id)
    {
      while (elt_list.size () > frame_id)
        discard ();
    }

  // String tags are deprecated. Use the above trio.

  static void begin_frame (const std::string& tag) GCC_ATTR_DEPRECATED;

  static void run_frame (const std::string& tag) GCC_ATTR_DEPRECATED;

  static void discard_frame (const std::string& tag) GCC_ATTR_DEPRECATED;

  static void run_all (void)
    { 
      run_frame (0);
      while (! tag_list.empty ())
        tag_list.pop ();
    }

  static void discard_all (void)
    { 
      discard_frame (0);
      while (! tag_list.empty ())
        tag_list.pop ();
    }

  // For backward compatibility.
  static void add (void (*fcn) (void *), void *ptr = 0)
    {
      elt_list.push (new fcn_arg_elem<void *> (fcn, ptr));
    }

  // Call to void func (void).
  static void add_fcn (void (*fcn) (void))
    {
      elt_list.push (new fcn_elem (fcn));
    }

  // Call to void func (T).
  template <class T>
  static void add_fcn (void (*action) (T), T val)
    {
      elt_list.push (new fcn_arg_elem<T> (action, val));
    }

  // Call to T::method (void).
  template <class T>
  static void add_method (T *obj, void (T::*method) (void))
    {
      elt_list.push (new method_elem<T> (obj, method));
    }

  // Call to delete (T*).

  template <class T>
  static void add_delete (T *obj)
    {
      elt_list.push (new delete_ptr_elem<T> (obj));
    }

  // Protect any variable.
  template <class T>
  static void protect_var (T& var)
    {
      elt_list.push (new restore_var_elem<T> (var, var));
    }

  // Protect any variable, value given.
  template <class T>
  static void protect_var (T& var, const T& val)
    {
      elt_list.push (new restore_var_elem<T> (var, val));
    }

private:

  static std::stack<elem *> elt_list;

  static std::stack<std::pair <std::string, frame_id_t> > tag_list;
};

// Backward compatibility macros. Avoid them; use protect_var directly.

#define unwind_protect_bool(b) \
  unwind_protect::protect_var (b)

#define unwind_protect_int(i) \
  unwind_protect::protect_var (i)

#define unwind_protect_size_t(i) \
  unwind_protect::protect_var (i)

#define unwind_protect_str(s) \
  unwind_protect::protect_var (s)

#define unwind_protect_ptr(p) \
  unwind_protect::protect_var (p)

#define unwind_protect_fptr(p) \
  unwind_protect::protect_var (p)

#define unwind_protect_const_ptr(p) \
  unwind_protect::protect_var (p)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
