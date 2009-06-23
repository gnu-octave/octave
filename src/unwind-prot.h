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
#include <utility>

class
OCTINTERP_API
unwind_protect
{
public:

  // This template class can be used to restore value of a variable of any
  // class posessing a copy constructor and assignment operator.

  template <class T>
  class
  restore_var
  {
  public:
    restore_var (T *ptr, const T& val) 
      : rptr (ptr), rval(val) { }
    restore_var (T *ptr) 
      : rptr (ptr), rval(*ptr) { }
    ~restore_var (void)
      { *rptr = rval; }

    // For unwind_protect.
    static void cleanup (void *ptr)
      {
        delete reinterpret_cast<restore_var *> (ptr);
      }

  private:

    // No copying!
    void operator = (const restore_var&); 

    T *rptr, rval;
  };

  // This class is used to restore arbitrary memory area using std::memcpy.

  class
  restore_mem
  {
  public:
    restore_mem (void *ptr, size_t size);
    ~restore_mem (void);

    // For unwind_protect.
    static void cleanup (void *ptr)
      {
        delete reinterpret_cast<restore_mem *> (ptr);
      }

  private:

    // No copying!
    void operator = (const restore_mem&); 

    void *rptr, *sptr;
    size_t rsize;
  };

  typedef void (*cleanup_func) (void *ptr);

  typedef size_t frame_id_t;

  typedef std::pair<cleanup_func, void *> elem;

  static bool empty (void)
    { return elt_list.empty (); }

  static void add (cleanup_func fptr, void *ptr = 0)
    {
      elt_list.push (elem (fptr, ptr));
    }

  static void run (void)
    {
      elem elt = elt_list.top ();
      elt_list.pop ();

      elt.first (elt.second);
    }

  static void discard (void)
    {
      elt_list.pop ();
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

  // Protect any variable.
  template <class T>
  static void protect_var (T& var)
    {
      add (restore_var<T>::cleanup, new restore_var<T> (&var));
    }

  // Protect any variable, value given.
  template <class T>
  static void protect_var (T& var, const T& val)
    {
      add (restore_var<T>::cleanup, new restore_var<T> (&var, val));
    }

  // Protect an area of memory.
  static void protect_mem (void *ptr, size_t size)
    {
      add (restore_mem::cleanup, new restore_mem (ptr, size));
    }

private:

  static std::stack<elem> elt_list;

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
