/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_unwind_prot_h)
#define octave_unwind_prot_h 1

#include <cstddef>

#include <string>
#include <stack>

class
unwind_elem
{
public:

  typedef void (*cleanup_func) (void *ptr);

  unwind_elem (void)
    : ue_tag (), ue_fptr (0), ue_ptr (0) { }

  unwind_elem (const std::string &t)
    : ue_tag (t), ue_fptr (0), ue_ptr (0) { }

  unwind_elem (cleanup_func f, void *p)
    : ue_tag (), ue_fptr (f), ue_ptr (p) { }

  unwind_elem (const unwind_elem& el)
    : ue_tag (el.ue_tag), ue_fptr (el.ue_fptr), ue_ptr (el.ue_ptr) { }

  ~unwind_elem (void) { }

  unwind_elem& operator = (const unwind_elem& el)
    {
      ue_tag = el.ue_tag;
      ue_fptr = el.ue_fptr;
      ue_ptr = el.ue_ptr;

      return *this;
    }

  std::string tag (void) { return ue_tag; }

  cleanup_func fptr (void) { return ue_fptr; }

  void *ptr (void) { return ue_ptr; }

private:

  std::string ue_tag;

  cleanup_func ue_fptr;

  void *ue_ptr;
};

class
unwind_protect
{
public:

  static void add (unwind_elem::cleanup_func fptr, void *ptr);

  static void run (void);

  static void discard (void);

  static void begin_frame (const std::string& tag);

  static void run_frame (const std::string& tag);

  static void discard_frame (const std::string& tag);

  static void run_all (void);

  static void discard_all (void);

  // Ways to save variables.

  static void save_bool (bool *ptr, bool value);

  static void save_int (int *ptr, int value);

  static void save_str (std::string *ptr, const std::string& value);

  static void save_ptr (void **ptr, void *value);

  static void save_var (void *ptr, void *value, size_t size);

  static std::stack<unwind_elem> elt_list;
};

// We could get by without these macros, but they are nice to have...

#define unwind_protect_bool(b) \
  unwind_protect::save_bool (&(b), (b))

#define unwind_protect_int(i) \
  unwind_protect::save_int (&(i), (i))

#define unwind_protect_str(s) \
  unwind_protect::save_str (&(s), (s))

#define unwind_protect_ptr(p) \
  unwind_protect::save_ptr (X_CAST (void **, &(p)), X_CAST (void *, (p)))

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
