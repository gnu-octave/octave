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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_unwind_prot_h)
#define octave_unwind_prot_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstddef>

#include <string>

typedef void (*cleanup_func)(void *ptr);

void add_unwind_protect (cleanup_func fptr, void *ptr);
void run_unwind_protect (void);
void discard_unwind_protect (void);
void begin_unwind_frame (const string& tag);
void run_unwind_frame (const string& tag);
void discard_unwind_frame (const string& tag);
void run_all_unwind_protects (void);
void discard_all_unwind_protects (void);

void unwind_protect_int_internal (int *ptr, int value);
void unwind_protect_str_internal (string *ptr, const string& value);
void unwind_protect_ptr_internal (void **ptr, void *value);
void unwind_protect_var_internal (void *ptr, void *value, size_t size);

#define unwind_protect_int(i) \
  unwind_protect_int_internal (&(i), (i))

#define unwind_protect_str(s) \
  unwind_protect_str_internal (&(s), (s))

#define unwind_protect_ptr(p) \
  unwind_protect_ptr_internal (static_cast<void **> (&(p)), \
			       static_cast<void *> (p))

class
unwind_elem
{
 public:
  unwind_elem (void)
    : ue_tag (), ue_fptr (0), ue_ptr (0) { }

  unwind_elem (const string &t)
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

  string tag (void) { return ue_tag; }

  cleanup_func fptr (void) { return ue_fptr; }

  void *ptr (void) { return ue_ptr; }

 private:
  string ue_tag;
  cleanup_func ue_fptr;
  void *ue_ptr;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
