// unwind-prot.h                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_unwind_prot_h)
#define octave_unwind_prot_h 1

#if defined (__GNUG__)
#pragma interface
#endif

typedef void (*cleanup_func)(void *ptr);

void add_unwind_protect (cleanup_func fptr, void *ptr);
void run_unwind_protect (void);
void discard_unwind_protect (void);
void begin_unwind_frame (char *tag);
void run_unwind_frame (char *tag);
void discard_unwind_frame (char *tag);
void run_all_unwind_protects (void);
void discard_all_unwind_protects (void);

void matrix_cleanup (void *m);
void complex_matrix_cleanup (void *cm);

void unwind_protect_int_internal (int *ptr, int value);
void unwind_protect_ptr_internal (void **ptr, void *value);
void unwind_protect_var_internal (void *ptr, void *value, size_t size);

#define unwind_protect_int(i) \
  unwind_protect_int_internal (&(i), (i))

#define unwind_protect_ptr(p) \
  unwind_protect_ptr_internal ((void **) &(p), (void *) (p))

#define unwind_protect_var(i) \
  unwind_protect_var_internal ((void *) &(i), (void *) &(i), sizeof (int))

class
unwind_elem
{
 public:
  unwind_elem (void);
  unwind_elem (char *t);
  unwind_elem (cleanup_func f, void *p);
  unwind_elem (const unwind_elem& el);
  ~unwind_elem (void);

  unwind_elem& operator = (const unwind_elem& el);

  char *tag (void);
  cleanup_func fptr (void);
  void *ptr (void);

 private:
  char *unwind_elem_tag;
  cleanup_func unwind_elem_fptr;
  void *unwind_elem_ptr;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
