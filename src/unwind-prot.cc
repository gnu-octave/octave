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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstddef>

#include <string>

#include "SLStack.h"

#include "CMatrix.h"

#include "error.h"
#include "unwind-prot.h"
#include "utils.h"

// XXX FIXME XXX -- this should really be static, but that causes
// problems on some systems.
SLStack <unwind_elem> unwind_protect_list;

void
add_unwind_protect (cleanup_func fptr, void *ptr)
{
  unwind_elem el (fptr, ptr);
  unwind_protect_list.push (el);
}

void
run_unwind_protect (void)
{
  unwind_elem el = unwind_protect_list.pop ();

  cleanup_func f = el.fptr ();

  if (f)
    f (el.ptr ());
}

void
discard_unwind_protect (void)
{
  unwind_protect_list.pop ();
}

void
begin_unwind_frame (const string& tag)
{
  unwind_elem elem (tag);
  unwind_protect_list.push (elem);
}

void
run_unwind_frame (const string& tag)
{
  while (! unwind_protect_list.empty ())
    {
      unwind_elem el = unwind_protect_list.pop ();

      cleanup_func f = el.fptr ();

      if (f)
	f (el.ptr ());

      if (tag == el.tag ())
	break;
    }
}

void
discard_unwind_frame (const string& tag)
{
  while (! unwind_protect_list.empty ())
    {
      unwind_elem el = unwind_protect_list.pop ();

      if (tag == el.tag ())
	break;
    }
}

void
run_all_unwind_protects (void)
{
  while (! unwind_protect_list.empty ())
    {
      unwind_elem el = unwind_protect_list.pop ();

      cleanup_func f = el.fptr ();

      if (f)
	f (el.ptr ());
    }
}

void
discard_all_unwind_protects (void)
{
  unwind_protect_list.clear ();
}

class saved_variable
{
 public:
  enum var_type { integer, string_type, generic_ptr, generic };

  saved_variable (void);
  saved_variable (int *p, int v);
  saved_variable (string *p, const string& v);
  saved_variable (void **p, void *v);
  ~saved_variable (void);

  void restore_value (void);

 private:
  union
    {
      int *ptr_to_int;
      void *gen_ptr;
      void **ptr_to_gen_ptr;
    };

  union
    {
      int int_value;
      const string *str_value;
      void *gen_ptr_value;
    };

  var_type type_tag;
  size_t size;
};

saved_variable::saved_variable (void)
{
  gen_ptr = 0;
  gen_ptr_value = 0;
  type_tag = generic;
  size = 0;
}

saved_variable::saved_variable (int *p, int v)
{
  type_tag = integer;
  ptr_to_int = p;
  int_value = v;
  size = sizeof (int);  // Is this necessary?
}

saved_variable::saved_variable (string *p, const string& v)
{
  type_tag = string_type;
  gen_ptr = p;
  str_value = new string (v);
  size = sizeof (string);  // Is this necessary?
}

saved_variable::saved_variable (void **p, void *v)
{
  type_tag = generic_ptr;
  ptr_to_gen_ptr = p;
  gen_ptr_value = v;
  size = sizeof (void *);
}

saved_variable::~saved_variable (void)
{
  switch (type_tag)
    {
    case string_type:
      delete str_value;
      break;

    case generic:
      delete [] gen_ptr_value;  // Can this be right?
      break;

    default:
      break;
    }
}

void
saved_variable::restore_value (void)
{
  switch (type_tag)
    {
    case integer:
      *ptr_to_int = int_value;
      break;

    case string_type:
      (static_cast<string *> (gen_ptr)) -> assign (*str_value);
      break;

    case generic_ptr:
      *ptr_to_gen_ptr = gen_ptr_value;
      break;

    case generic:
      memcpy (gen_ptr, gen_ptr_value, size);
      break;

    default:
      panic_impossible ();
      break;
    }
}

static void
restore_saved_variable (void *s)
{
  saved_variable *sv = static_cast<saved_variable *> (s);
  sv->restore_value ();
  delete sv;
}

void
unwind_protect_int_internal (int *ptr, int value)
{
  saved_variable *s = new saved_variable (ptr, value);
  add_unwind_protect (restore_saved_variable, s);
}

void
unwind_protect_str_internal (string *ptr, const string& value)
{
  saved_variable *s = new saved_variable (ptr, value);
  add_unwind_protect (restore_saved_variable, s);
}

void
unwind_protect_ptr_internal (void **ptr, void *value)
{
  saved_variable *s = new saved_variable (ptr, value);
  add_unwind_protect (restore_saved_variable, s);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
