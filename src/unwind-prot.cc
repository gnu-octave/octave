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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstddef>

#include "CMatrix.h"

#include "error.h"
#include "unwind-prot.h"
#include "utils.h"

std::stack<unwind_elem> unwind_protect::elt_list;

class
saved_variable
{
public:

  enum var_type
  {
    boolean,
    integer,
    string_type,
    generic_ptr,
    generic
  };

  saved_variable (void);

  saved_variable (bool *p, bool v);

  saved_variable (int *p, int v);

  saved_variable (std::string *p, const std::string& v);

  saved_variable (void **p, void *v);

  ~saved_variable (void);

  void restore_value (void);

  static void restore (void *s);

private:

  union
    {
      bool *ptr_to_bool;
      int *ptr_to_int;
      void *gen_ptr;
      void **ptr_to_gen_ptr;
    };

  union
    {
      bool bool_value;
      int int_value;
      const std::string *str_value;
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

saved_variable::saved_variable (bool *p, bool v)
{
  type_tag = boolean;
  ptr_to_bool = p;
  bool_value = v;
  size = sizeof (bool);  // Is this necessary?
}

saved_variable::saved_variable (int *p, int v)
{
  type_tag = integer;
  ptr_to_int = p;
  int_value = v;
  size = sizeof (int);  // Is this necessary?
}

saved_variable::saved_variable (std::string *p, const std::string& v)
{
  type_tag = string_type;
  gen_ptr = p;
  str_value = new std::string (v);
  size = sizeof (std::string);  // Is this necessary?
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
      // XXX FIXME XXX
      // delete [] gen_ptr_value;
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
    case boolean:
      *ptr_to_bool = bool_value;
      break;

    case integer:
      *ptr_to_int = int_value;
      break;

    case string_type:
      (static_cast<std::string *> (gen_ptr)) -> assign (*str_value);
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

void
saved_variable::restore (void *s)
{
  saved_variable *sv = static_cast<saved_variable *> (s);
  sv->restore_value ();
  delete sv;
}

void
unwind_protect::add (unwind_elem::cleanup_func fptr, void *ptr)
{
  unwind_elem el (fptr, ptr);
  elt_list.push (el);
}

void
unwind_protect::run (void)
{
  unwind_elem el = elt_list.top ();

  elt_list.pop ();

  unwind_elem::cleanup_func f = el.fptr ();

  if (f)
    f (el.ptr ());
}

void
unwind_protect::discard (void)
{
  elt_list.pop ();
}

void
unwind_protect::begin_frame (const std::string& tag)
{
  unwind_elem elem (tag);
  elt_list.push (elem);
}

void
unwind_protect::run_frame (const std::string& tag)
{
  while (! elt_list.empty ())
    {
      unwind_elem el = elt_list.top ();

      elt_list.pop ();

      unwind_elem::cleanup_func f = el.fptr ();

      if (f)
	f (el.ptr ());

      if (tag == el.tag ())
	break;
    }
}

void
unwind_protect::discard_frame (const std::string& tag)
{
  while (! elt_list.empty ())
    {
      unwind_elem el = elt_list.top ();

      elt_list.pop ();

      if (tag == el.tag ())
	break;
    }
}

void
unwind_protect::run_all (void)
{
  while (! elt_list.empty ())
    {
      unwind_elem el = elt_list.top ();

      elt_list.pop ();

      unwind_elem::cleanup_func f = el.fptr ();

      if (f)
	f (el.ptr ());
    }
}

void
unwind_protect::discard_all (void)
{
  while (! elt_list.empty ())
    elt_list.pop ();
}

void
unwind_protect::save_bool (bool *ptr, bool value)
{
  saved_variable *s = new saved_variable (ptr, value);
  add (saved_variable::restore, s);
}

void
unwind_protect::save_int (int *ptr, int value)
{
  saved_variable *s = new saved_variable (ptr, value);
  add (saved_variable::restore, s);
}

void
unwind_protect::save_str (std::string *ptr, const std::string& value)
{
  saved_variable *s = new saved_variable (ptr, value);
  add (saved_variable::restore, s);
}

void
unwind_protect::save_ptr (void **ptr, void *value)
{
  saved_variable *s = new saved_variable (ptr, value);
  add (saved_variable::restore, s);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
