// Template stack class                                  -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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

// Maybe this will work with 2.6.x and beyond?

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined (__GNUG__) && defined (USE_EXTERNAL_TEMPLATES)
#pragma implementation
#endif

#include <iostream.h>

#include "Stack.h"

template <class T>
void
Stack<T>::error (const char *msg)
{
  cerr << msg;
}

#ifdef __GNUG__
#if defined (OCTAVE_SOURCE) && defined (USE_EXTERNAL_TEMPLATES)

typedef Stack<int> stack_type_int;
typedef Stack<char *> stack_type_p_char;

#include "symtab.h"
typedef Stack<symbol_def *> stack_type_p_symbol_def;

#include "token.h"
typedef Stack<token *> stack_type_p_token;

#include "tree.h"
typedef Stack<tree_matrix *> stack_type_p_tree_matrix;

#include "unwind-prot.h"
typedef Stack<unwind_elem> stack_type_unwind_elem;

#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
