/*

Copyright (C) 2003 John W. Eaton

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

#if !defined (octave_fcn_handle_h)
#define octave_fcn_handle_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <iostream>
#include <string>

#include "oct-alloc.h"

#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"
#include "symtab.h"

// Function handles.

class fcn_handle_elt
{
public:

  fcn_handle_elt (void) : fcn (0), nm ("@[]") { }

  fcn_handle_elt (octave_function *f, const std::string& n)
    : fcn (f), nm (std::string ("@") + n) { }

  fcn_handle_elt (const fcn_handle_elt& fhe)
    : fcn (fhe.fcn), nm (fhe.nm) { }

  fcn_handle_elt& operator = (const fcn_handle_elt& fhe)
    {
      if (this != &fhe)
	{
	  fcn = fhe.fcn;
	  nm  = fhe.nm;
	}

      return *this;
    }

  ~fcn_handle_elt (void) { }

  octave_function *function_value (void) { return fcn; }

  std::string name (void) const { return nm; }

private:

  // The function we are handling.
  octave_function *fcn;

  // The name of the handle, including the "@".
  std::string nm;
};

class fcn_handle_array : public ArrayN<fcn_handle_elt>
{
public:

  fcn_handle_array (void) : ArrayN<fcn_handle_elt> () { }

  fcn_handle_array (const dim_vector& dv,
		    const fcn_handle_elt& val = resize_fill_value ())
    : ArrayN<fcn_handle_elt> (dv, val) { }

  fcn_handle_array (octave_function *f, const std::string& nm)
    : ArrayN<fcn_handle_elt> (dim_vector (1, 1), fcn_handle_elt (f, nm)) { }

  fcn_handle_array (const ArrayN<fcn_handle_elt>& fa)
    : ArrayN<fcn_handle_elt> (fa) { }

  fcn_handle_array (const fcn_handle_array& fa)
    : ArrayN<fcn_handle_elt> (fa) { }

  ~fcn_handle_array (void) { }

  fcn_handle_array& operator = (const fcn_handle_array& fa)
    {
      if (this != &fa)
	ArrayN<fcn_handle_elt>::operator = (fa);

      return *this;
    }

  fcn_handle_array squeeze (void) const
    { return ArrayN<fcn_handle_elt>::squeeze (); }

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  ArrayN<std::string> names (void) const;

  static int compute_index (Array<int>& ra_idx,
			    const dim_vector& dimensions);

  static fcn_handle_elt resize_fill_value (void)
    {
      static fcn_handle_elt nil_handle = fcn_handle_elt ();
      return nil_handle;
    }
};

class
octave_fcn_handle : public octave_base_matrix<fcn_handle_array>
{
public:

  octave_fcn_handle (void)
    : octave_base_matrix<fcn_handle_array> () { }

  octave_fcn_handle (octave_function *f, const std::string& n)
    : octave_base_matrix<fcn_handle_array>
        (fcn_handle_array (dim_vector (1, 1), fcn_handle_elt (f, n))) { }

  octave_fcn_handle (const fcn_handle_array& fha)
    : octave_base_matrix<fcn_handle_array> (fha) { }

  octave_fcn_handle (const octave_fcn_handle& fh)
    : octave_base_matrix<fcn_handle_array> (fh) { }

  ~octave_fcn_handle (void) { }

  octave_value *clone (void) const { return new octave_fcn_handle (*this); }
  octave_value *empty_clone (void) const { return new octave_fcn_handle (); }

  bool is_defined (void) const { return true; }

  bool is_function_handle (void) const { return true; }

  octave_function *function_value (bool = false);

  std::string name (void) const;

  octave_fcn_handle *fcn_handle_value (bool = false) { return this; }

  fcn_handle_array fcn_handle_array_value (void) const { return matrix; }

  ArrayN<std::string> name_array (void) const { return matrix.names (); }

  bool print_as_scalar (void) const { return true; }

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

private:

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

extern octave_value make_fcn_handle (const std::string& nm);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
