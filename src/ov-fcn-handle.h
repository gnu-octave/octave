/*

Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 John W. Eaton

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

#if !defined (octave_fcn_handle_h)
#define octave_fcn_handle_h 1

#include <iosfwd>
#include <string>
#include <memory>

#include "oct-alloc.h"

#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"

// Function handles.

class
OCTINTERP_API
octave_fcn_handle : public octave_base_value
{
private:

  typedef std::map<std::string, octave_value> str_ov_map;

  octave_fcn_handle (const octave_value& f, const std::string& n,
                     str_ov_map *sdisp)
    : fcn (f), nm (n), disp (sdisp) { }

public:
  octave_fcn_handle (void)
    : fcn (), nm () { }

  octave_fcn_handle (const std::string& n)
    : fcn (), nm (n) { }

  octave_fcn_handle (const octave_value& f,  const std::string& n);

  octave_fcn_handle (const octave_fcn_handle& fh)
    : octave_base_value (fh), fcn (fh.fcn), nm (fh.nm)
   { 
     if (fh.disp.get ())
       disp.reset (new str_ov_map (*fh.disp));
   }

  ~octave_fcn_handle (void) { }

  octave_base_value *clone (void) const { return new octave_fcn_handle (*this); }
  octave_base_value *empty_clone (void) const { return new octave_fcn_handle (); }

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx)
    {
      octave_value_list tmp = subsref (type, idx, 1);
      return tmp.length () > 0 ? tmp(0) : octave_value ();
    }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout);

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& args);

  bool is_defined (void) const { return true; }

  bool is_function_handle (void) const { return true; }

  bool is_overloaded (void) const { return disp.get () && ! disp->empty (); }

  dim_vector dims (void) const { static dim_vector dv (1, 1); return dv; }

  octave_function *function_value (bool = false)
    { return fcn.function_value (); }

  octave_user_function *user_function_value (bool = false)
    { return fcn.user_function_value (); }

  octave_fcn_handle *fcn_handle_value (bool = false) { return this; }

  octave_value fcn_val (void) const { return fcn; }

  std::string fcn_name (void) const { return nm; }

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap, 
		    oct_mach_info::float_format fmt);

#if defined (HAVE_HDF5)
  bool save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (hid_t loc_id, const char *name);
#endif

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

private:

  bool set_fcn (const std::string &octaveroot, const std::string& fpath);

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

protected:

  // The function we are handling.
  octave_value fcn;

  // The name of the handle, including the "@".
  std::string nm;

  // A pointer to statical dispatch to standard classes. If null, we don't want
  // to dispatch at all.
  std::auto_ptr<str_ov_map> disp;

  friend octave_value make_fcn_handle (const std::string &, bool);
};

extern octave_value make_fcn_handle (const std::string& nm,
                                     bool local_funcs = true);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
