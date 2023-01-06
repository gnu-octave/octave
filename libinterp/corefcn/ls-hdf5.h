////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_ls_hdf5_h)
#define octave_ls_hdf5_h 1

#include "octave-config.h"

#include <iosfwd>

#include "oct-hdf5-types.h"
#include "ov.h"

// first, we need to define our own dummy stream subclass, since
// HDF5 needs to do its own file i/o

// hdf5_fstreambase is used for both input and output streams, modeled
// on the fstreambase class in <fstream.h>

class hdf5_fstreambase : virtual public std::ios
{
public:

  // HDF5 uses an "id" to refer to an open file
  octave_hdf5_id file_id;

  // keep track of current item index in the file
  int current_item;

  hdf5_fstreambase () : file_id (-1), current_item () { }

  ~hdf5_fstreambase () { close (); }

  OCTINTERP_API hdf5_fstreambase (const char *name, int mode,
                                  int /* prot */ = 0);

  OCTINTERP_API void close (void);

  OCTINTERP_API void open (const char *name, int mode, int);

  OCTINTERP_API void open_create (const char *name, int mode);
};

// input and output streams, subclassing istream and ostream
// so that we can pass them for stream parameters in the functions below.

class hdf5_ifstream : public hdf5_fstreambase, public std::istream
{
public:

  hdf5_ifstream () : hdf5_fstreambase (), std::istream (nullptr) { }

  hdf5_ifstream (const char *name, int mode = std::ios::in | std::ios::binary,
                 int prot = 0)
    : hdf5_fstreambase (name, mode, prot), std::istream (nullptr) { }

  void open (const char *name, int mode = std::ios::in | std::ios::binary,
             int prot = 0)
  { hdf5_fstreambase::open (name, mode, prot); }
};

class hdf5_ofstream : public hdf5_fstreambase, public std::ostream
{
public:

  hdf5_ofstream () : hdf5_fstreambase (), std::ostream (nullptr) { }

  hdf5_ofstream (const char *name, int mode = std::ios::out | std::ios::binary,
                 int prot = 0)
    : hdf5_fstreambase (name, mode, prot), std::ostream (nullptr) { }

  void open (const char *name, int mode = std::ios::out | std::ios::binary,
             int prot = 0)
  { hdf5_fstreambase::open (name, mode, prot); }
};

// Callback data structure for passing data to hdf5_read_next_data, below.

struct hdf5_callback_data
{
public:
  hdf5_callback_data (void)
    : name (), global (false), tc (), doc () { }

  // the following fields are set by hdf5_read_data on successful return:

  // the name of the variable
  std::string name;

  // whether it is global
  bool global;

  // the value of the variable, in Octave form
  octave_value tc;

  // a documentation string (NULL if none)
  std::string doc;
};

extern OCTINTERP_API octave_hdf5_id
save_type_to_hdf5 (save_type st);

extern OCTINTERP_API octave_hdf5_id
hdf5_make_complex_type (octave_hdf5_id num_type);

extern OCTINTERP_API bool
hdf5_types_compatible (octave_hdf5_id t1, octave_hdf5_id t2);

extern OCTINTERP_API octave_hdf5_err
hdf5_read_next_data (octave_hdf5_id group_id, const char *name, void *dv);

extern OCTINTERP_API octave_hdf5_err
hdf5_h5g_iterate (octave_hdf5_id loc_id, const char *name, int *idx,
                  void *operator_data);

extern OCTINTERP_API bool
add_hdf5_data (octave_hdf5_id loc_id, const octave_value& tc,
               const std::string& name, const std::string& doc,
               bool mark_global, bool save_as_floats);

extern OCTINTERP_API int
save_hdf5_empty (octave_hdf5_id loc_id, const char *name, const dim_vector& d);

extern OCTINTERP_API int
load_hdf5_empty (octave_hdf5_id loc_id, const char *name, dim_vector& d);

extern OCTINTERP_API std::string
read_hdf5_data (std::istream& is,  const std::string& filename, bool& global,
                octave_value& tc, std::string& doc,
                const string_vector& argv, int argv_idx, int argc);

extern OCTINTERP_API bool
save_hdf5_data (std::ostream& os, const octave_value& tc,
                const std::string& name, const std::string& doc,
                bool mark_global, bool save_as_floats);

extern OCTINTERP_API bool
hdf5_check_attr (octave_hdf5_id loc_id, const char *attr_name);

extern OCTINTERP_API bool
hdf5_get_scalar_attr (octave_hdf5_id loc_id, octave_hdf5_id type_id,
                      const char *attr_name, void *buf);

extern OCTINTERP_API octave_hdf5_err
hdf5_add_attr (octave_hdf5_id loc_id, const char *attr_name);


extern OCTINTERP_API octave_hdf5_err
hdf5_add_scalar_attr (octave_hdf5_id loc_id, octave_hdf5_id type_id,
                      const char *attr_name, void *buf);

#endif
