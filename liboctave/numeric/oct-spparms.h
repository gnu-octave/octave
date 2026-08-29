////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2026 The Octave Project Developers
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

#if ! defined (octave_oct_spparms_h)
#define octave_oct_spparms_h 1

#include "octave-config.h"

#include <array>
#include <iosfwd>
#include <string>
#include <string_view>

#include "Array-fwd.h"
#include "str-vec.h"
#include "dColVector.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class OCTAVE_API sparse_params
{
protected:

  sparse_params ()
  {
    do_defaults ();
  }

public:

  sparse_params (const sparse_params&) = default;

  sparse_params& operator = (const sparse_params&) = default;

  ~sparse_params () = default;

  static bool instance_ok ();

  static void defaults ();

  static void tight ();

  static string_vector get_keys ();

  static ColumnVector get_vals ();

  static bool set_vals (const Array<double>& vals);

  static bool set_key (const std::string& key, const double& val);

  static double get_key (const std::string& key);

  static double get_bandden ();

  static void print_info (std::ostream& os, const std::string& prefix);

  // Number of sparse parameters.
  static constexpr octave_idx_type NUM_PARAMS = 13;

private:

  static constexpr std::array<std::string_view, NUM_PARAMS> s_keys = {{
    "spumoni", "ths_rel", "ths_abs", "exact_d", "supernd",
    "rreduce", "wh_frac", "autommd", "autoamd", "piv_tol",
    "bandden", "umfpack", "sym_tol"
  }};

  std::array<double, NUM_PARAMS> m_params;

  static sparse_params *s_instance;

  static void cleanup_instance ()
  {
    delete s_instance;
    s_instance = nullptr;
  }

  void do_defaults ();

  void do_tight ();

  string_vector do_get_keys () const;

  ColumnVector do_get_vals () const;

  bool do_set_vals (const Array<double>& vals);

  bool do_set_key (const std::string& key, const double& val);

  double do_get_key (const std::string& key);

  double do_get_bandden ();

  void do_print_info (std::ostream& os, const std::string& prefix) const;
};

OCTAVE_END_NAMESPACE(octave)

#endif
