////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#if ! defined (octave_load_save_h)
#define octave_load_save_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

#include "mach-info.h"

#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;
class load_save_format;
class symbol_info;

class load_save_system
{
public:

  // FIXME: maybe MAT5 and MAT7 should be options to MAT_BINARY.
  // Similarly, save_as_floats may be an option for LS_BINARY,
  // LS_HDF5 etc.

  enum format_type
  {
    TEXT,
    BINARY,
    MAT_ASCII,
    MAT_BINARY,
    MAT5_BINARY,
    MAT7_BINARY,
    HDF5,
    UNKNOWN
  };

  enum format_options
  {
    // MAT_ASCII options (not exclusive)
    MAT_ASCII_LONG = 1,
    MAT_ASCII_TABS = 2,
    // MAT_BINARY options
    MAT_BINARY_V5 = 1,
    MAT_BINARY_V7,
    // zero means no option.
    NO_OPTION = 0
  };

  OCTINTERP_API load_save_system (interpreter& interp);

  OCTINTERP_API ~load_save_system (void);

  load_save_system (const load_save_system&) = delete;

  load_save_system& operator = (const load_save_system&) = delete;

  OCTINTERP_API octave_value
  crash_dumps_octave_core (const octave_value_list& args, int nargout);

  bool crash_dumps_octave_core (void) const
  {
    return m_crash_dumps_octave_core;
  }

  bool crash_dumps_octave_core (bool flag)
  {
    return set (m_crash_dumps_octave_core, flag);
  }

  octave_value octave_core_file_limit (const octave_value_list& args,
                                       int nargout);

  double octave_core_file_limit (void) const
  {
    return m_octave_core_file_limit;
  }

  double octave_core_file_limit (double limit)
  {
    return set (m_octave_core_file_limit, limit);
  }

  OCTINTERP_API octave_value
  octave_core_file_name (const octave_value_list& args, int nargout);

  std::string octave_core_file_name (void) const
  {
    return m_octave_core_file_name;
  }

  std::string octave_core_file_name (const std::string& file)
  {
    return set (m_octave_core_file_name, file);
  }

  OCTINTERP_API octave_value
  save_default_options (const octave_value_list& args, int nargout);

  std::string save_default_options (void) const
  {
    return m_save_default_options;
  }

  std::string save_default_options (const std::string& options)
  {
    return set (m_save_default_options, options);
  }

  OCTINTERP_API octave_value
  octave_core_file_options (const octave_value_list& args, int nargout);

  std::string octave_core_file_options (void) const
  {
    return m_octave_core_file_options;
  }

  std::string octave_core_file_options (const std::string& options)
  {
    return set (m_octave_core_file_options, options);
  }

  OCTINTERP_API octave_value
  save_header_format_string (const octave_value_list& args, int nargout);

  std::string save_header_format_string (void) const
  {
    return m_save_header_format_string;
  }

  std::string save_header_format_string (const std::string& format)
  {
    return set (m_save_header_format_string, format);
  }

  static OCTINTERP_API load_save_format
  get_file_format (const std::string& fname, const std::string& orig_fname,
                   bool& use_zlib, bool quiet = false);

  // FIXME: this is probably not the best public interface for
  // loading and saving variables, but it is what is currently
  // needed for the Fload and Fsave functions.

  OCTINTERP_API octave_value
  load_vars (std::istream& stream, const std::string& orig_fname,
             const load_save_format& fmt, mach_info::float_format flt_fmt,
             bool list_only, bool swap, bool verbose,
             const string_vector& argv, int argv_idx, int argc, int nargout);

  static OCTINTERP_API string_vector
  parse_save_options (const string_vector& argv, load_save_format& fmt,
                      bool& append, bool& save_as_floats, bool& use_zlib);

  static OCTINTERP_API string_vector
  parse_save_options (const std::string& arg, load_save_format& fmt,
                      bool& append, bool& save_as_floats, bool& use_zlib);

  OCTINTERP_API void
  save_vars (const string_vector& argv, int argv_idx, int argc,
             std::ostream& os, const load_save_format& fmt,
             bool save_as_floats, bool write_header_info);

  OCTINTERP_API void dump_octave_core (void);

  OCTINTERP_API octave_value_list
  load (const octave_value_list& args = octave_value_list (),
        int nargout = 0);

  OCTINTERP_API octave_value_list
  save (const octave_value_list& args = octave_value_list (),
        int nargout = 0);

private:

  interpreter& m_interpreter;

  // Write octave-workspace file if Octave crashes or is killed by a
  // signal.
  bool m_crash_dumps_octave_core;

  // The maximum amount of memory (in kilobytes) that we will
  // attempt to write to the Octave core file.
  double m_octave_core_file_limit;

  // The name of the Octave core file.
  std::string m_octave_core_file_name;

  // The default output format.  May be one of "binary", "text",
  // "mat-binary", or "hdf5".
  std::string m_save_default_options;

  // The output format for Octave core files.
  std::string m_octave_core_file_options;

  // The format string for the comment line at the top of
  // text-format save files.  Passed to strftime.  Should begin with
  // '#' and contain no newline characters.
  std::string m_save_header_format_string;

  OCTINTERP_API void
  write_header (std::ostream& os, const load_save_format& fmt);

  OCTINTERP_API std::size_t
  save_vars (std::ostream& os, const std::string& pattern,
             const load_save_format& fmt, bool save_as_floats);

  OCTINTERP_API void
  do_save (std::ostream& os, const octave_value& tc, const std::string& name,
           const std::string& help, bool global, const load_save_format& fmt,
           bool save_as_floats);

  OCTINTERP_API void
  do_save (std::ostream& os, const symbol_info& syminfo,
           const load_save_format& fmt, bool save_as_floats);

  OCTINTERP_API std::size_t
  save_fields (std::ostream& os, const octave_scalar_map& m,
               const std::string& pattern, const load_save_format& fmt,
               bool save_as_floats);

  OCTINTERP_API void
  dump_octave_core (std::ostream& os, const char *fname,
                    const load_save_format& fmt, bool save_as_floats);

  OCTINTERP_API void
  install_loaded_variable (const std::string& name, const octave_value& val,
                           bool global, const std::string& /*doc*/);

  static OCTINTERP_API std::string init_save_header_format (void);

  static OCTINTERP_API load_save_format
  get_file_format (std::istream& file, const std::string& filename);

  template <typename T>
  T set (T& var, const T& new_val)
  {
    T old_val = var;
    var = new_val;
    return old_val;
  }
};

class load_save_format
{
public:

  load_save_format (load_save_system::format_type type,
                    load_save_system::format_options options = load_save_system::NO_OPTION)
    : m_type (type), m_options (options)
  { }

  void set_type (load_save_system::format_type type) { m_type = type; }

  load_save_system::format_type type (void) const { return m_type; }

  void set_option (load_save_system::format_options option)
  {
    m_options |= option;
  }

  int options (void) const { return m_options; }

private:

  load_save_system::format_type m_type;
  int m_options;
};

OCTAVE_END_NAMESPACE(octave)

OCTAVE_DEPRECATED (7, "use 'load_save_system::dump_octave_core' instead")
extern OCTINTERP_API void dump_octave_core (void);

#endif
