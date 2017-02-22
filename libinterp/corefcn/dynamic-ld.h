/*

Copyright (C) 1993-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_dynamic_ld_h)
#define octave_dynamic_ld_h 1

#include "octave-config.h"

#include <list>
#include <string>

#include "oct-shlib.h"

class octave_function;

namespace octave
{
  class
  dynamic_loader
  {
  private:

    class
    shlibs_list
    {
    public:

      typedef std::list<octave::dynamic_library>::iterator iterator;
      typedef std::list<octave::dynamic_library>::const_iterator const_iterator;

      shlibs_list (void) : lib_list () { }

      ~shlibs_list (void) = default;

      void append (const octave::dynamic_library& shl);

      void remove (octave::dynamic_library& shl,
                   octave::dynamic_library::close_hook cl_hook = 0);

      octave::dynamic_library find_file (const std::string& file_name) const;

      void display (void) const;

    private:

      // No copying!

      shlibs_list (const shlibs_list&) = delete;

      shlibs_list& operator = (const shlibs_list&) = delete;

      // List of libraries we have loaded.
      std::list<octave::dynamic_library> lib_list;
    };

  protected:

    dynamic_loader (void) : loaded_shlibs () { }

  public:

    // No copying!

    dynamic_loader (const dynamic_loader&) = delete;

    dynamic_loader& operator = (const dynamic_loader&) = delete;

    virtual ~dynamic_loader (void) = default;

    static octave_function *
    load_oct (const std::string& fcn_name,
              const std::string& file_name = "",
              bool relative = false);

    static octave_function *
    load_mex (const std::string& fcn_name,
              const std::string& file_name = "",
              bool relative = false);

    static bool remove_oct (const std::string& fcn_name,
                            octave::dynamic_library& shl);

    static bool remove_mex (const std::string& fcn_name,
                            octave::dynamic_library& shl);

  private:

    static dynamic_loader *instance;

    static void cleanup_instance (void) { delete instance; instance = 0; }

    static bool instance_ok (void);

    static void do_clear_function (const std::string& fcn_name);

    void do_clear (octave::dynamic_library& oct_file);

    octave_function *
    do_load_oct (const std::string& fcn_name,
                 const std::string& file_name = "",
                 bool relative = false);

    octave_function *
    do_load_mex (const std::string& fcn_name,
                 const std::string& file_name = "",
                 bool relative = false);

    bool do_remove_oct (const std::string& fcn_name, octave::dynamic_library& shl);

    bool do_remove_mex (const std::string& fcn_name, octave::dynamic_library& shl);

    static bool doing_load;

  protected:

    shlibs_list loaded_shlibs;

    static std::string name_mangler (const std::string& name);

    static std::string name_uscore_mangler (const std::string& name);

    static std::string mex_mangler (const std::string& name);

    static std::string mex_uscore_mangler (const std::string& name);

    static std::string mex_f77_mangler (const std::string& name);
  };
}

#endif
