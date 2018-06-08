/*

Copyright (C) 1993-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_defaults_h)
#define octave_defaults_h 1

#include "octave-config.h"

#include <string>

#include "installation-data.h"

namespace octave
{
  namespace config
  {
#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::prepend_octave_home' instead")
    inline std::string prepend_octave_home (const std::string& s)
    {
      installation_data& inst_data
        = __get_installation_data__ ("prepend_octave_home");

      return inst_data.prepend_home (s);
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::prepend_octave_exec_home' instead")
    inline std::string prepend_octave_exec_home (const std::string& s)
    {
      installation_data& inst_data
        = __get_installation_data__ ("prepend_octave_exec_home");

      return inst_data.prepend_exec_home (s);
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::canonical_host_type' instead")
    inline std::string canonical_host_type (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("canonical_host_type");

      return inst_data.canonical_host_type ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::release' instead")
    inline std::string release (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("release");

      return inst_data.release ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::default_pager' instead")
    inline std::string default_pager (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("default_pager");

      return inst_data.default_pager ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::octave_home' instead")
    inline std::string octave_home (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("octave_home");

      return inst_data.home ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::octave_exec_home' instead")
    inline std::string octave_exec_home (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("octave_exec_home");

      return inst_data.exec_home ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::bin_dir' instead")
    inline std::string bin_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("bin_dir");

      return inst_data.bin_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::data_dir' instead")
    inline std::string data_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("data_dir");

      return inst_data.data_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::dataroot_dir' instead")
    inline std::string dataroot_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("dataroot_dir");

      return inst_data.dataroot_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::include_dir' instead")
    inline std::string include_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("include_dir");

      return inst_data.include_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::lib_dir' instead")
    inline std::string lib_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("lib_dir");

      return inst_data.lib_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::libexec_dir' instead")
    inline std::string libexec_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("libexec_dir");

      return inst_data.libexec_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_ver_arch_lib_dir' instead")
    inline std::string arch_lib_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("arch_lib_dir");

      return inst_data.arch_lib_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_api_arch_lib_dir' instead")
    inline std::string info_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("info_dir");

      return inst_data.info_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_arch_lib_dir' instead")
    inline std::string local_ver_arch_lib_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_ver_arch_lib_dir");

      return inst_data.local_ver_arch_lib_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::arch_lib_dir' instead")
    inline std::string local_api_arch_lib_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_api_arch_lib_dir");

      return inst_data.local_api_arch_lib_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_ver_oct_file_dir' instead")
    inline std::string local_arch_lib_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_arch_lib_dir");

      return inst_data.local_arch_lib_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_api_oct_file_dir' instead")
    inline std::string local_ver_oct_file_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_ver_oct_file_dir");

      return inst_data.local_ver_oct_file_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_oct_file_dir' instead")
    inline std::string local_api_oct_file_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_api_oct_file_dir");

      return inst_data.local_api_oct_file_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::oct_file_dir' instead")
    inline std::string local_oct_file_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_oct_file_dir");

      return inst_data.local_oct_file_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_ver_fcn_file_dir' instead")
    inline std::string oct_file_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("oct_file_dir");

      return inst_data.oct_file_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_api_fcn_file_dir' instead")
    inline std::string local_ver_fcn_file_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_ver_fcn_file_dir");

      return inst_data.local_ver_fcn_file_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_fcn_file_dir' instead")
    inline std::string local_api_fcn_file_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_api_fcn_file_dir");

      return inst_data.local_api_fcn_file_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::fcn_file_dir' instead")
    inline std::string local_fcn_file_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_fcn_file_dir");

      return inst_data.local_fcn_file_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::oct_data_dir' instead")
    inline std::string fcn_file_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("fcn_file_dir");

      return inst_data.fcn_file_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::oct_doc_dir' instead")
    inline std::string oct_data_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("oct_data_dir");

      return inst_data.oct_data_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::oct_etc_dir' instead")
    inline std::string oct_doc_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("oct_doc_dir");

      return inst_data.oct_doc_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::oct_fonts_dir' instead")
    inline std::string oct_etc_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("oct_etc_dir");

      return inst_data.oct_etc_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::oct_include_dir' instead")
    inline std::string oct_fonts_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("oct_fonts_dir");

      return inst_data.oct_fonts_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::oct_lib_dir' instead")
    inline std::string oct_include_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("oct_include_dir");

      return inst_data.oct_include_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::oct_locale_dir' instead")
    inline std::string oct_lib_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("oct_lib_dir");

      return inst_data.oct_lib_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::oct_tests_dir' instead")
    inline std::string oct_locale_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("oct_locale_dir");

      return inst_data.oct_locale_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::info_dir' instead")
    inline std::string oct_tests_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("oct_tests_dir");

      return inst_data.oct_tests_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::man_dir' instead")
    inline std::string man_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("man_dir");

      return inst_data.man_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::man1_dir' instead")
    inline std::string man1_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("man1_dir");

      return inst_data.man1_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::man1_ext' instead")
    inline std::string man1_ext (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("man1_ext");

      return inst_data.man1_ext ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::image_dir' instead")
    inline std::string image_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("image_dir");

      return inst_data.image_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_startupfile_dir' instead")
    inline std::string local_startupfile_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_startupfile_dir");

      return inst_data.local_startupfile_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::startupfile_dir' instead")
    inline std::string startupfile_dir (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("startupfile_dir");

      return inst_data.startupfile_dir ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::local_site_defaults_file' instead")
    inline std::string local_site_defaults_file (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("local_site_defaults_file");

      return inst_data.local_site_defaults_file ();
    }

    OCTAVE_DEPRECATED (5, "use 'octave::installation_data::site_defaults_file' instead")
    inline std::string site_defaults_file (void)
    {
      installation_data& inst_data
        = __get_installation_data__ ("site_defaults_file");

      return inst_data.site_defaults_file ();
    }

#endif
  }
}

#endif
