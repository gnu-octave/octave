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

#if ! defined (octave_installation_data_h)
#define octave_installation_data_h 1

#include "octave-config.h"

#include <string>

namespace octave
{
  class
  installation_data
  {
  public:

    installation_data (void);

    installation_data (const installation_data&) = delete;

    installation_data& operator = (const installation_data&) = delete;

    ~installation_data (void) = default;

    std::string canonical_host_type (void) const
    {
      return m_canonical_host_type;
    }

    std::string release (void) const
    {
      return m_release;
    }

    std::string default_pager (void) const
    {
      return m_default_pager;
    }

    std::string home (void) const
    {
      return m_home;
    }

    std::string exec_home (void) const
    {
      return m_exec_home;
    }

    std::string bin_dir (void) const
    {
      return m_bin_dir;
    }

    std::string data_dir (void) const
    {
      return m_data_dir;
    }

    std::string dataroot_dir (void) const
    {
      return m_dataroot_dir;
    }

    std::string include_dir (void) const
    {
      return m_include_dir;
    }

    std::string lib_dir (void) const
    {
      return m_lib_dir;
    }

    std::string libexec_dir (void) const
    {
      return m_libexec_dir;
    }

    std::string local_ver_arch_lib_dir (void) const
    {
      return m_local_ver_arch_lib_dir;
    }

    std::string local_api_arch_lib_dir (void) const
    {
      return m_local_api_arch_lib_dir;
    }

    std::string local_arch_lib_dir (void) const
    {
      return m_local_arch_lib_dir;
    }

    std::string arch_lib_dir (void) const
    {
      return m_arch_lib_dir;
    }

    std::string local_ver_oct_file_dir (void) const
    {
      return m_local_ver_oct_file_dir;
    }

    std::string local_api_oct_file_dir (void) const
    {
      return m_local_api_oct_file_dir;
    }

    std::string local_oct_file_dir (void) const
    {
      return m_local_oct_file_dir;
    }

    std::string oct_file_dir (void) const
    {
      return m_oct_file_dir;
    }

    std::string local_ver_fcn_file_dir (void) const
    {
      return m_local_ver_fcn_file_dir;
    }

    std::string local_api_fcn_file_dir (void) const
    {
      return m_local_api_fcn_file_dir;
    }

    std::string local_fcn_file_dir (void) const
    {
      return m_local_fcn_file_dir;
    }

    std::string fcn_file_dir (void) const
    {
      return m_fcn_file_dir;
    }

    std::string oct_data_dir (void) const
    {
      return m_oct_data_dir;
    }

    std::string oct_doc_dir (void) const
    {
      return m_oct_doc_dir;
    }

    std::string oct_etc_dir (void) const
    {
      return m_oct_etc_dir;
    }

    std::string oct_fonts_dir (void) const
    {
      return m_oct_fonts_dir;
    }

    std::string oct_include_dir (void) const
    {
      return m_oct_include_dir;
    }

    std::string oct_lib_dir (void) const
    {
      return m_oct_lib_dir;
    }

    std::string oct_locale_dir (void) const
    {
      return m_oct_locale_dir;
    }

    std::string oct_tests_dir (void) const
    {
      return m_oct_tests_dir;
    }

    std::string info_dir (void) const
    {
      return m_info_dir;
    }

    std::string man_dir (void) const
    {
      return m_man_dir;
    }

    std::string man1_dir (void) const
    {
      return m_man1_dir;
    }

    std::string man1_ext (void) const
    {
      return m_man1_ext;
    }

    std::string image_dir (void) const
    {
      return m_image_dir;
    }

    std::string local_startupfile_dir (void) const
    {
      return m_local_startupfile_dir;
    }

    std::string startupfile_dir (void) const
    {
      return m_startupfile_dir;
    }

    std::string local_site_defaults_file (void) const
    {
      return m_local_site_defaults_file;
    }

    std::string site_defaults_file (void) const
    {
      return m_site_defaults_file;
    }

    std::string prepend_home (const std::string& s) const;

    std::string prepend_exec_home (const std::string& s) const;

  private:

    void set_home (void);

    void set_local_site_defaults_file (void);

    void set_site_defaults_file (void);

    std::string m_canonical_host_type;
    std::string m_release;
    std::string m_default_pager;

    std::string m_home;
    std::string m_exec_home;

    std::string m_bin_dir;
    std::string m_data_dir;
    std::string m_dataroot_dir;
    std::string m_include_dir;
    std::string m_lib_dir;
    std::string m_libexec_dir;

    std::string m_local_ver_arch_lib_dir;
    std::string m_local_api_arch_lib_dir;
    std::string m_local_arch_lib_dir;
    std::string m_arch_lib_dir;

    std::string m_local_ver_oct_file_dir;
    std::string m_local_api_oct_file_dir;
    std::string m_local_oct_file_dir;
    std::string m_oct_file_dir;

    std::string m_local_ver_fcn_file_dir;
    std::string m_local_api_fcn_file_dir;
    std::string m_local_fcn_file_dir;
    std::string m_fcn_file_dir;

    std::string m_oct_data_dir;
    std::string m_oct_doc_dir;
    std::string m_oct_etc_dir;
    std::string m_oct_fonts_dir;
    std::string m_oct_include_dir;
    std::string m_oct_lib_dir;
    std::string m_oct_locale_dir;
    std::string m_oct_tests_dir;

    std::string m_info_dir;

    std::string m_man_dir;
    std::string m_man1_dir;
    std::string m_man1_ext;

    std::string m_image_dir;

    std::string m_local_startupfile_dir;
    std::string m_startupfile_dir;

    std::string m_local_site_defaults_file;
    std::string m_site_defaults_file;
  };
}

#endif
