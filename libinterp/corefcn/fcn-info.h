/*

Copyright (C) 1993-2017 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#if ! defined (octave_fcn_info_h)
#define octave_fcn_info_h 1

#include "octave-config.h"

#include <list>
#include <map>
#include <string>

#include "oct-refcount.h"

#include "ov.h"
#include "ovl.h"

namespace octave
{
  class fcn_info
  {
  public:

    typedef std::map<std::string, octave_value>::const_iterator
    str_val_const_iterator;
    typedef std::map<std::string, octave_value>::iterator str_val_iterator;

  private:

    class fcn_info_rep
    {
    public:

      fcn_info_rep (const std::string& nm)
        : name (nm), package_name (), local_functions (),
          private_functions (), class_constructors (), class_methods (),
          cmdline_function (), autoload_function (), function_on_path (),
          built_in_function (), count (1)
      {
        size_t pos = name.rfind ('.');

        if (pos != std::string::npos)
          {
            package_name = name.substr (0, pos);
            name = name.substr (pos+1);
          }
      }

      // No copying!

      fcn_info_rep (const fcn_info_rep&) = delete;

      fcn_info_rep& operator = (const fcn_info_rep&) = delete;

      ~fcn_info_rep (void) = default;

      octave_value install_local_function (const std::string& file_name);

      octave_value load_private_function (const std::string& dir_name);

      octave_value load_class_constructor (void);

      octave_value load_class_method (const std::string& dispatch_type);

      octave_value find (const octave_value_list& args, bool local_funcs);

      octave_value builtin_find (void);

      octave_value find_method (const std::string& dispatch_type);

      octave_value find_autoload (void);

      octave_value find_package (void);

      octave_value find_user_function (void);

      bool is_user_function_defined (void) const
      {
        return function_on_path.is_defined ();
      }

      octave_value find_function (const octave_value_list& args,
                                  bool local_funcs)
      {
        return find (args, local_funcs);
      }

      void install_cmdline_function (const octave_value& f)
      {
        cmdline_function = f;
      }

      void install_local_function (const octave_value& f,
                                   const std::string& file_name)
      {
        local_functions[file_name] = f;
      }

      void install_user_function (const octave_value& f)
      {
        function_on_path = f;
      }

      void install_built_in_function (const octave_value& f)
      {
        built_in_function = f;
      }

      void install_built_in_dispatch (const std::string& klass);

      template <typename T>
      void
      clear_map (std::map<T, octave_value>& map, bool force = false)
      {
        typename std::map<T, octave_value>::iterator p = map.begin ();

        while (p != map.end ())
          {
            if (force || ! p->second.islocked ())
              map.erase (p++);
            else
              p++;
          }
      }

      void clear_autoload_function (bool force = false)
      {
        if (force || ! autoload_function.islocked ())
          autoload_function = octave_value ();
      }

      // We also clear command line functions here, as these are both
      // "user defined"
      void clear_user_function (bool force = false)
      {
        clear_autoload_function (force);

        if (force || ! function_on_path.islocked ())
          function_on_path = octave_value ();

        if (force || ! cmdline_function.islocked ())
          cmdline_function = octave_value ();
      }

      void clear_mex_function (void)
      {
        if (function_on_path.is_mex_function ())
          clear_user_function ();
      }

      void clear_package (void)
      {
        package = octave_value ();
      }

      void clear (bool force = false)
      {
        clear_map (local_functions, force);
        clear_map (private_functions, force);
        clear_map (class_constructors, force);
        clear_map (class_methods, force);

        clear_autoload_function (force);
        clear_user_function (force);
        clear_package ();
      }

      octave_value dump (void) const;

      std::string full_name (void) const
      {
        if (package_name.empty ())
          return name;
        else
          return package_name + '.' + name;
      }

      std::string name;

      std::string package_name;

      // File name to function object.
      std::map<std::string, octave_value> local_functions;

      // Directory name to function object.
      std::map<std::string, octave_value> private_functions;

      // Class name to function object.
      std::map<std::string, octave_value> class_constructors;

      // Dispatch type to function object.
      std::map<std::string, octave_value> class_methods;

      octave_value cmdline_function;

      octave_value autoload_function;

      octave_value function_on_path;

      octave_value package;

      octave_value built_in_function;

      refcount<size_t> count;

    private:

      octave_value xfind (const octave_value_list& args, bool local_funcs);

      octave_value x_builtin_find (void);
    };

  public:

    fcn_info (const std::string& nm = "")
      : rep (new fcn_info_rep (nm)) { }

    fcn_info (const fcn_info& fi) : rep (fi.rep)
    {
      rep->count++;
    }

    fcn_info& operator = (const fcn_info& fi)
    {
      if (this != &fi)
        {
          if (--rep->count == 0)
            delete rep;

          rep = fi.rep;
          rep->count++;
        }

      return *this;
    }

    ~fcn_info (void)
    {
      if (--rep->count == 0)
        delete rep;
    }

    octave_value find (const octave_value_list& args = octave_value_list (),
                       bool local_funcs = true)
    {
      return rep->find (args, local_funcs);
    }

    octave_value builtin_find (void)
    {
      return rep->builtin_find ();
    }

    octave_value find_method (const std::string& dispatch_type) const
    {
      return rep->find_method (dispatch_type);
    }

    octave_value find_built_in_function (void) const
    {
      return rep->built_in_function;
    }

    octave_value find_cmdline_function (void) const
    {
      return rep->cmdline_function;
    }

    octave_value find_autoload (void)
    {
      return rep->find_autoload ();
    }

    octave_value find_user_function (void)
    {
      return rep->find_user_function ();
    }

    bool is_user_function_defined (void) const
    {
      return rep->is_user_function_defined ();
    }

    octave_value find_function (const octave_value_list& args
                                = octave_value_list (),
                                bool local_funcs = true)
    {
      return rep->find_function (args, local_funcs);
    }

    void install_cmdline_function (const octave_value& f)
    {
      rep->install_cmdline_function (f);
    }

    void install_local_function (const octave_value& f,
                                 const std::string& file_name)
    {
      rep->install_local_function (f, file_name);
    }

    void install_user_function (const octave_value& f)
    {
      rep->install_user_function (f);
    }

    void install_built_in_function (const octave_value& f)
    {
      rep->install_built_in_function (f);
    }

    void install_built_in_dispatch (const std::string& klass)
    {
      rep->install_built_in_dispatch (klass);
    }

    void clear (bool force = false) { rep->clear (force); }

    void clear_user_function (bool force = false)
    {
      rep->clear_user_function (force);
    }

    void clear_autoload_function (bool force = false)
    {
      rep->clear_autoload_function (force);
    }

    void clear_mex_function (void) { rep->clear_mex_function (); }

    octave_value dump (void) const { return rep->dump (); }

  private:

    fcn_info_rep *rep;
  };

  octave_value
  dump_function_map (const std::map<std::string, octave_value>& fcn_map);
}

#endif
