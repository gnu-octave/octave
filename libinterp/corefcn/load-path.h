/*

Copyright (C) 2006-2017 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#if ! defined (octave_load_path_h)
#define octave_load_path_h 1

#include "octave-config.h"

#include <iosfwd>
#include <list>
#include <map>
#include <set>
#include <string>

#include "oct-time.h"
#include "pathsearch.h"
#include "str-vec.h"

class
OCTINTERP_API
load_path
{
public:

  load_path (void)
    : package_map (), top_level_package (), dir_info_list (), init_dirs (),
      m_command_line_path (), add_hook (load_path::execute_pkg_add),
      remove_hook (load_path::execute_pkg_del)
    { }

  typedef void (*hook_fcn_ptr) (const std::string& dir);

  load_path (const load_path&) = delete;

  load_path& operator = (const load_path&) = delete;

  ~load_path (void) = default;

  void initialize (bool set_initial_path = false);

  void clear (void);

  void set (const std::string& p, bool warn = false, bool is_init = false);

  void append (const std::string& dir, bool warn = false);

  void prepend (const std::string& dir, bool warn = false);

  bool remove (const std::string& dir);

  void update (void) const;

  bool contains_canonical (const std::string& dir_name) const;

  std::string find_method (const std::string& class_name,
                           const std::string& meth,
                           std::string& dir_name,
                           const std::string& pack_name = "")
  {
    return get_package (pack_name).find_method (class_name, meth, dir_name);
  }

  std::string find_method (const std::string& class_name,
                           const std::string& meth,
                           const std::string& pack_name = "")
  {
    std::string dir_name;
    return find_method (class_name, meth, dir_name, pack_name);
  }

  std::list<std::string> methods (const std::string& class_name,
                                  const std::string& pack_name = "")
  {
    return get_package (pack_name).methods (class_name);
  }

  std::list<std::string> overloads (const std::string& meth) const;

  bool find_package (const std::string& package_name) const
  {
    return (package_map.find (package_name) != package_map.end ());
  }

  std::list<std::string> get_all_package_names (bool only_top_level = true) const;

  std::string find_fcn (const std::string& fcn, std::string& dir_name,
                        const std::string& pack_name = "")
  {
    return get_package (pack_name).find_fcn (fcn, dir_name);
  }

  std::string find_fcn (const std::string& fcn,
                        const std::string& pack_name = "")
  {
    std::string dir_name;
    return find_fcn (fcn, dir_name, pack_name);
  }

  std::string find_private_fcn (const std::string& dir,
                                const std::string& fcn,
                                const std::string& pack_name = "")
  {
    return get_package (pack_name).find_private_fcn (dir, fcn);
  }

  std::string find_fcn_file (const std::string& fcn,
                             const std::string& pack_name = "")
  {
    std::string dir_name;
    return get_package (pack_name).find_fcn (fcn, dir_name, M_FILE);
  }

  std::string find_oct_file (const std::string& fcn,
                             const std::string& pack_name = "")
  {
    std::string dir_name;
    return get_package (pack_name).find_fcn (fcn, dir_name, M_FILE);
  }

  std::string find_mex_file (const std::string& fcn,
                             const std::string& pack_name = "")
  {
    std::string dir_name;
    return get_package (pack_name).find_fcn (fcn, dir_name, M_FILE);
  }

  std::string find_file (const std::string& file) const;

  std::string find_dir (const std::string& dir) const;

  string_vector find_matching_dirs (const std::string& dir) const;

  std::string find_first_of (const string_vector& files) const;

  string_vector find_all_first_of (const string_vector& files) const;

  string_vector dirs (void) const;

  std::list<std::string> dir_list (void) const;

  string_vector files (const std::string& dir, bool omit_exts = false) const;

  string_vector fcn_names (void) const;

  std::string path (void) const;

  void display (std::ostream& os) const;

  hook_fcn_ptr get_add_hook (void) { return add_hook; }
  hook_fcn_ptr get_remove_hook (void) { return remove_hook; }

  void set_add_hook (hook_fcn_ptr f) { add_hook = f; }
  void set_remove_hook (hook_fcn_ptr f) { remove_hook = f; }

  static void execute_pkg_add (const std::string& dir);
  static void execute_pkg_del (const std::string& dir);

  void set_command_line_path (const std::string& p)
  {
    if (m_command_line_path.empty ())
      m_command_line_path = p;
    else
      m_command_line_path += octave::directory_path::path_sep_str () + p;
  }

  std::string get_command_line_path (void) const { return m_command_line_path; }

  std::string system_path (void) const { return sys_path; }

private:

  static const int M_FILE = 1;
  static const int OCT_FILE = 2;
  static const int MEX_FILE = 4;

  class dir_info
  {
  public:

    // <FCN_NAME, TYPE>
    typedef std::map<std::string, int> fcn_file_map_type;

    typedef fcn_file_map_type::const_iterator const_fcn_file_map_iterator;
    typedef fcn_file_map_type::iterator fcn_file_map_iterator;

    struct class_info
    {
      class_info (void) : method_file_map (), private_file_map () { }

      class_info (const class_info& ci)
        : method_file_map (ci.method_file_map),
          private_file_map (ci.private_file_map) { }

      class_info& operator = (const class_info& ci)
      {
        if (this != &ci)
          {
            method_file_map = ci.method_file_map;
            private_file_map = ci.private_file_map;
          }
        return *this;
      }

      ~class_info (void) = default;

      fcn_file_map_type method_file_map;
      fcn_file_map_type private_file_map;
    };

    // <CLASS_NAME, CLASS_INFO>
    typedef std::map<std::string, class_info> method_file_map_type;

    typedef method_file_map_type::const_iterator const_method_file_map_iterator;
    typedef method_file_map_type::iterator method_file_map_iterator;

    // <PACKAGE_NAME, DIR_INFO>
    typedef std::map<std::string, dir_info> package_dir_map_type;

    typedef package_dir_map_type::const_iterator const_package_dir_map_iterator;
    typedef package_dir_map_type::iterator package_dir_map_iterator;

    // This default constructor is only provided so we can create a
    // std::map of dir_info objects.  You should not use this
    // constructor for any other purpose.
    dir_info (void) = default;

    dir_info (const std::string& d)
      : dir_name (d), abs_dir_name (), is_relative (false),
        dir_mtime (), dir_time_last_checked (), all_files (), fcn_files (),
        private_file_map (), method_file_map (), package_dir_map ()
    {
      initialize ();
    }

    dir_info (const dir_info& di) = default;

    ~dir_info (void) = default;

    dir_info& operator = (const dir_info& di) = default;

    void update (void);

    std::string dir_name;
    std::string abs_dir_name;
    bool is_relative;
    octave::sys::time dir_mtime;
    octave::sys::time dir_time_last_checked;
    string_vector all_files;
    string_vector fcn_files;
    fcn_file_map_type private_file_map;
    method_file_map_type method_file_map;
    package_dir_map_type package_dir_map;

    bool is_package (const std::string& name) const;

  private:

    void initialize (void);

    void get_file_list (const std::string& d);

    void get_private_file_map (const std::string& d);

    void get_method_file_map (const std::string& d,
                              const std::string& class_name);

    void get_package_dir (const std::string& d,
                          const std::string& package_name);

    friend fcn_file_map_type get_fcn_files (const std::string& d);
  };

  class file_info
  {
  public:

    file_info (const std::string& d, int t) : dir_name (d), types (t) { }

    file_info (const file_info& fi)
      : dir_name (fi.dir_name), types (fi.types) { }

    ~file_info (void) = default;

    file_info& operator = (const file_info& fi)
    {
      if (&fi != this)
        {
          dir_name = fi.dir_name;
          types = fi.types;
        }

      return *this;
    }

    std::string dir_name;
    int types;
  };

  // We maintain two ways of looking at the same information.
  //
  // First, a list of directories and the set of "public" files and
  // private files (those found in the special "private" subdirectory)
  // in each directory.
  //
  // Second, a map from filenames (the union of all "public" files for all
  // directories, but without filename extensions) to a list of
  // corresponding information (directory name and file types).  This
  // way, we can quickly find shadowed filenames and look up all
  // overloaded functions (in the "@" directories used to implement
  // classes).

  typedef std::list<dir_info> dir_info_list_type;

  typedef dir_info_list_type::const_iterator const_dir_info_list_iterator;
  typedef dir_info_list_type::iterator dir_info_list_iterator;

  typedef std::map<std::string, dir_info> abs_dir_cache_type;

  typedef abs_dir_cache_type::const_iterator const_abs_dir_cache_iterator;
  typedef abs_dir_cache_type::iterator abs_dir_cache_iterator;

  typedef std::list<file_info> file_info_list_type;

  typedef file_info_list_type::const_iterator const_file_info_list_iterator;
  typedef file_info_list_type::iterator file_info_list_iterator;

  // <FCN_NAME, FILE_INFO_LIST>
  typedef std::map<std::string, file_info_list_type> fcn_map_type;

  typedef fcn_map_type::const_iterator const_fcn_map_iterator;
  typedef fcn_map_type::iterator fcn_map_iterator;

  // <DIR_NAME, <FCN_NAME, TYPE>>
  typedef std::map<std::string, dir_info::fcn_file_map_type>
    private_fcn_map_type;

  typedef private_fcn_map_type::const_iterator const_private_fcn_map_iterator;
  typedef private_fcn_map_type::iterator private_fcn_map_iterator;

  // <CLASS_NAME, <FCN_NAME, FILE_INFO_LIST>>
  typedef std::map<std::string, fcn_map_type> method_map_type;

  typedef method_map_type::const_iterator const_method_map_iterator;
  typedef method_map_type::iterator method_map_iterator;

  class package_info
  {
  public:

    package_info (const std::string& package_name = "")
      : m_package_name (package_name), dir_list (), fcn_map (), private_fcn_map (),
        method_map () { }

    package_info (const package_info& l)
      : m_package_name (l.m_package_name), dir_list (l.dir_list),
        private_fcn_map (l.private_fcn_map), method_map (l.method_map) { }

    ~package_info (void) = default;

    package_info& operator = (const package_info& l)
    {
      if (&l != this)
        {
          m_package_name = l.m_package_name;
          dir_list = l.dir_list;
          fcn_map = l.fcn_map;
          private_fcn_map = l.private_fcn_map;
          method_map = l.method_map;
        }

      return *this;
    }

    void add (const dir_info& di, bool at_end, bool updating)
    {
      if (at_end)
        dir_list.push_back (di.dir_name);
      else
        dir_list.push_front (di.dir_name);

      add_to_fcn_map (di, at_end, updating);

      add_to_private_fcn_map (di);

      add_to_method_map (di, at_end);
    }

    void move (const dir_info& di, bool at_end);

    void remove (const dir_info& di);

    void clear (void)
    {
      dir_list.clear ();

      fcn_map.clear ();

      private_fcn_map.clear ();

      method_map.clear ();
    }

    void display (std::ostream& out) const;

    std::string find_fcn (const std::string& fcn,
                          std::string& dir_name,
                          int type = M_FILE | OCT_FILE | MEX_FILE) const;

    std::string find_private_fcn (const std::string& dir,
                                  const std::string& fcn,
                                  int type = M_FILE | OCT_FILE | MEX_FILE) const;

    std::string find_method (const std::string& class_name,
                             const std::string& meth,
                             std::string& dir_name,
                             int type = M_FILE | OCT_FILE | MEX_FILE) const;

    std::list<std::string> methods (const std::string& class_name) const;

    void overloads (const std::string& meth, std::list<std::string>& l) const;

    string_vector fcn_names (void) const;

  private:

    void add_to_fcn_map (const dir_info& di, bool at_end, bool updating);

    void add_to_private_fcn_map (const dir_info& di);

    void add_to_method_map (const dir_info& di, bool at_end);

    void move_fcn_map (const std::string& dir,
                       const string_vector& fcn_files, bool at_end);

    void move_method_map (const std::string& dir, bool at_end);

    void remove_fcn_map (const std::string& dir,
                         const string_vector& fcn_files);

    void remove_private_fcn_map (const std::string& dir);

    void remove_method_map (const std::string& dir);

    bool check_file_type (std::string& fname, int type, int possible_types,
                          const std::string& fcn, const char *who) const;

    void print_types (std::ostream& os, int types) const;

    void print_fcn_list (std::ostream& os,
                         const dir_info::fcn_file_map_type& lst) const;

    std::string m_package_name;

    std::list<std::string> dir_list;

    fcn_map_type fcn_map;

    private_fcn_map_type private_fcn_map;

    method_map_type method_map;
  };

  // <PACKAGE_NAME, PACKAGE_INFO>
  typedef std::map<std::string, package_info> package_map_type;

  typedef package_map_type::const_iterator const_package_map_iterator;
  typedef package_map_type::iterator package_map_iterator;

  mutable package_map_type package_map;

  mutable package_info top_level_package;

  mutable dir_info_list_type dir_info_list;

  mutable std::set<std::string> init_dirs;

  std::string m_command_line_path;

  static std::string sys_path;

  static abs_dir_cache_type abs_dir_cache;

  hook_fcn_ptr add_hook;

  hook_fcn_ptr remove_hook;

  const_dir_info_list_iterator find_dir_info (const std::string& dir) const;
  dir_info_list_iterator find_dir_info (const std::string& dir);

  bool contains (const std::string& dir) const;

  void move (dir_info_list_iterator i, bool at_end);

  void move (const dir_info& di, bool at_end, const std::string& pname = "");

  void remove (const dir_info& di, const std::string& pname = "");

  void add (const std::string& dir, bool at_end, bool warn);

  void add (const dir_info& di, bool at_end, const std::string& pname = "",
            bool updating = false) const;

  bool is_package (const std::string& name) const;

  package_info& get_package (const std::string& name) const
  {
    if (! name.empty () && is_package (name))
      {
        package_map_iterator l = package_map.find (name);

        if (l == package_map.end ())
          l = package_map.insert (package_map.end (),
                                  package_map_type::value_type (name, package_info (name)));

        return l->second;
      }

    return top_level_package;
  }

  string_vector get_file_list (const dir_info::fcn_file_map_type& lst) const;

  friend dir_info::fcn_file_map_type get_fcn_files (const std::string& d);
};

extern std::string
genpath (const std::string& dir, const string_vector& skip = "private");

#endif
