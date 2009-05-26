/*

Copyright (C) 2006, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_load_path_h)
#define octave_load_path_h 1

#include <iosfwd>
#include <list>
#include <map>
#include <string>

#include "pathsearch.h"
#include "str-vec.h"

class
OCTINTERP_API
load_path
{
protected:

  load_path (void)
    : dir_info_list (), fcn_map (), method_map (), parent_map () { }

public:

  typedef void (*hook_fcn_ptr) (const std::string& dir);

  ~load_path (void) { }

  static void initialize (bool set_initial_path = false)
  {
    if (instance_ok ())
      instance->do_initialize (set_initial_path);
  }

  static void clear (void)
  {
    if (instance_ok ())
      instance->do_clear ();
  }

  static void set (const std::string& p, bool warn = false)
  {
    if (instance_ok ())
      instance->do_set (p, warn);
  }

  static void append (const std::string& dir, bool warn = false)
  {
    if (instance_ok ())
      instance->do_append (dir, warn);
  }

  static void prepend (const std::string& dir, bool warn = false)
  {
    if (instance_ok ())
      instance->do_prepend (dir, warn);
  }

  static bool remove (const std::string& dir)
  {
    return instance_ok () ? instance->do_remove (dir) : false;
  }

  static void update (void)
  {
    if (instance_ok ())
      instance->do_update ();
  }

  static std::string find_method (const std::string& class_name,
				  const std::string& meth,
				  std::string& dir_name)
  {
    return instance_ok ()
      ? instance->do_find_method (class_name, meth, dir_name) : std::string ();
  }

  static std::string find_method (const std::string& class_name,
				  const std::string& meth)
  {
    std::string dir_name;
    return find_method (class_name, meth, dir_name);
  }

  static std::list<std::string> methods (const std::string& class_name)
  {
    return instance_ok ()
      ? instance->do_methods (class_name) : std::list<std::string> ();
  }

  static std::string find_fcn (const std::string& fcn, std::string& dir_name)
  {
    return instance_ok ()
      ? instance->do_find_fcn (fcn, dir_name) : std::string ();
  }

  static std::string find_fcn (const std::string& fcn)
  {
    std::string dir_name;
    return find_fcn (fcn, dir_name);
  }

  static std::string find_private_fcn (const std::string& dir,
				       const std::string& fcn)
  {
    return instance_ok ()
      ? instance->do_find_private_fcn (dir, fcn) : std::string ();
  }

  static std::string find_fcn_file (const std::string& fcn)
  {
    std::string dir_name;

    return instance_ok () ?
      instance->do_find_fcn (fcn, dir_name, M_FILE) : std::string ();
  }

  static std::string find_oct_file (const std::string& fcn)
  {
    std::string dir_name;

    return instance_ok () ?
      instance->do_find_fcn (fcn, dir_name, OCT_FILE) : std::string ();
  }

  static std::string find_mex_file (const std::string& fcn)
  {
    std::string dir_name;

    return instance_ok () ?
      instance->do_find_fcn (fcn, dir_name, MEX_FILE) : std::string ();
  }

  static std::string find_file (const std::string& file)
  {
    return instance_ok ()
      ? instance->do_find_file (file) : std::string ();
  }

  static std::string find_dir (const std::string& dir)
  {
    return instance_ok ()
      ? instance->do_find_dir (dir) : std::string ();
  }

  static std::string find_first_of (const string_vector& files)
  {
    return instance_ok () ?
      instance->do_find_first_of (files) : std::string ();
  }

  static string_vector find_all_first_of (const string_vector& files)
  {
    return instance_ok () ?
      instance->do_find_all_first_of (files) : string_vector ();
  }

  static string_vector dirs (void)
  {
    return instance_ok () ? instance->do_dirs () : string_vector ();
  }

  static std::list<std::string> dir_list (void)
  {
    return instance_ok ()
      ? instance->do_dir_list () : std::list<std::string> ();
  }

  static string_vector files (const std::string& dir, bool omit_exts = false)
  {
    return instance_ok ()
      ? instance->do_files (dir, omit_exts) : string_vector ();
  }

  static string_vector fcn_names (void)
  {
    return instance_ok () ? instance->do_fcn_names () : string_vector ();
  }

  static std::string path (void)
  {
    return instance_ok () ? instance->do_path () : std::string ();
  }

  static void display (std::ostream& os)
  {
    if (instance_ok ())
      instance->do_display (os);
  }

  static void set_add_hook (hook_fcn_ptr f) { add_hook = f; }

  static void set_remove_hook (hook_fcn_ptr f) { remove_hook = f; }

  static void set_command_line_path (const std::string& p)
  {
    if (command_line_path.empty ())
      command_line_path = p;
    else
      command_line_path += dir_path::path_sep_str () + p;
  }

  static std::string get_command_line_path (void)
  {
    return instance_ok () ? instance->do_get_command_line_path () : std::string ();
  }

  static std::string system_path (void)
  {
    return instance_ok () ? instance->do_system_path () : std::string ();
  }

  static void add_to_parent_map (const std::string& classname,
				 const std::list<std::string>& parent_list)
  {
    if (instance_ok ())
      instance->do_add_to_parent_map (classname, parent_list);
  }

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
      fcn_file_map_type method_file_map;
      fcn_file_map_type private_file_map;
    };

    // <CLASS_NAME, CLASS_INFO>
    typedef std::map<std::string, class_info> method_file_map_type;

    typedef method_file_map_type::const_iterator const_method_file_map_iterator;
    typedef method_file_map_type::iterator method_file_map_iterator;

    // This default constructor is only provided so we can create a
    // std::map of dir_info objects.  You should not use this
    // constructor for any other purpose.
    dir_info (void) { }

    dir_info (const std::string& d) : dir_name (d) { initialize (); }

    dir_info (const dir_info& di)
      : dir_name (di.dir_name), abs_dir_name (di.abs_dir_name),
	is_relative (di.is_relative),
	dir_mtime (di.dir_mtime), all_files (di.all_files),
	fcn_files (di.fcn_files),
	private_file_map (di.private_file_map),
	method_file_map (di.method_file_map) { }

    ~dir_info (void) { }

    dir_info& operator = (const dir_info& di)
    {
      if (&di != this)
	{
	  dir_name = di.dir_name;
	  abs_dir_name = di.abs_dir_name;
	  is_relative = di.is_relative;
	  dir_mtime = di.dir_mtime;
	  all_files = di.all_files;
	  fcn_files = di.fcn_files;
	  private_file_map = di.private_file_map;
	  method_file_map = di.method_file_map;
	}

      return *this;
    }

    void update (void);

    std::string dir_name;
    std::string abs_dir_name;
    bool is_relative;
    octave_time dir_mtime;
    string_vector all_files;
    string_vector fcn_files;
    fcn_file_map_type private_file_map;
    method_file_map_type method_file_map;

  private:

    void initialize (void);

    void get_file_list (const std::string& d);

    void get_private_file_map (const std::string& d);

    void get_method_file_map (const std::string& d,
			      const std::string& class_name);

    friend fcn_file_map_type get_fcn_files (const std::string& d);
  };

  class file_info
  {
  public:

    file_info (const std::string& d, int t) : dir_name (d), types (t) { }

    file_info (const file_info& fi)
      : dir_name (fi.dir_name), types (fi.types) { }

    ~file_info (void) { }

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
  // Second, a map from file names (the union of all "public" files for all
  // directories, but without filename extensions) to a list of
  // corresponding information (directory name and file types).  This
  // way, we can quickly find shadowed file names and look up all
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
  typedef std::map<std::string, dir_info::fcn_file_map_type> private_fcn_map_type;

  typedef private_fcn_map_type::const_iterator const_private_fcn_map_iterator;
  typedef private_fcn_map_type::iterator private_fcn_map_iterator;

  // <CLASS_NAME, <FCN_NAME, FILE_INFO_LIST>>
  typedef std::map<std::string, fcn_map_type> method_map_type;

  typedef method_map_type::const_iterator const_method_map_iterator;
  typedef method_map_type::iterator method_map_iterator;
 
  // <CLASS_NAME, PARENT_LIST>>
  typedef std::map<std::string, std::list<std::string> > parent_map_type;

  typedef parent_map_type::const_iterator const_parent_map_iterator;
  typedef parent_map_type::iterator parent_map_iterator;

  mutable dir_info_list_type dir_info_list;

  mutable fcn_map_type fcn_map;

  mutable private_fcn_map_type private_fcn_map;

  mutable method_map_type method_map;

  mutable parent_map_type parent_map;

  static load_path *instance;

  static hook_fcn_ptr add_hook;

  static hook_fcn_ptr remove_hook;

  static std::string command_line_path;

  static std::string sys_path;

  static abs_dir_cache_type abs_dir_cache;

  static bool instance_ok (void);

  const_dir_info_list_iterator find_dir_info (const std::string& dir) const;
  dir_info_list_iterator find_dir_info (const std::string& dir);

  bool contains (const std::string& dir) const;

  void move_fcn_map (const std::string& dir,
		     const string_vector& fcn_files, bool at_end);

  void move_method_map (const std::string& dir, bool at_end);

  void move (std::list<dir_info>::iterator i, bool at_end);

  void do_initialize (bool set_initial_path);

  void do_clear (void);

  void do_set (const std::string& p, bool warn);

  void do_append (const std::string& dir, bool warn);

  void do_prepend (const std::string& dir, bool warn);

  void do_add (const std::string& dir, bool at_end, bool warn);

  void remove_fcn_map (const std::string& dir, const string_vector& fcn_files);

  void remove_private_fcn_map (const std::string& dir);

  void remove_method_map (const std::string& dir);

  bool do_remove (const std::string& dir);

  void do_update (void) const;

  static bool
  check_file_type (std::string& fname, int type, int possible_types,
		   const std::string& fcn, const char *who);

  std::string do_find_fcn (const std::string& fcn,
			   std::string& dir_name,
			   int type = M_FILE | OCT_FILE | MEX_FILE) const;

  std::string do_find_private_fcn (const std::string& dir,
				   const std::string& fcn,
				   int type = M_FILE | OCT_FILE | MEX_FILE) const;

  std::string do_find_method (const std::string& class_name,
			      const std::string& meth,
			      std::string& dir_name,
			      int type = M_FILE | OCT_FILE | MEX_FILE) const;

  std::list<std::string> do_methods (const std::string& class_name) const;

  std::string do_find_file (const std::string& file) const;

  std::string do_find_dir (const std::string& dir) const;

  std::string do_find_first_of (const string_vector& files) const;

  string_vector do_find_all_first_of (const string_vector& files) const;

  string_vector do_dirs (void) const;

  std::list<std::string> do_dir_list (void) const;

  string_vector do_files (const std::string& dir, bool omit_exts) const;

  string_vector do_fcn_names (void) const;

  std::string do_path (void) const;

  friend void print_types (std::ostream& os, int types);

  friend string_vector get_file_list (const dir_info::fcn_file_map_type& lst);

  friend void
  print_fcn_list (std::ostream& os, const dir_info::fcn_file_map_type& lst);

  void do_display (std::ostream& os) const;

  std::string do_system_path (void) const { return sys_path; }

  std::string do_get_command_line_path (void) const { return command_line_path; }

  void do_add_to_parent_map (const std::string& classname,
			     const std::list<std::string>& parent_list) const;

  void add_to_fcn_map (const dir_info& di, bool at_end) const;

  void add_to_private_fcn_map (const dir_info& di) const;

  void add_to_method_map (const dir_info& di, bool at_end) const;

  friend dir_info::fcn_file_map_type get_fcn_files (const std::string& d);
};

extern std::string
genpath (const std::string& dir, const string_vector& skip = "private");

extern void execute_pkg_add (const std::string& dir);
extern void execute_pkg_del (const std::string& dir);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
