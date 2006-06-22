/*

Copyright (C) 2006 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_load_path_h)
#define octave_load_path_h 1

#include <iostream>
#include <list>
#include <map>
#include <string>

#include "pathsearch.h"
#include "str-vec.h"

class
load_path
{
protected:

  load_path (void) : dir_info_list (), fcn_map () { }

public:

  typedef void (*hook_function_ptr) (const std::string& dir);

  ~load_path (void) { }

  static void initialize (void)
  {
    if (instance_ok ())
      instance->do_initialize ();
  }

  static void clear (void)
  {
    if (instance_ok ())
      instance->do_clear ();
  }

  static void set (const std::string& p)
  {
    if (instance_ok ())
      instance->do_set (p);
  }

  static void append (const std::string& dir)
  {
    if (instance_ok ())
      instance->do_append (dir);
  }

  static void prepend (const std::string& dir)
  {
    if (instance_ok ())
      instance->do_prepend (dir);
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

  static std::string find_fcn (const std::string& fcn)
  {
    return instance_ok ()
      ? instance->do_find_fcn (fcn) : std::string ();
  }

  static std::string find_fcn_file (const std::string& fcn)
  {
    return instance_ok () ?
      instance->do_find_fcn (fcn, M_FILE) : std::string ();
  }

  static std::string find_oct_file (const std::string& fcn)
  {
    return instance_ok () ?
      instance->do_find_fcn (fcn, OCT_FILE) : std::string ();
  }

  static std::string find_mex_file (const std::string& fcn)
  {
    return instance_ok () ?
      instance->do_find_fcn (fcn, MEX_FILE) : std::string ();
  }

  static std::string find_file (const std::string& file)
  {
    return instance_ok ()
      ? instance->do_find_file (file) : std::string ();
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

  static string_vector files (const std::string& dir)
  {
    return instance_ok () ? instance->do_files (dir) : string_vector ();
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

  static void set_add_hook (hook_function_ptr f) { add_hook = f; }

  static void set_remove_hook (hook_function_ptr f) { remove_hook = f; }

  static void set_command_line_path (const std::string& p)
  {
    if (command_line_path.empty ())
      command_line_path = p;
    else
      command_line_path += dir_path::path_sep_str + p;
  }

private:

  static const int M_FILE = 1;
  static const int OCT_FILE = 2;
  static const int MEX_FILE = 4;

  class dir_info
  {
  public:

    dir_info (const std::string& d) : dir_name (d) { initialize (); }

    dir_info (const dir_info& di)
      : dir_name (di.dir_name), is_relative (di.is_relative),
	dir_mtime (di.dir_mtime), all_files (di.all_files),
	fcn_files (di.fcn_files),
	private_function_map (di.private_function_map) { }

    ~dir_info (void) { }

    dir_info& operator = (const dir_info& di)
    {
      if (&di != this)
	{
	  dir_name = di.dir_name;
	  is_relative = di.is_relative;
	  dir_mtime = di.dir_mtime;
	  all_files = di.all_files;
	  fcn_files = di.fcn_files;
	  private_function_map = di.private_function_map;
	}

      return *this;
    }

    void update (void);

    std::string dir_name;
    bool is_relative;
    octave_time dir_mtime;
    string_vector all_files;
    string_vector fcn_files;
    std::map<std::string, int> private_function_map;

  private:

    void initialize (void);

    bool get_file_list (const std::string& d);

    void get_private_function_map (const std::string& d);
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
  // directories, but without filename exteinsions) to a list of
  // corresponding information (directory name and file types).  This
  // way, we can quickly find shadowed file names and look up all
  // overloaded functions (in the "@" directories used to implement
  // classes).

  mutable std::list<dir_info> dir_info_list;

  mutable std::map<std::string, std::list<file_info> > fcn_map;

  static load_path *instance;

  static hook_function_ptr add_hook;

  static hook_function_ptr remove_hook;

  static std::string command_line_path;

  static bool instance_ok (void);

  typedef std::list<dir_info>::const_iterator const_dir_info_list_iterator;
  typedef std::list<dir_info>::iterator dir_info_list_iterator;

  typedef std::map<std::string, std::list<file_info> >::const_iterator const_fcn_map_iterator;
  typedef std::map<std::string, std::list<file_info> >::iterator fcn_map_iterator;

  typedef std::list<file_info>::const_iterator const_file_info_list_iterator;
  typedef std::list<file_info>::iterator file_info_list_iterator;

  const_dir_info_list_iterator find_dir_info (const std::string& dir) const;
  dir_info_list_iterator find_dir_info (const std::string& dir);

  bool contains (const std::string& dir) const;

  void move (std::list<dir_info>::iterator i, bool at_end);

  void do_initialize (void);

  void do_clear (void);

  void do_set (const std::string& p);

  void do_append (const std::string& dir);

  void do_prepend (const std::string& dir);

  bool do_remove (const std::string& dir);

  void do_update (void) const;

  std::string do_find_fcn (const std::string& fcn,
			   int type = M_FILE | OCT_FILE | MEX_FILE) const;

  std::string do_find_file (const std::string& file) const;

  std::string do_find_first_of (const string_vector& files) const;

  string_vector do_find_all_first_of (const string_vector& files) const;

  string_vector do_dirs (void) const;

  std::list<std::string> do_dir_list (void) const;

  string_vector do_files (const std::string& dir) const;

  string_vector do_fcn_names (void) const;

  std::string do_path (void) const;

  void do_display (std::ostream& os) const;

  void add_to_fcn_map (const dir_info& di, bool at_end) const;
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
