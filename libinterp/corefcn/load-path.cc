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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <algorithm>

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "pathsearch.h"
#include "singleton-cleanup.h"

#include "defaults.h"
#include "defun.h"
#include "input.h"
#include "interpreter.h"
#include "load-path.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "unwind-prot.h"
#include "utils.h"

load_path *load_path::instance = 0;
load_path::hook_fcn_ptr load_path::add_hook = load_path::execute_pkg_add;
load_path::hook_fcn_ptr load_path::remove_hook = load_path::execute_pkg_del;
std::string load_path::command_line_path;
std::string load_path::sys_path;
load_path::abs_dir_cache_type load_path::abs_dir_cache;

void
load_path::dir_info::update (void)
{
  octave::sys::file_stat fs (dir_name);

  if (! fs)
    {
      std::string msg = fs.error ();
      warning ("load_path: %s: %s", dir_name.c_str (), msg.c_str ());
    }
  else
    {
      if (is_relative)
        {
          try
            {
              std::string abs_name = octave::sys::env::make_absolute (dir_name);

              abs_dir_cache_iterator p = abs_dir_cache.find (abs_name);

              if (p != abs_dir_cache.end ())
                {
                  // The directory is in the cache of all directories we have
                  // visited (indexed by absolute name).  If it is out of date,
                  // initialize it.  Otherwise, copy the info from the cache.
                  // By doing that, we avoid unnecessary calls to stat that can
                  // slow things down tremendously for large directories.
                  const dir_info& di = p->second;

                  if (fs.mtime () + fs.time_resolution ()
                      > di.dir_time_last_checked)
                    initialize ();
                  else
                    {
                      // Copy over info from cache, but leave dir_name and
                      // is_relative unmodified.
                      this->abs_dir_name = di.abs_dir_name;
                      this->dir_mtime = di.dir_mtime;
                      this->dir_time_last_checked = di.dir_time_last_checked;
                      this->all_files = di.all_files;
                      this->fcn_files = di.fcn_files;
                      this->private_file_map = di.private_file_map;
                      this->method_file_map = di.method_file_map;
                      this->package_dir_map = di.package_dir_map;
                    }
                }
              else
                {
                  // We haven't seen this directory before.
                  initialize ();
                }
            }
          catch (const octave::execution_exception&)
            {
              // Skip updating if we don't know where we are,
              // but don't treat it as an error.
              octave::interpreter::recover_from_exception ();
            }
        }
      // Absolute path, check timestamp to see whether it requires re-caching
      else if (fs.mtime () + fs.time_resolution () > dir_time_last_checked)
        initialize ();
    }
}

bool
load_path::dir_info::is_package (const std::string& name) const
{
  size_t pos = name.find ('.');

  if (pos == std::string::npos)
    return package_dir_map.find (name) != package_dir_map.end ();
  else
    {
      std::string name_head = name.substr (0, pos);
      std::string name_tail = name.substr (pos + 1);

      const_package_dir_map_iterator it = package_dir_map.find (name_head);

      if (it != package_dir_map.end ())
        return it->second.is_package (name_tail);
      else
        return false;
    }
}

void
load_path::dir_info::initialize (void)
{
  is_relative = ! octave::sys::env::absolute_pathname (dir_name);

  dir_time_last_checked = octave::sys::time (static_cast<time_t> (0));

  octave::sys::file_stat fs (dir_name);

  if (fs)
    {
      method_file_map.clear ();
      package_dir_map.clear ();

      dir_mtime = fs.mtime ();
      dir_time_last_checked = octave::sys::time ();

      get_file_list (dir_name);

      try
        {
          std::string abs_name = octave::sys::env::make_absolute (dir_name);

          // FIXME: nothing is ever removed from this cache of
          // directory information, so there could be some resource
          // problems.  Perhaps it should be pruned from time to time.

          abs_dir_cache[abs_name] = *this;
        }
      catch (const octave::execution_exception&)
        {
          // Skip updating if we don't know where we are but don't treat
          // it as an error.

          octave::interpreter::recover_from_exception ();
        }
    }
  else
    {
      std::string msg = fs.error ();
      warning ("load_path: %s: %s", dir_name.c_str (), msg.c_str ());
    }
}

void
load_path::dir_info::get_file_list (const std::string& d)
{
  octave::sys::dir_entry dir (d);

  if (dir)
    {
      string_vector flist = dir.read ();

      octave_idx_type len = flist.numel ();

      all_files.resize (len);
      fcn_files.resize (len);

      octave_idx_type all_files_count = 0;
      octave_idx_type fcn_files_count = 0;

      for (octave_idx_type i = 0; i < len; i++)
        {
          std::string fname = flist[i];

          std::string full_name = octave::sys::file_ops::concat (d, fname);

          octave::sys::file_stat fs (full_name);

          if (fs)
            {
              if (fs.is_dir ())
                {
                  if (fname == "private")
                    get_private_file_map (full_name);
                  else if (fname[0] == '@')
                    get_method_file_map (full_name, fname.substr (1));
                  else if (fname[0] == '+')
                    get_package_dir (full_name, fname.substr (1));
                }
              else
                {
                  all_files[all_files_count++] = fname;

                  size_t pos = fname.rfind ('.');

                  if (pos != std::string::npos)
                    {
                      std::string ext = fname.substr (pos);

                      if (ext == ".m" || ext == ".oct" || ext == ".mex")
                        {
                          std::string base = fname.substr (0, pos);

                          if (valid_identifier (base))
                            fcn_files[fcn_files_count++] = fname;
                        }
                    }
                }
            }
        }

      all_files.resize (all_files_count);
      fcn_files.resize (fcn_files_count);
    }
  else
    {
      std::string msg = dir.error ();
      warning ("load_path: %s: %s", d.c_str (), msg.c_str ());
    }
}

load_path::dir_info::fcn_file_map_type
get_fcn_files (const std::string& d)
{
  load_path::dir_info::fcn_file_map_type retval;

  octave::sys::dir_entry dir (d);

  if (dir)
    {
      string_vector flist = dir.read ();

      octave_idx_type len = flist.numel ();

      for (octave_idx_type i = 0; i < len; i++)
        {
          std::string fname = flist[i];

          size_t pos = fname.rfind ('.');

          if (pos != std::string::npos)
            {
              std::string base = fname.substr (0, pos);
              std::string ext = fname.substr (pos);

              if (valid_identifier (base))
                {
                  int t = 0;

                  if (ext == ".m")
                    t = load_path::M_FILE;
                  else if (ext == ".oct")
                    t = load_path::OCT_FILE;
                  else if (ext == ".mex")
                    t = load_path::MEX_FILE;

                  if (t)
                    retval[base] = t;
                }
            }
        }
    }
  else
    {
      std::string msg = dir.error ();
      warning ("load_path: %s: %s", d.c_str (), msg.c_str ());
    }

  return retval;
}

void
load_path::dir_info::get_private_file_map (const std::string& d)
{
  private_file_map = get_fcn_files (d);
}

void
load_path::dir_info::get_method_file_map (const std::string& d,
                                          const std::string& class_name)
{
  method_file_map[class_name].method_file_map = get_fcn_files (d);

  std::string pd = octave::sys::file_ops::concat (d, "private");

  octave::sys::file_stat fs (pd);

  if (fs && fs.is_dir ())
    method_file_map[class_name].private_file_map = get_fcn_files (pd);
}

void
load_path::dir_info::get_package_dir (const std::string& d,
                                      const std::string& package_name)
{
  package_dir_map[package_name] = dir_info (d);
}

bool
load_path::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new load_path ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    error ("unable to create load path object!");

  return retval;
}

// FIXME: maybe we should also maintain a map to speed up this method of access.

load_path::const_dir_info_list_iterator
load_path::find_dir_info (const std::string& dir_arg) const
{
  std::string dir = octave::sys::file_ops::tilde_expand (dir_arg);

  const_dir_info_list_iterator retval = dir_info_list.begin ();

  while (retval != dir_info_list.end ())
    {
      if (retval->dir_name == dir)
        break;

      retval++;
    }

  return retval;
}

load_path::dir_info_list_iterator
load_path::find_dir_info (const std::string& dir_arg)
{
  std::string dir = octave::sys::file_ops::tilde_expand (dir_arg);

  dir_info_list_iterator retval = dir_info_list.begin ();

  while (retval != dir_info_list.end ())
    {
      if (retval->dir_name == dir)
        break;

      retval++;
    }

  return retval;
}

bool
load_path::contains (const std::string& dir) const
{
  return find_dir_info (dir) != dir_info_list.end ();
}

bool
load_path::do_contains_canonical (const std::string& dir) const
{
  bool retval = false;

  for (const auto& d : dir_info_list)
    {
      if (same_file (dir, d.dir_name))
        {
          retval = true;
          break;
        }
    }

  return retval;
}

void
load_path::package_info::move_fcn_map (const std::string& dir_name,
                                 const string_vector& fcn_files, bool at_end)
{
  octave_idx_type len = fcn_files.numel ();

  for (octave_idx_type k = 0; k < len; k++)
    {
      std::string fname = fcn_files[k];

      std::string ext;
      std::string base = fname;

      size_t pos = fname.rfind ('.');

      if (pos != std::string::npos)
        {
          base = fname.substr (0, pos);
          ext = fname.substr (pos);
        }

      file_info_list_type& file_info_list = fcn_map[base];

      if (file_info_list.size () == 1)
        continue;
      else
        {
          for (auto fi_it = file_info_list.begin ();
               fi_it != file_info_list.end ();
               fi_it++)
            {
              if (fi_it->dir_name == dir_name)
                {
                  file_info fi_tmp = *fi_it;

                  file_info_list.erase (fi_it);

                  if (at_end)
                    file_info_list.push_back (fi_tmp);
                  else
                    file_info_list.push_front (fi_tmp);

                  break;
                }
            }
        }
    }
}

void
load_path::package_info::move_method_map (const std::string& dir_name, bool at_end)
{
  for (auto& cls_fnmap : method_map)
    {
      std::string class_name = cls_fnmap.first;

      fcn_map_type& fn_map = cls_fnmap.second;

      std::string full_dir_name
        = octave::sys::file_ops::concat (dir_name, "@" + class_name);

      for (auto& nm_filst : fn_map)
        {
          file_info_list_type& file_info_list = nm_filst.second;

          if (file_info_list.size () == 1)
            continue;
          else
            {
              for (auto fi_it = file_info_list.begin ();
                   fi_it != file_info_list.end (); fi_it++)
                {
                  if (fi_it->dir_name == full_dir_name)
                    {
                      file_info fi_tmp = *fi_it;

                      file_info_list.erase (fi_it);

                      if (at_end)
                        file_info_list.push_back (fi_tmp);
                      else
                        file_info_list.push_front (fi_tmp);

                      break;
                    }
                }
            }
        }
    }
}

void
load_path::do_move (dir_info_list_iterator i, bool at_end)
{
  if (dir_info_list.size () > 1)
    {
      dir_info di = *i;

      dir_info_list.erase (i);

      if (at_end)
        dir_info_list.push_back (di);
      else
        dir_info_list.push_front (di);

      move (di, at_end);
    }
}

void
load_path::move (const dir_info& di, bool at_end, const std::string& pname)
{
  package_info& pkg = get_package (pname);

  pkg.move (di, at_end);

  dir_info::package_dir_map_type package_dir_map = di.package_dir_map;

  for (const auto& pkg_di : package_dir_map)
    {
      std::string full_name = pkg_di.first;

      if (! pname.empty ())
        full_name = pname + "." + full_name;

      move (pkg_di.second, at_end, full_name);
    }
}

void
load_path::package_info::move (const dir_info& di, bool at_end)
{
  std::string dir_name = di.dir_name;

  std::list<std::string>::iterator s =
    std::find (dir_list.begin (), dir_list.end (), dir_name);

  if (s != dir_list.end ())
    {
      dir_list.erase (s);

      if (at_end)
        dir_list.push_back (dir_name);
      else
        dir_list.push_front (dir_name);
    }

  move_fcn_map (dir_name, di.fcn_files, at_end);

  // No need to move elements of private function map.

  move_method_map (dir_name, at_end);
}

static void
maybe_add_path_elts (std::string& path, const std::string& dir)
{
  std::string tpath = genpath (dir);

  if (! tpath.empty ())
    {
      if (path.empty ())
        path = tpath;
      else
        path += octave::directory_path::path_sep_str () + tpath;
    }
}

void
load_path::do_initialize (bool set_initial_path)
{
  sys_path = "";

  if (set_initial_path)
    {
      maybe_add_path_elts (sys_path, Vlocal_ver_oct_file_dir);
      maybe_add_path_elts (sys_path, Vlocal_api_oct_file_dir);
      maybe_add_path_elts (sys_path, Vlocal_oct_file_dir);
      maybe_add_path_elts (sys_path, Vlocal_ver_fcn_file_dir);
      maybe_add_path_elts (sys_path, Vlocal_api_fcn_file_dir);
      maybe_add_path_elts (sys_path, Vlocal_fcn_file_dir);
      maybe_add_path_elts (sys_path, Voct_file_dir);
      maybe_add_path_elts (sys_path, Vfcn_file_dir);
      maybe_add_path_elts (sys_path, Voct_data_dir);
    }

  std::string tpath = load_path::command_line_path;

  if (tpath.empty ())
    tpath = octave::sys::env::getenv ("OCTAVE_PATH");

  std::string xpath;

  if (! tpath.empty ())
    {
      xpath = tpath;

      if (! sys_path.empty ())
        xpath += octave::directory_path::path_sep_str () + sys_path;
    }
  else
    xpath = sys_path;

  do_set (xpath, false, true);
}

void
load_path::do_clear (void)
{
  dir_info_list.clear ();

  top_level_package.clear ();

  package_map.clear ();
}

static std::list<std::string>
split_path (const std::string& p)
{
  std::list<std::string> retval;

  size_t beg = 0;
  size_t end = p.find (octave::directory_path::path_sep_char ());

  size_t len = p.length ();

  while (end != std::string::npos)
    {
      std::string elt = p.substr (beg, end-beg);

      if (! elt.empty ())
        retval.push_back (elt);

      beg = end + 1;

      if (beg == len)
        break;

      end = p.find (octave::directory_path::path_sep_char (), beg);
    }

  std::string elt = p.substr (beg);

  if (! elt.empty ())
    retval.push_back (elt);

  return retval;
}

void
load_path::do_set (const std::string& p, bool warn, bool is_init)
{
  // Use a list when we need to preserve order.
  std::list<std::string> elts = split_path (p);

  // Use a set when we need to search and order is not important.
  std::set<std::string> elts_set (elts.begin (), elts.end ());

  if (is_init)
    init_dirs = elts_set;
  else
    {
      for (const auto& init_dir : init_dirs)
        {
          if (elts_set.find (init_dir) == elts_set.end ())
            {
              warning_with_id ("Octave:remove-init-dir",
                               "default load path altered.  Some built-in functions may not be found.  Try restoredefaultpath() to recover it.");
              break;
            }
        }
    }

  // Temporarily disable add hook.

  octave::unwind_protect frame;
  frame.protect_var (add_hook);

  add_hook = 0;

  do_clear ();

  for (const auto& elt : elts)
    do_append (elt, warn);

  // Restore add hook and execute for all newly added directories.
  frame.run_first ();

  // FIXME: Shouldn't the test for add_hook be outside the for loop?
  //        Why not use const here?  Does add_hook change dir_info_list?
  for (auto& di : dir_info_list)
    {
      if (add_hook)
        add_hook (di.dir_name);
    }

  // Always prepend current directory.
  do_prepend (".", warn);
}

void
load_path::do_append (const std::string& dir, bool warn)
{
  if (! dir.empty ())
    do_add (dir, true, warn);
}

void
load_path::do_prepend (const std::string& dir, bool warn)
{
  if (! dir.empty ())
    do_add (dir, false, warn);
}

// Strip trailing directory separators.

static std::string
strip_trailing_separators (const std::string& dir_arg)
{
  std::string dir = dir_arg;

  size_t k = dir.length ();

  while (k > 1 && octave::sys::file_ops::is_dir_sep (dir[k-1]))
    k--;

  if (k < dir.length ())
    dir.resize (k);

  return dir;
}

void
load_path::do_add (const std::string& dir_arg, bool at_end, bool warn)
{
  size_t len = dir_arg.length ();

  if (len > 1 && dir_arg.substr (len-2) == "//")
    warning_with_id ("Octave:recursive-path-search",
                     "trailing '//' is no longer special in search path elements");

  std::string dir = octave::sys::file_ops::tilde_expand (dir_arg);

  dir = strip_trailing_separators (dir);

  dir_info_list_iterator i = find_dir_info (dir);

  if (i != dir_info_list.end ())
    do_move (i, at_end);
  else
    {
      octave::sys::file_stat fs (dir);

      if (fs)
        {
          if (fs.is_dir ())
            {
              dir_info di (dir);

              if (at_end)
                dir_info_list.push_back (di);
              else
                dir_info_list.push_front (di);

              add (di, at_end);

              if (add_hook)
                add_hook (dir);
            }
          else if (warn)
            warning ("addpath: %s: not a directory", dir_arg.c_str ());
        }
      else if (warn)
        {
          std::string msg = fs.error ();
          warning ("addpath: %s: %s", dir_arg.c_str (), msg.c_str ());
        }
    }

  // FIXME: is there a better way to do this?

  i = find_dir_info (".");

  if (i != dir_info_list.end ())
    do_move (i, false);
}

void
load_path::package_info::remove_fcn_map (const std::string& dir,
                                   const string_vector& fcn_files)
{
  octave_idx_type len = fcn_files.numel ();

  for (octave_idx_type k = 0; k < len; k++)
    {
      std::string fname = fcn_files[k];

      std::string ext;
      std::string base = fname;

      size_t pos = fname.rfind ('.');

      if (pos != std::string::npos)
        {
          base = fname.substr (0, pos);
          ext = fname.substr (pos);
        }

      file_info_list_type& file_info_list = fcn_map[base];

      for (auto fi_it = file_info_list.begin ();
           fi_it != file_info_list.end ();
           fi_it++)
        {
          if (fi_it->dir_name == dir)
            {
              file_info_list.erase (fi_it);

              if (file_info_list.empty ())
                fcn_map.erase (fname);

              break;
            }
        }
    }
}

void
load_path::package_info::remove_private_fcn_map (const std::string& dir)
{
  private_fcn_map_iterator p = private_fcn_map.find (dir);

  if (p != private_fcn_map.end ())
    private_fcn_map.erase (p);
}

void
load_path::package_info::remove_method_map (const std::string& dir)
{
  for (auto& cls_fnmap : method_map)
    {
      std::string class_name = cls_fnmap.first;

      fcn_map_type& fn_map = cls_fnmap.second;

      std::string full_dir_name
        = octave::sys::file_ops::concat (dir, "@" + class_name);

      for (auto& nm_filst : fn_map)
        {
          file_info_list_type& file_info_list = nm_filst.second;

          if (file_info_list.size () == 1)
            continue;
          else
            {
              for (auto fi_it = file_info_list.begin ();
                   fi_it != file_info_list.end (); fi_it++)
                {
                  if (fi_it->dir_name == full_dir_name)
                    {
                      file_info_list.erase (fi_it);
                      // FIXME: if there are no other elements, we
                      // should remove this element of fn_map but calling
                      // erase here would invalidate the iterator fi_it.

                      break;
                    }
                }
            }
        }
    }
}

bool
load_path::do_remove (const std::string& dir_arg)
{
  bool retval = false;

  if (! dir_arg.empty ())
    {
      if (dir_arg == ".")
        {
          warning ("rmpath: can't remove \".\" from path");

          // Avoid additional warnings.
          retval = true;
        }
      else
        {
          std::string dir = octave::sys::file_ops::tilde_expand (dir_arg);

          dir = strip_trailing_separators (dir);

          dir_info_list_iterator i = find_dir_info (dir);

          if (i != dir_info_list.end ())
            {
              retval = true;

              if (remove_hook)
                remove_hook (dir);

              dir_info& di = *i;

              remove (di);

              dir_info_list.erase (i);
            }
        }
    }

  return retval;
}

void
load_path::remove (const dir_info& di, const std::string& pname)
{
  package_info& pkg = get_package (pname);

  pkg.remove (di);

  dir_info::package_dir_map_type package_dir_map = di.package_dir_map;

  for (const auto& pkg_di : package_dir_map)
    {
      std::string full_name = pkg_di.first;

      if (! pname.empty ())
        full_name = pname + "." + full_name;

      remove (pkg_di.second, full_name);
    }
}

void
load_path::package_info::remove (const dir_info& di)
{
  std::string dir = di.dir_name;

  string_vector fcn_files = di.fcn_files;

  dir_list.remove (dir);

  remove_fcn_map (dir, fcn_files);

  remove_private_fcn_map (dir);

  remove_method_map (dir);
}

void
load_path::do_update (void) const
{
  // I don't see a better way to do this because we need to
  // preserve the correct directory ordering for new files that
  // have appeared.

  top_level_package.clear ();

  package_map.clear ();

  for (auto& di : dir_info_list)
    {
      di.update ();

      add (di, true, "", true);
    }
}

bool
load_path::check_file_type (std::string& fname, int type, int possible_types,
                            const std::string& fcn, const char *who)
{
  bool retval = false;

  if (type == load_path::OCT_FILE)
    {
      if ((type & possible_types) == load_path::OCT_FILE)
        {
          fname += ".oct";
          retval = true;
        }
    }
  else if (type == load_path::M_FILE)
    {
      if ((type & possible_types) == load_path::M_FILE)
        {
          fname += ".m";
          retval = true;
        }
    }
  else if (type == load_path::MEX_FILE)
    {
      if ((type & possible_types) == load_path::MEX_FILE)
        {
          fname += ".mex";
          retval = true;
        }
    }
  else if (type == (load_path::M_FILE | load_path::OCT_FILE))
    {
      if (possible_types & load_path::OCT_FILE)
        {
          fname += ".oct";
          retval = true;
        }
      else if (possible_types & load_path::M_FILE)
        {
          fname += ".m";
          retval = true;
        }
    }
  else if (type == (load_path::M_FILE | load_path::MEX_FILE))
    {
      if (possible_types & load_path::MEX_FILE)
        {
          fname += ".mex";
          retval = true;
        }
      else if (possible_types & load_path::M_FILE)
        {
          fname += ".m";
          retval = true;
        }
    }
  else if (type == (load_path::OCT_FILE | load_path::MEX_FILE))
    {
      if (possible_types & load_path::OCT_FILE)
        {
          fname += ".oct";
          retval = true;
        }
      else if (possible_types & load_path::MEX_FILE)
        {
          fname += ".mex";
          retval = true;
        }
    }
  else if (type == (load_path::M_FILE | load_path::OCT_FILE
                    | load_path::MEX_FILE))
    {
      if (possible_types & load_path::OCT_FILE)
        {
          fname += ".oct";
          retval = true;
        }
      else if (possible_types & load_path::MEX_FILE)
        {
          fname += ".mex";
          retval = true;
        }
      else if (possible_types & load_path::M_FILE)
        {
          fname += ".m";
          retval = true;
        }
    }
  else
    error ("%s: %s: invalid type code = %d", who, fcn.c_str (), type);

  return retval;
}

std::string
load_path::package_info::find_fcn (const std::string& fcn, std::string& dir_name,
                             int type) const
{
  std::string retval;

  //  update ();

  if (fcn.length () > 0 && fcn[0] == '@')
    {
      size_t pos = fcn.find ('/');

      if (pos != std::string::npos)
        {
          std::string class_name = fcn.substr (1, pos-1);
          std::string meth = fcn.substr (pos+1);

          retval = find_method (class_name, meth, dir_name);
        }
      else
        retval = "";
    }
  else
    {
      dir_name = "";

      const_fcn_map_iterator p = fcn_map.find (fcn);

      if (p != fcn_map.end ())
        {
          const file_info_list_type& file_info_list = p->second;

          for (const auto& fi : file_info_list)
            {
              retval = octave::sys::file_ops::concat (fi.dir_name, fcn);

              if (check_file_type (retval, type, fi.types,
                                   fcn, "load_path::do_find_fcn"))
                {
                  dir_name = fi.dir_name;
                  break;
                }
              else
                retval = "";
            }
        }
    }

  return retval;
}

std::string
load_path::package_info::find_private_fcn (const std::string& dir,
                                     const std::string& fcn, int type) const
{
  std::string retval;

  //  update ();

  const_private_fcn_map_iterator q = private_fcn_map.find (dir);

  if (q != private_fcn_map.end ())
    {
      const dir_info::fcn_file_map_type& m = q->second;

      dir_info::const_fcn_file_map_iterator p = m.find (fcn);

      if (p != m.end ())
        {
          std::string fname
            = octave::sys::file_ops::concat (octave::sys::file_ops::concat (dir, "private"), fcn);

          if (check_file_type (fname, type, p->second, fcn,
                               "load_path::find_private_fcn"))
            retval = fname;
        }
    }

  return retval;
}

std::string
load_path::package_info::find_method (const std::string& class_name,
                                const std::string& meth,
                                std::string& dir_name, int type) const
{
  std::string retval;

  //  update ();

  dir_name = "";

  const_method_map_iterator q = method_map.find (class_name);

  if (q != method_map.end ())
    {
      const fcn_map_type& m = q->second;

      const_fcn_map_iterator p = m.find (meth);

      if (p != m.end ())
        {
          const file_info_list_type& file_info_list = p->second;

          for (const auto& fi : file_info_list)
            {
              retval = octave::sys::file_ops::concat (fi.dir_name, meth);

              bool found = check_file_type (retval, type, fi.types,
                                            meth, "load_path::do_find_method");

              if (found)
                {
                  dir_name = fi.dir_name;
                  break;
                }
              else
                retval = "";
            }
        }
    }

  return retval;
}

std::list<std::string>
load_path::package_info::methods (const std::string& class_name) const
{
  std::list<std::string> retval;

  //  update ();

  const_method_map_iterator mtd_map_it = method_map.find (class_name);

  if (mtd_map_it != method_map.end ())
    {
      for (const auto& nm_filst : mtd_map_it->second)
        retval.push_back (nm_filst.first);
    }

  if (! retval.empty ())
    retval.sort ();

  return retval;
}

bool
load_path::is_package (const std::string& name) const
{
  for (const auto& di : dir_info_list)
    {
      if (di.is_package (name))
        return true;
    }

  return false;
}

std::list<std::string>
load_path::do_overloads (const std::string& meth) const
{
  std::list<std::string> retval;

  //  update ();

  top_level_package.overloads (meth, retval);

  for (const auto& nm_ldr : package_map)
    nm_ldr.second.overloads (meth, retval);

  return retval;
}

void
load_path::package_info::overloads (const std::string& meth,
                              std::list<std::string>& l) const
{
  for (const auto& cls_fnmap : method_map)
    {
      const fcn_map_type& m = cls_fnmap.second;

      if (m.find (meth) != m.end ())
        {
          std::string class_name = cls_fnmap.first;

          if (! m_package_name.empty ())
            class_name = m_package_name + "." + class_name;

          l.push_back (class_name);
        }
    }
}

// Should we cache all files in private directories, or is it OK to just
// look them up each time as needed?

std::string
find_private_file (const std::string& fname)
{
  std::string retval;

  // Look in private directory corresponding to current function (if
  // any).

  octave_user_function *curr_fcn = symbol_table::get_curr_fcn ();

  if (curr_fcn)
    {
      // Even for private functions, dir_name doesn't contain the
      // "private" directory component so we append it here in all
      // cases.

      std::string dir_name = curr_fcn->dir_name ();

      if (! dir_name.empty ())
        {
          std::string pfname = dir_name + octave::sys::file_ops::dir_sep_str ()
                               + "private" + octave::sys::file_ops::dir_sep_str () + fname;

          octave::sys::file_stat fs (pfname);

          if (fs.exists () && fs.is_reg ())
            retval = pfname;
        }
    }

  return retval;
}

std::string
load_path::do_find_file (const std::string& file) const
{
  std::string retval;

  if (octave::sys::env::absolute_pathname (file)
      || octave::sys::env::rooted_relative_pathname (file))
    {
      octave::sys::file_stat fs (file);

      return fs.exists () ? file : retval;
    }
  else
    {
      std::string tfile = find_private_file (file);

      if (! tfile.empty ())
        return tfile;
    }

  if (file.find_first_of (octave::sys::file_ops::dir_sep_chars ())
      != std::string::npos)
    {
      // Given name has a directory separator, so append it to each
      // element of the load path in turn.
      for (const auto& di : dir_info_list)
        {
          std::string tfile = octave::sys::file_ops::concat (di.dir_name, file);

          octave::sys::file_stat fs (tfile);

          if (fs.exists ())
            return tfile;
        }
    }
  else
    {
      // Look in cache.
      for (const auto & di : dir_info_list)
        {
          string_vector all_files = di.all_files;

          octave_idx_type len = all_files.numel ();

          for (octave_idx_type i = 0; i < len; i++)
            {
              if (all_files[i] == file)
                return octave::sys::file_ops::concat (di.dir_name, file);
            }
        }
    }

  return retval;
}

std::string
load_path::do_find_dir (const std::string& dir) const
{
  std::string retval;

  if (dir.find_first_of (octave::sys::file_ops::dir_sep_chars ()) != std::string::npos
      && (octave::sys::env::absolute_pathname (dir)
          || octave::sys::env::rooted_relative_pathname (dir)))
    {
      octave::sys::file_stat fs (dir);

      if (fs.exists () && fs.is_dir ())
        return dir;
    }
  else
    {
      for (const auto& di : dir_info_list)
        {
          std::string dname = octave::sys::env::make_absolute (di.dir_name);

          size_t dname_len = dname.length ();

          if (dname.substr (dname_len - 1)
              == octave::sys::file_ops::dir_sep_str ())
            {
              dname = dname.substr (0, dname_len - 1);
              dname_len--;
            }

          size_t dir_len = dir.length ();

          if (dname_len > dir_len
              && octave::sys::file_ops::is_dir_sep (dname[dname_len - dir_len - 1])
              && dir == dname.substr (dname_len - dir_len))
            {
              octave::sys::file_stat fs (di.dir_name);

              if (fs.exists () && fs.is_dir ())
                return di.dir_name;
            }
        }
    }

  return retval;
}

string_vector
load_path::do_find_matching_dirs (const std::string& dir) const
{
  std::list<std::string> retlist;

  if (dir.find_first_of (octave::sys::file_ops::dir_sep_chars ()) != std::string::npos
      && (octave::sys::env::absolute_pathname (dir)
          || octave::sys::env::rooted_relative_pathname (dir)))
    {
      octave::sys::file_stat fs (dir);

      if (fs.exists () && fs.is_dir ())
        retlist.push_back (dir);
    }
  else
    {
      for (const auto& di : dir_info_list)
        {
          std::string dname = octave::sys::env::make_absolute (di.dir_name);

          size_t dname_len = dname.length ();

          if (dname.substr (dname_len - 1)
              == octave::sys::file_ops::dir_sep_str ())
            {
              dname = dname.substr (0, dname_len - 1);
              dname_len--;
            }

          size_t dir_len = dir.length ();

          if (dname_len > dir_len
              && octave::sys::file_ops::is_dir_sep (dname[dname_len - dir_len - 1])
              && dir == dname.substr (dname_len - dir_len))
            {
              octave::sys::file_stat fs (di.dir_name);

              if (fs.exists () && fs.is_dir ())
                retlist.push_back (di.dir_name);
            }
        }
    }

  return retlist;
}

std::string
load_path::do_find_first_of (const string_vector& flist) const
{
  std::string retval;

  std::string dir_name;
  std::string file_name;

  octave_idx_type flen = flist.numel ();
  octave_idx_type rel_flen = 0;

  string_vector rel_flist (flen);

  for (octave_idx_type i = 0; i < flen; i++)
    {
      std::string file = flist[i];

      if (file.find_first_of (octave::sys::file_ops::dir_sep_chars ())
          != std::string::npos)
        {
          if (octave::sys::env::absolute_pathname (file)
              || octave::sys::env::rooted_relative_pathname (file))
            {
              octave::sys::file_stat fs (file);

              if (fs.exists ())
                return file;
            }
          else
            {
              for (const auto& di : dir_info_list)
                {
                  std::string tfile;
                  tfile = octave::sys::file_ops::concat (di.dir_name, file);

                  octave::sys::file_stat fs (tfile);

                  if (fs.exists ())
                    return tfile;
                }
            }
        }
      else
        rel_flist[rel_flen++] = file;
    }

  rel_flist.resize (rel_flen);

  for (const auto& di : dir_info_list)
    {
      string_vector all_files = di.all_files;

      octave_idx_type len = all_files.numel ();

      for (octave_idx_type i = 0; i < len; i++)
        {
          for (octave_idx_type j = 0; j < rel_flen; j++)
            {
              if (all_files[i] == rel_flist[j])
                {
                  dir_name = di.dir_name;
                  file_name = rel_flist[j];

                  goto done;
                }
            }
        }
    }

done:

  if (! dir_name.empty ())
    retval = octave::sys::file_ops::concat (dir_name, file_name);

  return retval;
}

string_vector
load_path::do_find_all_first_of (const string_vector& flist) const
{
  std::list<std::string> retlist;

  std::string dir_name;
  std::string file_name;

  octave_idx_type flen = flist.numel ();
  octave_idx_type rel_flen = 0;

  string_vector rel_flist (flen);

  for (octave_idx_type i = 0; i < flen; i++)
    {
      std::string file = flist[i];

      if (file.find_first_of (octave::sys::file_ops::dir_sep_chars ())
          != std::string::npos)
        {
          if (octave::sys::env::absolute_pathname (file)
              || octave::sys::env::rooted_relative_pathname (file))
            {
              octave::sys::file_stat fs (file);

              if (fs.exists ())
                retlist.push_back (file);
            }
          else
            {
              for (const auto& di : dir_info_list)
                {
                  std::string tfile;
                  tfile = octave::sys::file_ops::concat (di.dir_name, file);

                  octave::sys::file_stat fs (tfile);

                  if (fs.exists ())
                    retlist.push_back (tfile);
                }
            }
        }
      else
        rel_flist[rel_flen++] = file;
    }

  rel_flist.resize (rel_flen);

  for (const auto& di : dir_info_list)
    {
      string_vector all_files = di.all_files;

      octave_idx_type len = all_files.numel ();

      for (octave_idx_type i = 0; i < len; i++)
        {
          for (octave_idx_type j = 0; j < rel_flen; j++)
            {
              if (all_files[i] == rel_flist[j])
                retlist.push_back (octave::sys::file_ops::concat (di.dir_name,
                                                                  rel_flist[j]));
            }
        }
    }

  return retlist;
}

string_vector
load_path::do_dirs (void) const
{
  size_t len = dir_info_list.size ();

  string_vector retval (len);

  octave_idx_type k = 0;

  for (const auto& di : dir_info_list)
    retval[k++] = di.dir_name;

  return retval;
}

std::list<std::string>
load_path::do_dir_list (void) const
{
  std::list<std::string> retval;

  for (const auto& di : dir_info_list)
    retval.push_back (di.dir_name);

  return retval;
}

string_vector
load_path::do_files (const std::string& dir, bool omit_exts) const
{
  string_vector retval;

  const_dir_info_list_iterator p = find_dir_info (dir);

  if (p != dir_info_list.end ())
    retval = p->fcn_files;

  if (omit_exts)
    {
      octave_idx_type len = retval.numel ();

      for (octave_idx_type i = 0; i < len; i++)
        {
          std::string fname = retval[i];

          size_t pos = fname.rfind ('.');

          if (pos != std::string::npos)
            retval[i] = fname.substr (0, pos);
        }
    }

  return retval;
}

string_vector
load_path::do_fcn_names (void) const
{
  return top_level_package.fcn_names ();
}

string_vector
load_path::package_info::fcn_names (void) const
{
  size_t len = fcn_map.size ();

  string_vector retval (len);

  octave_idx_type count = 0;

  for (const auto& nm_filst : fcn_map)
    retval[count++] = nm_filst.first;

  return retval;
}

std::string
load_path::do_path (void) const
{
  std::string xpath;

  string_vector xdirs = load_path::dirs ();

  octave_idx_type len = xdirs.numel ();

  if (len > 0)
    xpath = xdirs[0];

  for (octave_idx_type i = 1; i < len; i++)
    xpath += octave::directory_path::path_sep_str () + xdirs[i];

  return xpath;
}

void
print_types (std::ostream& os, int types)
{
  bool printed_type = false;

  if (types & load_path::OCT_FILE)
    {
      os << "oct";
      printed_type = true;
    }

  if (types & load_path::MEX_FILE)
    {
      if (printed_type)
        os << "|";
      os << "mex";
      printed_type = true;
    }

  if (types & load_path::M_FILE)
    {
      if (printed_type)
        os << "|";
      os << "m";
      printed_type = true;
    }
}

void
print_fcn_list (std::ostream& os,
                const load_path::dir_info::fcn_file_map_type& lst)
{
  for (const auto& nm_typ : lst)
    {
      os << "  " << nm_typ.first << " (";

      print_types (os, nm_typ.second);

      os << ")\n";
    }
}

string_vector
get_file_list (const load_path::dir_info::fcn_file_map_type& lst)
{
  octave_idx_type n = lst.size ();

  string_vector retval (n);

  octave_idx_type count = 0;

  for (const auto& nm_typ : lst)
    {
      std::string nm = nm_typ.first;

      int types = nm_typ.second;

      if (types & load_path::OCT_FILE)
        nm += ".oct";
      else if (types & load_path::MEX_FILE)
        nm += ".mex";
      else
        nm += ".m";

      retval[count++] = nm;
    }

  return retval;
}

void
load_path::do_display (std::ostream& os) const
{
  for (const auto& di : dir_info_list)
    {
      string_vector fcn_files = di.fcn_files;

      if (! fcn_files.empty ())
        {
          os << "\n*** function files in " << di.dir_name << ":\n\n";

          fcn_files.list_in_columns (os);
        }

      const dir_info::method_file_map_type& method_file_map
        = di.method_file_map;

      if (! method_file_map.empty ())
        {
          for (const auto& cls_ci : method_file_map)
            {
              os << "\n*** methods in " << di.dir_name
                 << "/@" << cls_ci.first << ":\n\n";

              const dir_info::class_info& ci = cls_ci.second;

              string_vector method_files = get_file_list (ci.method_file_map);

              method_files.list_in_columns (os);
            }
        }
    }

  top_level_package.display (os);

  for (const auto& nm_ldr : package_map)
    nm_ldr.second.display (os);
}

// True if a path is contained in a path list separated by path_sep_char
static bool
in_path_list (const std::string& path_list, const std::string& path)
{
  size_t ps = path.size ();
  size_t pls = path_list.size ();
  size_t pos = path_list.find (path);
  char psc = octave::directory_path::path_sep_char ();
  while (pos != std::string::npos)
    {
      if ((pos == 0 || path_list[pos-1] == psc)
          && (pos + ps == pls || path_list[pos + ps] == psc))
        return true;
      else
        pos = path_list.find (path, pos + 1);
    }

  return false;
}

void
load_path::add (const dir_info& di, bool at_end,
                const std::string& pname, bool updating) const
{
  package_info& pkg = get_package (pname);

  pkg.add (di, at_end, updating);

  dir_info::package_dir_map_type package_dir_map = di.package_dir_map;

  for (const auto& pkg_di : package_dir_map)
    {
      std::string full_name = pkg_di.first;

      if (! pname.empty ())
        full_name = pname + "." + full_name;

      add (pkg_di.second, at_end, full_name);
    }
}

void
load_path::package_info::add_to_fcn_map (const dir_info& di, bool at_end,
                                   bool updating)
{
  std::string dir_name = di.dir_name;

  string_vector fcn_files = di.fcn_files;

  octave_idx_type len = fcn_files.numel ();

  for (octave_idx_type i = 0; i < len; i++)
    {
      std::string fname = fcn_files[i];

      std::string ext;
      std::string base = fname;

      size_t pos = fname.rfind ('.');

      if (pos != std::string::npos)
        {
          base = fname.substr (0, pos);
          ext = fname.substr (pos);
        }

      file_info_list_type& file_info_list = fcn_map[base];

      file_info_list_iterator p = file_info_list.begin ();

      while (p != file_info_list.end ())
        {
          if (p->dir_name == dir_name)
            break;

          p++;
        }

      int t = 0;
      if (ext == ".m")
        t = load_path::M_FILE;
      else if (ext == ".oct")
        t = load_path::OCT_FILE;
      else if (ext == ".mex")
        t = load_path::MEX_FILE;

      if (p == file_info_list.end ())
        {
          // Warn if a built-in or library function is being shadowed,
          // but not if we are just updating (rehashing) the list.

          if (! updating)
            {
              if (file_info_list.empty ())
                {
                  if (symbol_table::is_built_in_function_name (base))
                    {
                      std::string fcn_path = octave::sys::file_ops::concat (dir_name, fname);

                      warning_with_id ("Octave:shadowed-function",
                                       "function %s shadows a built-in function",
                                       fcn_path.c_str ());
                    }
                }
              else if (! at_end)
                {
                  file_info& old = file_info_list.front ();

                  // FIXME: do we need to be more careful about the
                  // way we look for old.dir_name in sys_path to avoid
                  // partial matches?

                  // Don't warn about Contents.m files since we expect
                  // more than one to exist in the load path.

                  if (fname != "Contents.m"
                      && sys_path.find (old.dir_name) != std::string::npos
                      && in_path_list (sys_path, old.dir_name))
                    {
                      std::string fcn_path = octave::sys::file_ops::concat (dir_name, fname);

                      warning_with_id ("Octave:shadowed-function",
                                       "function %s shadows a core library function",
                                       fcn_path.c_str ());
                    }
                }
            }

          file_info fi (dir_name, t);

          if (at_end)
            file_info_list.push_back (fi);
          else
            file_info_list.push_front (fi);
        }
      else
        {
          file_info& fi = *p;

          fi.types |= t;
        }
    }
}

void
load_path::package_info::add_to_private_fcn_map (const dir_info& di)
{
  dir_info::fcn_file_map_type private_file_map = di.private_file_map;

  if (! private_file_map.empty ())
    private_fcn_map[di.dir_name] = private_file_map;
}

void
load_path::package_info::add_to_method_map (const dir_info& di, bool at_end)
{
  std::string dir_name = di.dir_name;

  // <CLASS_NAME, CLASS_INFO>
  dir_info::method_file_map_type method_file_map = di.method_file_map;

  for (const auto& cls_ci : method_file_map)
    {
      std::string class_name = cls_ci.first;

      fcn_map_type& fm = method_map[class_name];

      std::string full_dir_name
        = octave::sys::file_ops::concat (dir_name, "@" + class_name);

      const dir_info::class_info& ci = cls_ci.second;

      // <FCN_NAME, TYPES>
      const dir_info::fcn_file_map_type& m = ci.method_file_map;

      for (const auto& nm_typ : m)
        {
          std::string base = nm_typ.first;
          int types = nm_typ.second;

          file_info_list_type& file_info_list = fm[base];

          file_info_list_iterator p2 = file_info_list.begin ();
          while (p2 != file_info_list.end ())
            {
              if (p2->dir_name == full_dir_name)
                break;

              p2++;
            }

          if (p2 == file_info_list.end ())
            {
              file_info fi (full_dir_name, types);

              if (at_end)
                file_info_list.push_back (fi);
              else
                file_info_list.push_front (fi);
            }
          else
            {
              // FIXME: is this possible?
              file_info& fi = *p2;

              fi.types = types;
            }
        }

      // <FCN_NAME, TYPES>
      dir_info::fcn_file_map_type private_file_map = ci.private_file_map;

      if (! private_file_map.empty ())
        private_fcn_map[full_dir_name] = private_file_map;
    }
}

void
load_path::package_info::display (std::ostream& os) const
{
  os << "*** package info: "
     << (m_package_name.empty () ? "<top-level>" : m_package_name) << "\n\n";

  for (const auto& dir : dir_list)
    os << dir << "\n";
  os << "\n";

  for (const auto& dir_fnlst : private_fcn_map)
    {
      os << "\n*** private functions in "
         << octave::sys::file_ops::concat (dir_fnlst.first, "private") << ":\n\n";

      print_fcn_list (os, dir_fnlst.second);
    }

#if defined (DEBUG_LOAD_PATH)

  for (const auto& nm_filst : fcn_map)
    {
      os << nm_filst.first << ":\n";

      const file_info_list_type& file_info_list = nm_filst.second;

      for (const auto& finfo : file_info_list)
        {
          os << "  " << finfo.dir_name << " (";

          print_types (os, finfo.types);

          os << ")\n";
        }
    }

  for (const auto& cls_fnmap : method_map)
    {
      os << "CLASS " << cls_fnmap.first << ":\n";

      const fcn_map_type& fm = cls_fnmap.second;

      for (const auto& nm_fnlst : fcn_map)
        {
          os << "  " << nm_fnlst.first << ":\n";

          const file_info_list_type& file_info_list = nm_fnlst.second;

          for (const auto& finfo : file_info_list)
            {
              os << "  " << finfo.dir_name << " (";

              print_types (os, finfo.types);

              os << ")\n";
            }
        }
    }

  os << "\n";

#endif
}

std::string
genpath (const std::string& dirname, const string_vector& skip)
{
  std::string retval;

  octave::sys::dir_entry dir (dirname);

  if (dir)
    {
      retval = dirname;

      string_vector dirlist = dir.read ().sort (false);

      octave_idx_type len = dirlist.numel ();

      for (octave_idx_type i = 0; i < len; i++)
        {
          std::string elt = dirlist[i];

          bool skip_p = (elt == "." || elt == ".." || elt[0] == '@'
                         || elt[0] == '+');

          if (! skip_p)
            {
              for (octave_idx_type j = 0; j < skip.numel (); j++)
                {
                  skip_p = (elt == skip[j]);
                  if (skip_p)
                    break;
                }

              if (! skip_p)
                {
                  std::string nm = octave::sys::file_ops::concat (dirname, elt);

                  octave::sys::file_stat fs (nm);

                  if (fs && fs.is_dir ())
                    retval += octave::directory_path::path_sep_str () + genpath (nm, skip);
                }
            }
        }
    }

  return retval;
}

std::list<std::string>
load_path::do_get_all_package_names (bool only_top_level) const
{
  std::list<std::string> retval;

  for (const auto& dir_ldr : package_map)
    {
      if (! only_top_level || dir_ldr.first.find ('.') == std::string::npos)
        retval.push_back (dir_ldr.first);
    }

  return retval;
}

static void
execute_pkg_add_or_del (const std::string& dir,
                        const std::string& script_file)
{
  if (! octave_interpreter_ready)
    return;

  octave::unwind_protect frame;

  std::string file = octave::sys::file_ops::concat (dir, script_file);

  octave::sys::file_stat fs (file);

  if (fs.exists ())
    octave::source_file (file, "base");
}

void
load_path::execute_pkg_add (const std::string& dir)
{
  execute_pkg_add_or_del (dir, "PKG_ADD");
}

void
load_path::execute_pkg_del (const std::string& dir)
{
  execute_pkg_add_or_del (dir, "PKG_DEL");
}

DEFUN (genpath, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} genpath (@var{dir})
@deftypefnx {} {} genpath (@var{dir}, @var{skip}, @dots{})
Return a path constructed from @var{dir} and all its subdirectories.

If additional string parameters are given, the resulting path will exclude
directories with those names.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();

  octave_value retval;

  if (nargin == 1)
    {
      std::string dirname = args(0).xstring_value ("genpath: DIR must be a string");

      retval = genpath (dirname);
    }
  else
    {
      std::string dirname = args(0).xstring_value ("genpath: all arguments must be strings");

      string_vector skip (nargin - 1);

      for (octave_idx_type i = 1; i < nargin; i++)
        skip[i-1] = args(i).xstring_value ("genpath: all arguments must be strings");

      retval = genpath (dirname, skip);
    }

  return retval;
}

static void
rehash_internal (void)
{
  load_path::update ();

  // FIXME: maybe we should rename this variable since it is being
  // used for more than keeping track of the prompt time.

  // This will force updated functions to be found.
  Vlast_prompt_time.stamp ();
}

DEFUN (rehash, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} rehash ()
Reinitialize Octave's load path directory cache.
@end deftypefn */)
{
  rehash_internal ();

  return ovl ();
}

DEFUN (command_line_path, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} command_line_path (@dots{})
Return the command line path variable.

@seealso{path, addpath, rmpath, genpath, pathdef, savepath, pathsep}
@end deftypefn */)
{
  return ovl (load_path::get_command_line_path ());
}

DEFUN (restoredefaultpath, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} restoredefaultpath (@dots{})
Restore Octave's path to its initial state at startup.

@seealso{path, addpath, rmpath, genpath, pathdef, savepath, pathsep}
@end deftypefn */)
{
  load_path::initialize (true);

  return ovl (load_path::system_path ());
}

// Return Octave's original default list of directories in which to
// search for function files.  This corresponds to the path that
// exists prior to running the system's octaverc file or the user's
// ~/.octaverc file

DEFUN (__pathorig__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{val} =} __pathorig__ ()
Undocumented internal function.
@end deftypefn */)
{
  return ovl (load_path::system_path ());
}

DEFUN (path, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} path ()
@deftypefnx {} {@var{str} =} path ()
@deftypefnx {} {@var{str} =} path (@var{path1}, @dots{})
Modify or display Octave's load path.

If @var{nargin} and @var{nargout} are zero, display the elements of
Octave's load path in an easy to read format.

If @var{nargin} is zero and nargout is greater than zero, return the
current load path.

If @var{nargin} is greater than zero, concatenate the arguments,
separating them with @code{pathsep}.  Set the internal search path
to the result and return it.

No checks are made for duplicate elements.
@seealso{addpath, rmpath, genpath, pathdef, savepath, pathsep}
@end deftypefn */)
{
  int nargin = args.length ();

  string_vector argv = args.make_argv ("path");

  if (nargin > 0)
    {
      std::string path = argv[1];

      for (int i = 2; i <= nargin; i++)
        path += octave::directory_path::path_sep_str () + argv[i];

      load_path::set (path, true);

      rehash_internal ();
    }

  if (nargout > 0)
    return ovl (load_path::path ());
  else if (nargin == 0 && nargout == 0)
    {
      octave_stdout <<
        "\nOctave's search path contains the following directories:\n\n";

      string_vector dirs = load_path::dirs ();

      dirs.list_in_columns (octave_stdout);

      octave_stdout << "\n";
    }

  return ovl ();
}

DEFUN (addpath, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} addpath (@var{dir1}, @dots{})
@deftypefnx {} {} addpath (@var{dir1}, @dots{}, @var{option})
Add named directories to the function search path.

If @var{option} is @qcode{"-begin"} or 0 (the default), prepend the
directory name to the current path.  If @var{option} is @qcode{"-end"}
or 1, append the directory name to the current path.
Directories added to the path must exist.

In addition to accepting individual directory arguments, lists of
directory names separated by @code{pathsep} are also accepted.  For example:

@example
addpath ("dir1:/dir2:~/dir3")
@end example

For each directory that is added, and that was not already in the path,
@code{addpath} checks for the existence of a file named @file{PKG_ADD}
(note lack of .m extension) and runs it if it exists.

@seealso{path, rmpath, genpath, pathdef, savepath, pathsep}
@end deftypefn */)
{
  // Originally written by Bill Denney and Etienne Grossman.
  // Heavily modified and translated to C++ by jwe.

  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();

  octave_value retval;

  if (nargout > 0)
    retval = load_path::path ();

  bool append = false;

  octave_value option_arg = args(nargin-1);

  if (option_arg.is_string ())
    {
      std::string option = option_arg.string_value ();

      if (option == "-end")
        {
          append = true;
          nargin--;
        }
      else if (option == "-begin")
        nargin--;
    }
  else if (option_arg.is_numeric_type ())
    {
      int val = option_arg.xint_value ("addpath: OPTION must be '-begin'/0 or '-end'/1");

      if (val == 0)
        nargin--;
      else if (val == 1)
        {
          append = true;
          nargin--;
        }
      else
        error ("addpath: OPTION must be '-begin'/0 or '-end'/1");
    }

  bool need_to_update = false;

  octave_value_list arglist (args.slice (0, nargin));
  if (! append)
    arglist.reverse ();

  for (int i = 0; i < arglist.length (); i++)
    {
      std::string arg = arglist(i).xstring_value ("addpath: all arguments must be strings");

      std::list<std::string> dir_elts = split_path (arg);

      if (! append)
        std::reverse (dir_elts.begin (), dir_elts.end ());

      for (auto dir : dir_elts)
        {
          // Remove duplicate directory separators
          dir.erase (std::unique (dir.begin (), dir.end (),
                                  [](char l, char r)
                                  {
                                    return l == r &&
                                           octave::sys::file_ops::is_dir_sep (l);
                                  }),
                     dir.end ());

          if (append)
            load_path::append (dir, true);
          else
            load_path::prepend (dir, true);

          need_to_update = true;
        }
    }

  if (need_to_update)
    rehash_internal ();

  return retval;
}

DEFUN (rmpath, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn {} {} rmpath (@var{dir1}, @dots{})
Remove @var{dir1}, @dots{} from the current function search path.

In addition to accepting individual directory arguments, lists of
directory names separated by @code{pathsep} are also accepted.  For example:

@example
rmpath ("dir1:/dir2:~/dir3")
@end example

For each directory that is removed, @code{rmpath} checks for the
existence of a file named @file{PKG_DEL} (note lack of .m extension)
and runs it if it exists.

@seealso{path, addpath, genpath, pathdef, savepath, pathsep}
@end deftypefn */)
{
  // Originally written by Etienne Grossmann.  Heavily modified and translated
  // to C++ by jwe.

  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();

  octave_value retval;

  if (nargout > 0)
    retval = load_path::path ();

  bool need_to_update = false;

  for (int i = 0; i < nargin; i++)
    {
      std::string arg = args(i).xstring_value ("rmpath: all arguments must be strings");
      std::list<std::string> dir_elts = split_path (arg);

      for (const auto& dir : dir_elts)
        {
          //dir = regexprep (dir_elts{j}, '//+', "/");
          //dir = regexprep (dir, '/$', "");

          if (! load_path::remove (dir))
            warning ("rmpath: %s: not found", dir.c_str ());
          else
            need_to_update = true;
        }
    }

  if (need_to_update)
    rehash_internal ();

  return retval;
}

DEFUN (__dump_load_path__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __dump_load_path__ ()
Undocumented internal function.
@end deftypefn */)
{
  load_path::display (octave_stdout);

  return ovl ();
}
