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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "pathsearch.h"

#include "defaults.h"
#include "defun.h"
#include "input.h"
#include "load-path.h"
#include "pager.h"
#include "parse.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"

load_path *load_path::instance = 0;
load_path::hook_function_ptr load_path::add_hook = execute_pkg_add;
load_path::hook_function_ptr load_path::remove_hook = execute_pkg_del;
std::string load_path::command_line_path;

static std::string Vsystem_path;

void
load_path::dir_info::update (void)
{
  if (is_relative)
    initialize ();
  else
    {
      file_stat fs (dir_name);

      if (fs)
	{
	  if (fs.mtime () != dir_mtime)
	    initialize ();
	}
      else
	{
	  std::string msg = fs.error ();
	  warning ("load_path: %s: %s", dir_name.c_str (), msg.c_str ());
	}
    }
}

void
load_path::dir_info::initialize (void)
{
  is_relative = ! octave_env::absolute_pathname (dir_name);

  file_stat fs (dir_name);

  if (fs)
    {
      dir_mtime = fs.mtime ();

      bool has_private_subdir = get_file_list (dir_name);

      if (! error_state)
	{
	  if (has_private_subdir)
	    {
	      std::string pdn = dir_name + file_ops::dir_sep_str + "private";

	      get_private_function_map (pdn);
	    }
	}
    }
  else
    {
      std::string msg = fs.error ();
      warning ("load_path: %s: %s", dir_name.c_str (), msg.c_str ());
    }
}

bool
load_path::dir_info::get_file_list (const std::string& d)
{
  bool has_private_subdir = false;

  dir_entry dir (d);

  if (dir)
    {
      string_vector flist = dir.read ();

      octave_idx_type len = flist.length ();

      all_files.resize (len);
      fcn_files.resize (len);

      octave_idx_type all_files_count = 0;
      octave_idx_type fcn_files_count = 0;

      for (octave_idx_type i = 0; i < len; i++)
	{
	  std::string fname = flist[i];

	  std::string full_name = d + file_ops::dir_sep_str + fname;

	  file_stat fs (full_name);

	  if (fs)
	    {
	      if (fs.is_dir ())
		{
		  if (! has_private_subdir && fname == "private")
		    has_private_subdir = true;
		}
	      else
		{
		  all_files[all_files_count++] = fname;

		  size_t pos = fname.rfind ('.');

		  if (pos != NPOS)
		    {
		      std::string ext = fname.substr (pos);

		      if (ext == ".m" || ext == ".oct")
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

  return has_private_subdir;
}

void
load_path::dir_info::get_private_function_map (const std::string& d)
{
  dir_entry dir (d);

  if (dir)
    {
      string_vector flist = dir.read ();

      octave_idx_type len = flist.length ();

      for (octave_idx_type i = 0; i < len; i++)
	{
	  std::string fname = flist[i];

	  std::string ext;
	  std::string base = fname;

	  size_t pos = fname.rfind ('.');

	  if (pos != NPOS)
	    {
	      base = fname.substr (0, pos);
	      ext = fname.substr (pos);

	      if (valid_identifier (base))
		{
		  int t = 0;

		  if (ext == ".m")
		    t = load_path::M_FILE;
		  else if (ext == ".oct")
		    t = load_path::OCT_FILE;

		  private_function_map[base] |= t;
		}
	    }
	}
    }
  else
    {
      std::string msg = dir.error ();
      warning ("load_path: %s: %s", d.c_str (), msg.c_str ());
    }
}

bool
load_path::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    instance = new load_path ();

  if (! instance)
    {
      ::error ("unable to create load path object!");

      retval = false;
    }

  return retval;
}

load_path::const_dir_info_list_iterator
load_path::find_dir_info (const std::string& dir) const
{
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
load_path::find_dir_info (const std::string& dir)
{
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

void
load_path::move (dir_info_list_iterator i, bool at_end)
{
  if (dir_info_list.size () > 1)
    {
      dir_info di = *i;

      dir_info_list.erase (i);

      if (at_end)
	dir_info_list.push_back (di);
      else
	dir_info_list.push_front (di);

      std::string dir = di.dir_name;

      string_vector fcn_files = di.fcn_files;

      octave_idx_type len = fcn_files.length ();

      for (octave_idx_type k = 0; k < len; k++)
	{
	  std::string fname = fcn_files[k];

	  std::string ext;
	  std::string base = fname;

	  size_t pos = fname.rfind ('.');

	  if (pos != NPOS)
	    {
	      base = fname.substr (0, pos);
	      ext = fname.substr (pos);
	    }

	  std::list<file_info>& file_info_list = fcn_map[base];

	  if (file_info_list.size () == 1)
	    continue;
	  else
	    {
	      for (std::list<file_info>::iterator p = file_info_list.begin ();
		   p != file_info_list.end ();
		   p++)
		{
		  if (p->dir_name == dir)
		    {
		      file_info& fi = *p;

		      file_info_list.erase (p);

		      if (at_end)
			file_info_list.push_back (fi);
		      else
			file_info_list.push_front (fi);

		      break;
		    }
		}
	    }
	}
    }
}

static void
maybe_add_path_elts (std::string& path, const std::string& dir)
{
  std::string tpath = genpath (dir);

  if (! tpath.empty ())
    path += dir_path::path_sep_str + tpath;
}

void
load_path::do_initialize (void)
{
  Vsystem_path = ":";

  maybe_add_path_elts (Vsystem_path, Vlocal_ver_oct_file_dir);
  maybe_add_path_elts (Vsystem_path, Vlocal_api_oct_file_dir);
  maybe_add_path_elts (Vsystem_path, Vlocal_oct_file_dir);
  maybe_add_path_elts (Vsystem_path, Vlocal_ver_fcn_file_dir);
  maybe_add_path_elts (Vsystem_path, Vlocal_api_fcn_file_dir);
  maybe_add_path_elts (Vsystem_path, Vlocal_fcn_file_dir);
  maybe_add_path_elts (Vsystem_path, Voct_file_dir);
  maybe_add_path_elts (Vsystem_path, Vfcn_file_dir);

  std::string tpath = load_path::command_line_path;

  if (tpath.empty ())
    tpath = octave_env::getenv ("OCTAVE_LOADPATH");

  std::string xpath = ".";

  if (! tpath.empty ())
    xpath += dir_path::path_sep_str + tpath;

  if (Vsystem_path != ":")
    xpath += Vsystem_path;

  do_set (xpath + ":::");
}

void
load_path::do_clear (void)
{
  dir_info_list.clear ();
  fcn_map.clear ();
}

static std::list<std::string>
split_path (const std::string& p)
{
  std::list<std::string> retval;

  size_t beg = 0;
  size_t end = p.find (dir_path::path_sep_char);

  size_t len = p.length ();

  while (end != NPOS)
    {
      std::string elt = p.substr (beg, end-beg);

      if (! elt.empty ())
	retval.push_back (elt);

      beg = end + 1;

      if (beg == len)
	break;

      end = p.find (dir_path::path_sep_char, beg);
    }

  std::string elt = p.substr (beg);

  if (! elt.empty ())
    retval.push_back (elt);

  return retval;
}

void
load_path::do_set (const std::string& p)
{
  do_clear ();

  std::list<std::string> elts = split_path (p);

  // Temporarily disable add hook.

  unwind_protect_ptr (add_hook);

  add_hook = 0;

  for (std::list<std::string>::const_iterator i = elts.begin ();
       i != elts.end ();
       i++)
    do_append (*i);

  // Restore add hook and execute for all newly added directories.

  unwind_protect::run ();

  for (dir_info_list_iterator i = dir_info_list.begin ();
       i != dir_info_list.end ();
       i++)
    {
      if (add_hook)
	add_hook (i->dir_name);
    }
}

void
load_path::do_append (const std::string& dir)
{
  if (! dir.empty ())
    {
      dir_info_list_iterator i = find_dir_info (dir);

      if (i != dir_info_list.end ())
	move (i, true);
      else
	{
	  dir_info di (dir);

	  if (! error_state)
	    {
	      dir_info_list.push_back (di);

	      add_to_fcn_map (di, true);

	      if (add_hook)
		add_hook (dir);
	    }
	}
    }
}

void
load_path::do_prepend (const std::string& dir)
{
  if (! dir.empty ())
    {
      dir_info_list_iterator i = find_dir_info (dir);

      if (i != dir_info_list.end ())
	move (i, false);
      else
	{
	  dir_info di (dir);

	  if (! error_state)
	    {
	      dir_info_list.push_front (di);

	      add_to_fcn_map (di, false);

	      if (add_hook)
		add_hook (dir);
	    }
	}

      // FIXME -- is there a better way to do this?

      i = find_dir_info (".");

      if (i != dir_info_list.end ())
	move (i, false);
      else
	panic_impossible ();
    }
}

bool
load_path::do_remove (const std::string& dir)
{
  bool retval = false;

  if (! dir.empty ())
    {
      if (dir == ".")
	warning ("rmpath: can't remove \".\" from path");

      dir_info_list_iterator i = find_dir_info (dir);

      if (i != dir_info_list.end ())
	{
	  retval = true;

	  string_vector fcn_files = i->fcn_files;

	  dir_info_list.erase (i);

	  octave_idx_type len = fcn_files.length ();

	  for (octave_idx_type k = 0; k < len; k++)
	    {
	      std::string fname = fcn_files[k];

	      std::string ext;
	      std::string base = fname;

	      size_t pos = fname.rfind ('.');

	      if (pos != NPOS)
		{
		  base = fname.substr (0, pos);
		  ext = fname.substr (pos);
		}

	      std::list<file_info>& file_info_list = fcn_map[base];

	      for (std::list<file_info>::iterator p = file_info_list.begin ();
		   p != file_info_list.end ();
		   p++)
		{
		  if (p->dir_name == dir)
		    {
		      file_info_list.erase (p);

		      if (file_info_list.empty ())
			fcn_map.erase (fname);

		      break;
		    }
		}
	    }

	  if (remove_hook)
	    remove_hook (dir);
	}
    }

  return retval;
}

void
load_path::do_update (void) const
{
  // I don't see a better way to do this because we need to
  // preserve the correct directory ordering for new files that
  // have appeared.

  fcn_map.clear ();

  for (dir_info_list_iterator p = dir_info_list.begin ();
       p != dir_info_list.end ();
       p++)
    {
      dir_info& di = *p;

      di.update ();

      add_to_fcn_map (di, true);
    }
}

std::string
load_path::do_find_fcn (const std::string& fcn, int type) const
{
  std::string retval;

  update ();

  const_fcn_map_iterator p = fcn_map.find (fcn);

  if (p != fcn_map.end ())
    {
      const std::list<file_info>& file_info_list = p->second;

      for (const_file_info_list_iterator i = file_info_list.begin ();
	   i != file_info_list.end ();
	   i++)
	{
	  const file_info& fi = *i;

	  int t = fi.types;

	  retval = fi.dir_name + file_ops::dir_sep_str + fcn;

	  if (type == load_path::OCT_FILE)
	    {
	      if ((type & t) == load_path::OCT_FILE)
		{
		  retval += ".oct";
		  break;
		}
	    }
	  else if (type == load_path::M_FILE)
	    {
	      if ((type & t) == load_path::M_FILE)
		{
		  retval += ".m";
		  break;
		}
	    }
	  else if (type == (load_path::M_FILE | load_path::OCT_FILE))
	    {
	      if (t & load_path::OCT_FILE)
		{
		  retval += ".oct";
		  break;
		}
	      else if (t & load_path::M_FILE)
		{
		  retval += ".m";
		  break;
		}
	    }
	  else
	    error ("load_path::do_find_fcn: %s: invalid type code = %d",
		   fcn.c_str (), type);
	}
    }

  return retval;
}

std::string
load_path::do_find_file (const std::string& file) const
{
  std::string retval;

  if (octave_env::absolute_pathname (file))
    {
      file_stat fs (file);

      if (fs.exists ())
	return file;
    }

  std::string dir_name;

  for (const_dir_info_list_iterator p = dir_info_list.begin ();
       p != dir_info_list.end ();
       p++)
    {
      string_vector all_files = p->all_files;

      octave_idx_type len = all_files.length ();

      for (octave_idx_type i = 0; i < len; i++)
	{
	  if (all_files[i] == file)
	    {
	      dir_name = p->dir_name;
	      break;
	    }
	}
    }

  if (! dir_name.empty ())
    retval = dir_name + file_ops::dir_sep_str + file;

  return retval;
}

std::string
load_path::do_find_first_of (const string_vector& flist) const
{
  std::string retval;

  std::string dir_name;
  std::string file_name;

  octave_idx_type flen = flist.length ();
  octave_idx_type rel_flen = 0;

  string_vector rel_flist (flen);

  for (octave_idx_type i = 0; i < flen; i++)
    {
      if (octave_env::absolute_pathname (flist[i]))
	{
	  file_stat fs (flist[i]);

	  if (fs.exists ())
	    return flist[i];
	}
      else
	rel_flist[rel_flen++] = flist[i];
    }

  rel_flist.resize (rel_flen);

  for (const_dir_info_list_iterator p = dir_info_list.begin ();
       p != dir_info_list.end ();
       p++)
    {
      string_vector all_files = p->all_files;

      octave_idx_type len = all_files.length ();

      for (octave_idx_type i = 0; i < len; i++)
	{
	  
	  for (octave_idx_type j = 0; j < rel_flen; j++)
	    {
	      if (all_files[i] == rel_flist[j])
		{
		  dir_name = p->dir_name;
		  file_name = rel_flist[j];
		  break;
		}
	    }
	}
    }

  if (! dir_name.empty ())
    retval = dir_name + file_ops::dir_sep_str + file_name;

  return retval;
}

string_vector
load_path::do_find_all_first_of (const string_vector& flist) const
{
  std::list<std::string> retlist;

  std::string dir_name;
  std::string file_name;

  octave_idx_type flen = flist.length ();
  octave_idx_type rel_flen = 0;

  string_vector rel_flist (flen);

  for (octave_idx_type i = 0; i < flen; i++)
    {
      if (octave_env::absolute_pathname (flist[i]))
	{
	  file_stat fs (flist[i]);

	  if (fs.exists ())
	    retlist.push_back (flist[i]);
	}
      else
	rel_flist[rel_flen++] = flist[i];
    }

  rel_flist.resize (rel_flen);

  for (const_dir_info_list_iterator p = dir_info_list.begin ();
       p != dir_info_list.end ();
       p++)
    {
      string_vector all_files = p->all_files;

      octave_idx_type len = all_files.length ();

      for (octave_idx_type i = 0; i < len; i++)
	{
	  for (octave_idx_type j = 0; j < rel_flen; j++)
	    {
	      if (all_files[i] == rel_flist[j])
		retlist.push_back
		  (p->dir_name + file_ops::dir_sep_str + rel_flist[j]);
	    }
	}
    }

  size_t retsize = retlist.size ();

  string_vector retval (retsize);

  for (size_t i = 0; i < retsize; i++)
    {
      retval[i] = retlist.front ();

      retlist.pop_front ();
    }

  return retval;
}

string_vector
load_path::do_dirs (void) const
{
  size_t len = dir_info_list.size ();

  string_vector retval (len);

  octave_idx_type k = 0;

  for (const_dir_info_list_iterator i = dir_info_list.begin ();
       i != dir_info_list.end ();
       i++)
    retval[k++] = i->dir_name;

  return retval;
}

std::list<std::string>
load_path::do_dir_list (void) const
{
  std::list<std::string> retval;

  for (const_dir_info_list_iterator i = dir_info_list.begin ();
       i != dir_info_list.end ();
       i++)
    retval.push_back (i->dir_name);

  return retval;
}

string_vector
load_path::do_files (const std::string& dir) const
{
  string_vector retval;

  const_dir_info_list_iterator i = find_dir_info (dir);

  if (i != dir_info_list.end ())
    retval = i->fcn_files;

  return retval;
}

string_vector
load_path::do_fcn_names (void) const
{
  size_t len = fcn_map.size ();

  string_vector retval (len);

  octave_idx_type count = 0;

  for (const_fcn_map_iterator p = fcn_map.begin ();
       p != fcn_map.end ();
       p++)
    retval[count++] = p->first;

  return retval;
}

std::string
load_path::do_path (void) const
{
  std::string xpath;

  string_vector xdirs = load_path::dirs ();

  octave_idx_type len = xdirs.length ();

  if (len > 0)
    xpath = xdirs[0];

  for (octave_idx_type i = 1; i < len; i++)
    xpath += dir_path::path_sep_str + xdirs[i];

  return xpath;
}

void
load_path::do_display (std::ostream& os) const
{
  for (const_dir_info_list_iterator i = dir_info_list.begin ();
       i != dir_info_list.end ();
       i++)
    {
      string_vector fcn_files = i->fcn_files;

      if (! fcn_files.empty ())
	{
	  os << "\n*** function files in " << i->dir_name << ":\n\n";

	  fcn_files.list_in_columns (os);
	}

#if defined (DEBUG_LOAD_PATH)

      const std::map<std::string, int>& private_function_map
	= i->private_function_map;

      if (private_function_map.size () > 0)
	{
	  os << "private:\n";

	  for (std::map<std::string, int>::const_iterator p = private_function_map.begin ();
	       p != private_function_map.end ();
	       p++)
	    {
	      os << "  " << p->first << " (";

	      bool printed_type = false;

	      int types = p->second;

	      if (types & load_path::OCT_FILE)
		{
		  os << "oct";
		  printed_type = true;
		}

	      if (types & load_path::M_FILE)
		{
		  if (printed_type)
		    os << "|";
		  os << "m";
		  printed_type = true;
		}

	      os << ")\n";
	    }

	  os << "\n";
	}
#endif
    }

#if defined (DEBUG_LOAD_PATH)

  for (const_fcn_map_iterator i = fcn_map.begin ();
       i != fcn_map.end ();
       i++)
    {
      os << i->first << ":\n";

      const std::list<file_info>& file_info_list = i->second;

      for (const_file_info_list_iterator p = file_info_list.begin ();
	   p != file_info_list.end ();
	   p++)
	{
	  os << "  " << p->dir_name << " (";

	  bool printed_type = false;

	  if (p->types & load_path::OCT_FILE)
	    {
	      os << "oct";
	      printed_type = true;
	    }

	  if (p->types & load_path::M_FILE)
	    {
	      if (printed_type)
		os << "|";
	      os << "m";
	      printed_type = true;
	    }

	  os << ")\n";
	}
    }

  os << "\n";

#endif
}

void
load_path::add_to_fcn_map (const dir_info& di, bool at_end) const
{
  std::string dir_name = di.dir_name;

  string_vector fcn_files = di.fcn_files;

  octave_idx_type len = fcn_files.length ();

  for (octave_idx_type i = 0; i < len; i++)
    {
      std::string fname = fcn_files[i];

      std::string ext;
      std::string base = fname;

      size_t pos = fname.rfind ('.');

      if (pos != NPOS)
	{
	  base = fname.substr (0, pos);
	  ext = fname.substr (pos);
	}

      std::list<file_info>& file_info_list = fcn_map[base];

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

      if (p == file_info_list.end ())
	{
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

std::string
genpath (const std::string& dirname, const string_vector& skip)
{
  std::string retval;

  std::string full_dirname = file_ops::tilde_expand (dirname);

  dir_entry dir (full_dirname);

  if (dir)
    {
      retval = dirname;

      string_vector dirlist = dir.read ();
      
      octave_idx_type len = dirlist.length ();

      for (octave_idx_type i = 0; i < len; i++)
	{
	  std::string elt = dirlist[i];

	  // FIXME -- the caller should be able to specify the list of
	  // directories to skip in addition to "." and "..".

	  bool skip_p = (elt == "." || elt == "..");

	  if (! skip_p)
	    {
	      for (octave_idx_type j = 0; j < skip.length (); j++)
		{
		  skip_p = (elt == skip[j]);
		  if (skip_p)
		    break;
		}

	      if (! skip_p)
		{
		  std::string nm = full_dirname + file_ops::dir_sep_str + elt;

		  file_stat fs (nm);

		  if (fs && fs.is_dir ())
		    retval += dir_path::path_sep_str + genpath (nm);
		}
	    }
	}
    }

  return retval;
}

static void
execute_pkg_add_or_del (const std::string& dir,
			const std::string& script_file)
{
  if (! octave_interpreter_ready)
    return;

  unwind_protect::begin_frame ("execute_pkg_add_or_del");

  unwind_protect_bool (input_from_startup_file);

  input_from_startup_file = true;

  std::string file = dir + file_ops::dir_sep_str + script_file;

  file_stat fs = file_stat (file);

  if (fs.exists ())
    source_file (file);

  unwind_protect::run_frame ("execute_pkg_add_or_del");
}

void
execute_pkg_add (const std::string& dir)
{
  execute_pkg_add_or_del (dir, "PKG_ADD");
}

void
execute_pkg_del (const std::string& dir)
{
  execute_pkg_add_or_del (dir, "PKG_DEL");
}

DEFUN (genpath, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} genpath (@var{dir})\n\
Return a path constructed from @var{dir} and all its subdiretories.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string dirname = args(0).string_value ();

      if (! error_state)
	retval = genpath (dirname);
      else
	error ("genpath: expecting argument to be a character string");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (rehash, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} rehash ()\n\
Reinitialize Octave's @code{LOADPATH} directory cache.\n\
@end deftypefn")
{
  octave_value_list retval;

  load_path::update ();

  // FIXME -- maybe we should rename this variable since it is being
  // used for more than keeping track of the prompt time.

  // This will force updated functions to be found.
  Vlast_prompt_time.stamp ();

  return retval;
}

DEFUN (pathdef, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} pathdef ()\n\
Return the default list of directories in which to search for function\n\
files.\n\
@seealso{path, addpath, rmpath, genpath, savepath, pathsep}\n\
@end deftypefn")
{
  return octave_value (Vsystem_path);
}

DEFUN (path, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Function File} {} path (@dots{})\n\
Modify or display Octave's @code{LOADPATH}.\n\
\n\
If @var{nargin} and @var{nargout} are zero, display the elements of\n\
Octave's @code{LOADPATH} in an easy to read format.\n\
\n\
If @var{nargin} is zero and nargout is greater than zero, return the\n\
current value of @code{LOADPATH}.\n\
\n\
If @var{nargin} is greater than zero, concatenate the arguments,\n\
separating them with @code{pathsep()}.  Set the internal search path\n\
to the result and return it.\n\
\n\
No checks are made for duplicate elements.\n\
@seealso{addpath, rmpath, genpath, pathdef, savepath, pathsep}\n\
@end deftypefn")
{
  octave_value retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("path");

  if (! error_state)
    {
      if (argc > 1)
	{
	  std::string path = argv[1];

	  for (int i = 2; i < argc; i++)
	    path += dir_path::path_sep_str;

	  size_t plen = path.length ();

	  if (! ((plen == 1 && path[0] == ':')
		 || (plen > 1
		     && path.substr (0, 2) == ("." + dir_path::path_sep_str))))
	    path = "." + dir_path::path_sep_str + path;

	  load_path::set (path);
	}

      if (nargout > 0)
	retval = load_path::path ();
      else if (argc == 1 && nargout == 0)
	{
	  octave_stdout << "\nOctave's search path contains the following directories:\n\n";

	  string_vector dirs = load_path::dirs ();

	  dirs.list_in_columns (octave_stdout);

	  octave_stdout << "\n";
	}
    }

  return retval;
}

DEFCMD (addpath, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {} addpath (@var{dir1}, @dots{})\n\
@deftypefnx {Function File} {} addpath (@var{dir1}, @dots{}, @var{option})\n\
Add @var{dir1}, @dots{} to the current function search path.  If\n\
@var{option} is @samp{\"-begin\"} or 0 (the default), prepend the\n\
directory name to the current path.  If @var{option} is @samp{\"-end\"}\n\
or 1, append the directory name to the current path.\n\
Directories added to the path must exist.\n\
@seealso{path, rmpath, genpath, pathdef, savepath, pathsep}\n\
@end deftypefn")
{
  octave_value retval;

  // Originally written by Bill Denney and Etienne Grossman.  Heavily
  // modified and translated to C++ by jwe.

  if (nargout > 0)
    retval = load_path::path ();

  int nargin = args.length ();

  if (nargin > 0)
    {
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
	  int val = option_arg.int_value ();

	  if (! error_state)
	    {
	      if (val == 0)
		append = false;
	      else if (val == 1)
		append = true;
	      else
		{
		  error ("addpath: expecting final argument to be 1 or 0");
		  return retval;
		}
	    }
	  else
	    {
	      error ("addpath: expecting final argument to be 1 or 0");
	      return retval;
	    }
	}

      std::list<std::string> xpath = load_path::dir_list ();

      // Strip "." for now.  Calling path to set the path will restore it.

      xpath.remove (".");

      for (int i = 0; i < nargin; i++)
	{
	  std::string arg = args(i).string_value ();

	  if (! error_state)
	    {
	      std::list<std::string> dir_elts = split_path (arg);

	      for (std::list<std::string>::const_iterator p = dir_elts.begin ();
		   p != dir_elts.end ();
		   p++)
		{
		  std::string dir = *p;

		  //dir = regexprep (dir_elts{j}, "//+", "/");
		  //dir = regexprep (dir, "/$", "");

		  if (dir == "." && append)
		    warning ("addpath: \".\" is always first in the path");

		  file_stat fs (dir);

		  if (fs)
		    {
		      if (fs.is_dir ())
			{
			  if (append)
			    load_path::append (dir);
			  else
			    load_path::prepend (dir);
			}
		      else
			warning ("addpath: %s: not a directory", dir.c_str ());
		    }
		  else
		    {
		      std::string msg = fs.error ();
		      warning ("addpath: %s: %s", dir.c_str (), msg.c_str ());
		    }
		}
	    }
	  else
	    error ("addpath: expecting all args to be character strings");
	}
    }
  else
    print_usage ();

  return retval;
}

DEFCMD (rmpath, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {} rmpath (@var{dir1}, @dots{})\n\
Remove @var{dir1}, @dots{} from the current function search path.\n\
\n\
@seealso{path, addpath, genpath, pathdef, savepath, pathsep}\n\
@end deftypefn")
{
  // Originally by Etienne Grossmann. Heavily modified and translated
  // to C++ by jwe.

  octave_value retval;

  if (nargout > 0)
    retval = load_path::path ();

  int nargin = args.length ();

  if (nargin > 0)
    {
      std::list<std::string> xpath = load_path::dir_list ();

      for (int i = 0; i < nargin; i++)
	{
	  std::string arg = args(i).string_value ();

	  if (! error_state)
	    {
	      std::list<std::string> dir_elts = split_path (arg);

	      for (std::list<std::string>::const_iterator p = dir_elts.begin ();
		   p != dir_elts.end ();
		   p++)
		{
		  std::string dir = *p;

		  //dir = regexprep (dir_elts{j}, "//+", "/");
		  //dir = regexprep (dir, "/$", "");

		  if (! load_path::remove (dir))
		    warning ("rmpath: %s: not found", dir.c_str ());
		}
	    }
	  else
	    error ("addpath: expecting all args to be character strings");
	}
    }
  else
    print_usage ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
