/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
              2002, 2004, 2005, 2006, 2007 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <list>

#include "oct-env.h"
#include "oct-time.h"
#include "file-stat.h"

#include <defaults.h>

#include "defun.h"
#include "dynamic-ld.h"
#include "ov-fcn.h"
#include "ov-dld-fcn.h"
#include "ov-mex-fcn.h"
#include "parse.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#define STRINGIFY(s) STRINGIFY1(s)
#define STRINGIFY1(s) #s

class
octave_shlib_list
{
public:

  typedef std::list<octave_shlib>::iterator iterator;
  typedef std::list<octave_shlib>::const_iterator const_iterator;

  static void append (const octave_shlib& shl);

  static void remove (octave_shlib& shl, octave_shlib::close_hook cl_hook = 0);

  static void *search (const std::string& fcn_name, octave_shlib& shl,
		       octave_shlib::name_mangler mangler = 0);

  static void display (void);

private:

  octave_shlib_list (void) { }

  ~octave_shlib_list (void) { }

  void do_append (const octave_shlib& shl);

  void do_remove (octave_shlib& shl, octave_shlib::close_hook cl_hook = 0);

  void *do_search (const std::string& fcn_name, octave_shlib& shl,
		   octave_shlib::name_mangler mangler = 0);

  void do_display (void) const;

  static octave_shlib_list *instance;

  static bool instance_ok (void);

  // List of libraries we have loaded.
  std::list<octave_shlib> lib_list;

  // No copying!

  octave_shlib_list (const octave_shlib_list&);

  octave_shlib_list& operator = (const octave_shlib_list&);
};

octave_shlib_list *octave_shlib_list::instance = 0;

void
octave_shlib_list::do_append (const octave_shlib& shl)
{
  lib_list.push_back (shl);
}

void
octave_shlib_list::do_remove (octave_shlib& shl,
			      octave_shlib::close_hook cl_hook)
{
  for (iterator p = lib_list.begin (); p != lib_list.end (); p++)
    {
      if (*p == shl)
	{
	  shl.close (cl_hook);

	  lib_list.erase (p);

	  break;
	}
    }
}

void *
octave_shlib_list::do_search (const std::string& fcn_name, octave_shlib& shl,
			      octave_shlib::name_mangler mangler)
{
  void *function = 0;

  shl = octave_shlib ();

  for (iterator p = lib_list.begin (); p != lib_list.end (); p++)
    {
      function = p->search (fcn_name, mangler);

      if (function)
	{
	  shl = *p;

	  break;
	}
    }

  return function;
}

void
octave_shlib_list::do_display (void) const
{
  std::cerr << "current shared libraries:" << std::endl;
  for (const_iterator p = lib_list.begin (); p != lib_list.end (); p++)
    std::cerr << "  " << p->file_name () << std::endl;
}

bool
octave_shlib_list::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    instance = new octave_shlib_list ();

  if (! instance)
    {
      ::error ("unable to create shared library list object!");

      retval = false;
    }

  return retval;
}

void
octave_shlib_list::append (const octave_shlib& shl)
{
  if (instance_ok ())
    instance->do_append (shl);
}

void
octave_shlib_list::remove (octave_shlib& shl,
			   octave_shlib::close_hook cl_hook)
{
  if (instance_ok ())
    instance->do_remove (shl, cl_hook);
}

void *
octave_shlib_list::search (const std::string& fcn_name, octave_shlib& shl,
			   octave_shlib::name_mangler mangler)
{
  return (instance_ok ()) ? instance->do_search (fcn_name, shl, mangler) : 0;
}

void
octave_shlib_list::display (void)
{
  if (instance_ok ())
    instance->do_display ();
}

class
octave_mex_file_list
{
public:

  typedef std::list<octave_shlib>::iterator iterator;
  typedef std::list<octave_shlib>::const_iterator const_iterator;

  static void append (const octave_shlib& shl);

  static void remove (octave_shlib& shl, octave_shlib::close_hook cl_hook = 0);

private:

  octave_mex_file_list (void) { }

  ~octave_mex_file_list (void) { }

  void do_append (const octave_shlib& shl);

  void do_remove (octave_shlib& shl, octave_shlib::close_hook cl_hook = 0);

  static octave_mex_file_list *instance;

  static bool instance_ok (void);

  // List of libraries we have loaded.
  std::list<octave_shlib> file_list;

  // No copying!

  octave_mex_file_list (const octave_mex_file_list&);

  octave_mex_file_list& operator = (const octave_mex_file_list&);
};

octave_mex_file_list *octave_mex_file_list::instance = 0;

void
octave_mex_file_list::do_append (const octave_shlib& shl)
{
  file_list.push_back (shl);
}

void
octave_mex_file_list::do_remove (octave_shlib& shl,
				 octave_shlib::close_hook cl_hook)
{
  for (iterator p = file_list.begin (); p != file_list.end (); p++)
    {
      if (*p == shl)
	{
	  shl.close (cl_hook);

	  file_list.erase (p);

	  break;
	}
    }
}

bool
octave_mex_file_list::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    instance = new octave_mex_file_list ();

  if (! instance)
    {
      ::error ("unable to create shared library list object!");

      retval = false;
    }

  return retval;
}

void
octave_mex_file_list::append (const octave_shlib& shl)
{
  if (instance_ok ())
    instance->do_append (shl);
}

void
octave_mex_file_list::remove (octave_shlib& shl,
			      octave_shlib::close_hook cl_hook)
{
  if (instance_ok ())
    instance->do_remove (shl, cl_hook);
}

octave_dynamic_loader *octave_dynamic_loader::instance = 0;

bool octave_dynamic_loader::doing_load = false;

bool
octave_dynamic_loader::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    instance = new octave_dynamic_loader ();

  if (! instance)
    {
      ::error ("unable to create dynamic loader object!");

      retval = false;
    }

  return retval;
}

static
void do_clear_function (const std::string& fcn_name)
{
  warning_with_id ("Octave:reload-forces-clear", "  %s", fcn_name.c_str ());

  symbol_table::clear_user_function (fcn_name);
}

static void
clear (octave_shlib& oct_file)
{
  if (oct_file.number_of_functions_loaded () > 1)
    warning_with_id ("Octave:reload-forces-clear",
		     "reloading %s clears the following functions:",
		     oct_file.file_name().c_str ());

  octave_shlib_list::remove (oct_file, do_clear_function);
}

octave_function *
octave_dynamic_loader::do_load_oct (const std::string& fcn_name,
				    const std::string& file_name,
				    bool relative)
{
  octave_function *retval = 0;

  octave_shlib oct_file;

  unwind_protect::begin_frame ("octave_dynamic_loader::do_load");

  unwind_protect_bool (octave_dynamic_loader::doing_load);

  doing_load = true;

  void *function
    = octave_shlib_list::search (fcn_name, oct_file, xmangle_name);

  if (! error_state)
    {
      bool reloading = false;

      if (function)
	{
	  // If there is already a function by this name installed
	  // from the same file, clear the file so we can reload it.

	  // If there is already a function by this name installed
	  // from a different file, leave the other file alone and
	  // load the function from the new file.

	  reloading = same_file (file_name, oct_file.file_name ());

	  if (reloading)
	    clear (oct_file);

	  function = 0;
	}

      if (! reloading)
	oct_file = octave_shlib ();

      oct_file.open (file_name);

      if (! error_state)
	{
	  if (oct_file)
	    {
	      if (relative)
		oct_file.mark_relative ();

	      octave_shlib_list::append (oct_file);

	      function = oct_file.search (fcn_name, xmangle_name);
	    }
	  else
	    ::error ("%s is not a valid shared library",
		     file_name.c_str ());
	}
    }

  if (function)
    {
      octave_dld_fcn_getter f
	= FCN_PTR_CAST (octave_dld_fcn_getter, function);

      retval = f (oct_file, relative);

      if (! retval)
	::error ("failed to install .oct file function `%s'",
		 fcn_name.c_str ());
    }
  
  unwind_protect::run_frame ("octave_dynamic_loader::do_load");

  return retval;
}

octave_function *
octave_dynamic_loader::do_load_mex (const std::string& fcn_name,
				    const std::string& file_name,
				    bool relative)
{
  octave_function *retval = 0;

  octave_shlib mex_file;

  unwind_protect::begin_frame ("octave_dynamic_loader::do_load");

  unwind_protect_bool (octave_dynamic_loader::doing_load);

  doing_load = true;

  std::string mex_file_name = file_name;

  if (mex_file_name.empty ())
    {
      mex_file_name = mex_file_in_path (fcn_name);

      if (! mex_file_name.empty ())
	relative = ! octave_env::absolute_pathname (mex_file_name);
    }

  void *function = 0;

  bool have_fmex = false;

  if (! mex_file_name.empty ())
    {
      mex_file.open (mex_file_name);

      if (! error_state)
	{
	  if (mex_file)
	    {
	      octave_mex_file_list::append (mex_file);

	      function = mex_file.search ("mexFunction");

	      if (! function)
		{
		  // FIXME -- can we determine this C mangling scheme
		  // automatically at run time or configure time?

		  function = mex_file.search ("_mexFunction");

		  if (! function)
		    {
		      function = mex_file.search (STRINGIFY (F77_FUNC (mexfunction, MEXFUNCTION)));
		      if (function)
			have_fmex = true;
		    }
		}
	    }
	  else
	    ::error ("%s is not a valid shared library",
		     mex_file_name.c_str ());
	}
    }

  if (function)
    retval = new octave_mex_function (function, have_fmex, mex_file, fcn_name);
  else
    ::error ("failed to install .mex file function `%s'", fcn_name.c_str ());
  
  unwind_protect::run_frame ("octave_dynamic_loader::do_load");

  return retval;
}

bool
octave_dynamic_loader::do_remove (const std::string& fcn_name, octave_shlib& shl)
{
  bool retval = false;

  // We don't need to do anything if this is called because we are in
  // the process of reloading a .oct file that has changed.

  if (! doing_load)
    {
      retval = shl.remove (fcn_name);

      if (shl.number_of_functions_loaded () == 0)
	octave_shlib_list::remove (shl);
    }

  return retval;
}

octave_function *
octave_dynamic_loader::load_oct (const std::string& fcn_name,
				  const std::string& file_name,
				  bool relative)
{
  return (instance_ok ())
    ? instance->do_load_oct (fcn_name, file_name, relative) : 0;
}

octave_function *
octave_dynamic_loader::load_mex (const std::string& fcn_name,
				  const std::string& file_name,
				  bool relative)
{
  return (instance_ok ())
    ? instance->do_load_mex (fcn_name, file_name, relative) : 0;
}

bool
octave_dynamic_loader::remove (const std::string& fcn_name, octave_shlib& shl)
{
  return (instance_ok ()) ? instance->do_remove (fcn_name, shl) : false;
}

std::string
octave_dynamic_loader::mangle_name (const std::string& name)
{
#if defined (CXX_PREPENDS_UNDERSCORE)
  std::string retval ("_FS");
#else
  std::string retval ("FS");
#endif
  retval.append (name);
  retval.append ("_");
  retval.append (STRINGIFY (CXX_ABI));
  return retval;
}

std::string
octave_dynamic_loader::xmangle_name (const std::string& name)
{
#if defined (CXX_PREPENDS_UNDERSCORE)
  std::string retval ("_G");
#else
  std::string retval ("G");
#endif
  retval.append (name);
  retval.append ("_");
  retval.append (STRINGIFY (CXX_ABI));
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
