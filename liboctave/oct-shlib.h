/*

Copyright (C) 1999 John W. Eaton

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

#if !defined (octave_shlib_h)
#define octave_shlib_h 1

#include <string>

#include "oct-time.h"

// This just provides a way to avoid infinite recursion when building
// octave_shlib objects.

class
octave_xshlib
{
public:

  octave_xshlib (void) { }
};

class
octave_shlib
{
public:

  typedef std::string (*name_mangler) (const std::string&);

  typedef void (*close_hook) (const std::string&);

  octave_shlib (void) : rep (make_shlib ()) { }

  octave_shlib (const std::string& f) : rep (make_shlib ()) { open (f); }

  virtual ~octave_shlib (void)
    {
      if (rep && --rep->count == 0)
	{
	  delete rep;
	  rep = 0;
	}
    }

  octave_shlib (const octave_shlib& sl)
    {
      rep = sl.rep;
      rep->count++;
    }

  octave_shlib& operator = (const octave_shlib& sl)
    {
      if (rep != sl.rep)
	{
	  if (--rep->count == 0)
	    delete rep;

	  rep = sl.rep;
	  rep->count++;
	}

      return *this;
    }

  bool operator == (const octave_shlib& sl) const
    { return (rep == sl.rep); }

  operator bool () const { return is_open (); }

  virtual void open (const std::string& f) { rep->open (f); }
  
  virtual void *search (const std::string& nm, name_mangler mangler = 0)
    { return rep->search (nm, mangler); }

  virtual void close (close_hook cl_hook = 0)
    { rep->close (cl_hook); }

  virtual bool remove (const std::string& fcn_name)
    { return rep->remove (fcn_name); }

  virtual bool is_out_of_date (void) const
    { return rep->is_out_of_date (); }

  virtual int number_of_functions_loaded (void) const
    { return rep->number_of_functions_loaded (); }

  virtual std::string file_name (void) const
    { return rep->file_name (); }

  virtual octave_time time_loaded (void) const
    { return rep->time_loaded (); }

protected:

  octave_shlib (const octave_xshlib&) : rep (0) { }

  virtual bool is_open (void) const { return rep->is_open (); }

  static octave_shlib *make_shlib (void);

  union
    {
      octave_shlib *rep;
      int count;
    };
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
