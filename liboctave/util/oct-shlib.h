/*

Copyright (C) 1999-2017 John W. Eaton
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

#if ! defined (octave_oct_shlib_h)
#define octave_oct_shlib_h 1

#include "octave-config.h"

#include <list>
#include <string>
#include <map>

#include "oct-time.h"
#include "oct-refcount.h"

namespace octave
{
  class
  OCTAVE_API
  dynamic_library
  {
  public: // FIXME: make this class private?

    typedef std::string (*name_mangler) (const std::string&);

    class dynlib_rep
    {
    public:

      dynlib_rep (void)
        : count (1), file (), tm_loaded (time_t ()), fcn_names () { }

    protected:

      dynlib_rep (const std::string& f);

    public:

      virtual ~dynlib_rep (void)
      {
        instances.erase (file);
      }

      virtual bool is_open (void) const
      { return false; }

      virtual void * search (const std::string&, name_mangler = 0)
      { return 0; }

      bool is_out_of_date (void) const;

      // This method will be overridden conditionally.
      static dynlib_rep * new_instance (const std::string& f);

      static dynlib_rep * get_instance (const std::string& f, bool fake);

      sys::time time_loaded (void) const
      { return tm_loaded; }

      std::string file_name (void) const
      { return file; }

      size_t num_fcn_names (void) const { return fcn_names.size (); }

      std::list<std::string> function_names (void) const;

      void add_fcn_name (const std::string&);

      bool remove_fcn_name (const std::string&);

      void clear_fcn_names (void) { fcn_names.clear (); }

    public:

      refcount<int> count;

    protected:

      void fake_reload (void);

      std::string file;
      sys::time tm_loaded;

      // Set of hooked function names.
      typedef std::map<std::string, size_t>::iterator fcn_names_iterator;
      typedef std::map<std::string, size_t>::const_iterator fcn_names_const_iterator;

      std::map<std::string, size_t> fcn_names;

      static std::map<std::string, dynlib_rep *> instances;
    };

  private:

    static dynlib_rep nil_rep;

  public:

    dynamic_library (void) : rep (&nil_rep) { rep->count++; }

    dynamic_library (const std::string& f, bool fake = true)
      : rep (dynlib_rep::get_instance (f, fake)) { }

    ~dynamic_library (void)
    {
      if (--rep->count == 0)
        delete rep;
    }

    dynamic_library (const dynamic_library& sl)
      : rep (sl.rep)
    {
      rep->count++;
    }

    dynamic_library& operator = (const dynamic_library& sl)
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

    bool operator == (const dynamic_library& sl) const
    { return (rep == sl.rep); }

    operator bool () const { return rep->is_open (); }

    void open (const std::string& f)
    { *this = dynamic_library (f); }

    std::list<std::string> close (void)
    {
      std::list<std::string> removed_fcns = rep->function_names ();

      rep->clear_fcn_names ();

      *this = dynamic_library ();

      return removed_fcns;
    }

    void * search (const std::string& nm, name_mangler mangler = 0) const
    {
      void *f = rep->search (nm, mangler);
      if (f)
        rep->add_fcn_name (nm);

      return f;
    }

    void add (const std::string& name)
    { rep->add_fcn_name (name); }

    bool remove (const std::string& name)
    { return rep->remove_fcn_name (name); }

    size_t number_of_functions_loaded (void) const
    { return rep->num_fcn_names (); }

    bool is_out_of_date (void) const
    { return rep->is_out_of_date (); }

    std::string file_name (void) const
    { return rep->file_name (); }

    sys::time time_loaded (void) const
    { return rep->time_loaded (); }

  private:

    dynlib_rep *rep;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.2, "use 'octave::dynamic_library' instead")
typedef octave::dynamic_library octave_shlib;

#endif

#endif
