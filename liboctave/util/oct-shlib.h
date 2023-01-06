////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1999-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_oct_shlib_h)
#define octave_oct_shlib_h 1

#include "octave-config.h"

#include <functional>
#include <list>
#include <map>
#include <string>

#include "oct-time.h"
#include "oct-refcount.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
dynamic_library
{
public: // FIXME: make this class private?

  typedef std::function<std::string (const std::string&)> name_mangler;

  class dynlib_rep
  {
  public:

    dynlib_rep (void)
      : m_count (1), m_fcn_names (), m_file (), m_time_loaded (OCTAVE_TIME_T ()),
        m_search_all_loaded (false)
    { }

  protected:

    OCTAVE_API
    dynlib_rep (const std::string& f);

  public:

    virtual ~dynlib_rep (void)
    {
      s_instances.erase (m_file);
    }

    virtual bool is_open (void) const
    { return false; }

    virtual void * search (const std::string&,
                          const name_mangler& = name_mangler ())
    { return nullptr; }

    OCTAVE_API bool is_out_of_date (void) const;

    // This method will be overridden conditionally.
    static OCTAVE_API dynlib_rep * new_instance (const std::string& f);

    static OCTAVE_API dynlib_rep * get_instance (const std::string& f, bool fake);

    sys::time time_loaded (void) const
    { return m_time_loaded; }

    std::string file_name (void) const
    { return m_file; }

    std::size_t num_fcn_names (void) const { return m_fcn_names.size (); }

    OCTAVE_API std::list<std::string> function_names (void) const;

    OCTAVE_API void add_fcn_name (const std::string&);

    OCTAVE_API bool remove_fcn_name (const std::string&);

    void clear_fcn_names (void) { m_fcn_names.clear (); }

  public:

    refcount<octave_idx_type> m_count;

  protected:

    OCTAVE_API void fake_reload (void);

    static OCTAVE_API std::map<std::string, dynlib_rep *> s_instances;

    // Set of hooked function names.
    typedef std::map<std::string, std::size_t>::iterator fcn_names_iterator;
    typedef std::map<std::string, std::size_t>::const_iterator fcn_names_const_iterator;

    std::map<std::string, std::size_t> m_fcn_names;
    std::string m_file;
    sys::time m_time_loaded;
    bool m_search_all_loaded;
  };

private:

  static OCTAVE_API dynlib_rep s_nil_rep;

public:

  dynamic_library (void) : m_rep (&s_nil_rep) { m_rep->m_count++; }

  dynamic_library (const std::string& f, bool fake = true)
    : m_rep (dynlib_rep::get_instance (f, fake)) { }

  ~dynamic_library (void)
  {
    if (--m_rep->m_count == 0 && m_rep != &s_nil_rep)
      delete m_rep;
  }

  OCTAVE_API void delete_later (void);

  dynamic_library (const dynamic_library& sl)
    : m_rep (sl.m_rep)
  {
    m_rep->m_count++;
  }

  dynamic_library& operator = (const dynamic_library& sl)
  {
    if (m_rep != sl.m_rep)
      {
        if (--m_rep->m_count == 0 && m_rep != &s_nil_rep)
          delete m_rep;

        m_rep = sl.m_rep;
        m_rep->m_count++;
      }

    return *this;
  }

  bool operator == (const dynamic_library& sl) const
  { return (m_rep == sl.m_rep); }

  operator bool () const { return m_rep->is_open (); }

  void open (const std::string& f)
  { *this = dynamic_library (f); }

  std::list<std::string> close (void)
  {
    std::list<std::string> removed_fcns = m_rep->function_names ();

    m_rep->clear_fcn_names ();

    *this = dynamic_library ();

    return removed_fcns;
  }

  void * search (const std::string& nm,
                const name_mangler& mangler = name_mangler ()) const
  {
    void *f = m_rep->search (nm, mangler);
    if (f)
      m_rep->add_fcn_name (nm);

    return f;
  }

  void add (const std::string& name)
  { m_rep->add_fcn_name (name); }

  bool remove (const std::string& name)
  { return m_rep->remove_fcn_name (name); }

  std::size_t number_of_functions_loaded (void) const
  { return m_rep->num_fcn_names (); }

  bool is_out_of_date (void) const
  { return m_rep->is_out_of_date (); }

  std::string file_name (void) const
  { return m_rep->file_name (); }

  sys::time time_loaded (void) const
  { return m_rep->time_loaded (); }

private:

  dynlib_rep *m_rep;
};

// FIXME: Currently must return int so that it may be used as an
// event_hook function.

OCTAVE_API int release_unreferenced_dynamic_libraries (void);

OCTAVE_END_NAMESPACE(octave)

#endif
