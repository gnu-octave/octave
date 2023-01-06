////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2023 The Octave Project Developers
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

#if ! defined (octave_hook_fcn_h)
#define octave_hook_fcn_h 1

#include "octave-config.h"

#include <memory>
#include <string>

#include "ovl.h"
#include "ov.h"
#include "ov-fcn-handle.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_hook_function
{
public:

  base_hook_function (void) = default;

  base_hook_function (const base_hook_function&) = default;

  virtual ~base_hook_function (void) = default;

  virtual std::string id (void) const { return ""; }

  virtual bool is_valid (void) const { return false; }

  virtual void eval (const octave_value_list&) { }
};

class hook_function
{
public:

  hook_function (void)
  {
    static std::shared_ptr<base_hook_function>
    nil_rep (new base_hook_function ());

    m_rep = nil_rep;
  }

  hook_function (const octave_value& f,
                 const octave_value& d = octave_value ());

  ~hook_function (void) = default;

  hook_function (const hook_function& hf) = default;

  hook_function& operator = (const hook_function& hf) = default;

  std::string id (void) const { return m_rep->id (); }

  bool is_valid (void) const { return m_rep->is_valid (); }

  void eval (const octave_value_list& initial_args)
  {
    m_rep->eval (initial_args);
  }

private:

  std::shared_ptr<base_hook_function> m_rep;
};

class named_hook_function : public base_hook_function
{
public:

  named_hook_function (const std::string& n, const octave_value& d)
    : m_name (n), m_data (d)
  { }

  void eval (const octave_value_list& initial_args);

  std::string id (void) const { return m_name; }

  bool is_valid (void) const { return is_valid_function (m_name); }

private:

  std::string m_name;

  octave_value m_data;
};

class fcn_handle_hook_function : public base_hook_function
{
public:

  fcn_handle_hook_function (const octave_value& fh_arg, const octave_value& d)
    : m_ident (), m_valid (false), m_fcn_handle (fh_arg), m_data (d)
  {
    octave_fcn_handle *fh = m_fcn_handle.fcn_handle_value (true);

    if (fh)
      {
        m_valid = true;

        std::ostringstream buf;
        buf << fh;
        m_ident = fh->fcn_name () + ':' + buf.str ();
      }
  }

  void eval (const octave_value_list& initial_args);

  std::string id (void) const { return m_ident; }

  bool is_valid (void) const { return m_valid; }

private:

  std::string m_ident;

  bool m_valid;

  octave_value m_fcn_handle;

  octave_value m_data;
};

class hook_function_list
{
public:

  typedef std::map<std::string, hook_function> map_type;

  typedef map_type::iterator iterator;
  typedef map_type::const_iterator const_iterator;

  hook_function_list (void) = default;

  ~hook_function_list (void) = default;

  hook_function_list (const hook_function_list& lst) = default;

  hook_function_list& operator = (const hook_function_list& lst) = default;

  bool empty (void) const { return m_fcn_map.empty (); }

  void clear (void) { m_fcn_map.clear (); }

  void insert (const std::string& id, const hook_function& f)
  {
    m_fcn_map[id] = f;
  }

  iterator find (const std::string& id)
  {
    return m_fcn_map.find (id);
  }

  const_iterator find (const std::string& id) const
  {
    return m_fcn_map.find (id);
  }

  iterator end (void) { return m_fcn_map.end (); }

  const_iterator end (void) const { return m_fcn_map.end (); }

  void erase (iterator p) { m_fcn_map.erase (p); }

  void run (const octave_value_list& initial_args = octave_value_list ())
  {
    auto p = m_fcn_map.begin ();

    while (p != m_fcn_map.end ())
      {
        std::string hook_fcn_id = p->first;
        hook_function hook_fcn = p->second;

        auto q = p++;

        if (hook_fcn.is_valid ())
          hook_fcn.eval (initial_args);
        else
          m_fcn_map.erase (q);
      }
  }

private:

  map_type m_fcn_map;
};

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
OCTAVE_DEPRECATED (7, "use 'octave::base_hook_function' instead")
typedef octave::base_hook_function base_hook_function;

OCTAVE_DEPRECATED (7, "use 'octave::hook_function' instead")
typedef octave::hook_function hook_function;

OCTAVE_DEPRECATED (7, "use 'octave::named_hook_function' instead")
typedef octave::named_hook_function named_hook_function;

OCTAVE_DEPRECATED (7, "use 'octave::fcn_handle_hook_function' instead")
typedef octave::fcn_handle_hook_function fcn_handle_hook_function;

OCTAVE_DEPRECATED (7, "use 'octave::hook_function_list' instead")
typedef octave::hook_function_list hook_function_list;
#endif

#endif
