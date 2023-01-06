////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#if ! defined (octave_graphics_h)
#define octave_graphics_h 1

#include "octave-config.h"

#include <cctype>
#include <cmath>

#include <algorithm>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include "caseless-str.h"

#include "errwarn.h"
#include "graphics-handle.h"
#include "graphics-toolkit.h"
#include "oct-map.h"
#include "oct-mutex.h"
#include "oct-refcount.h"
#include "ov.h"
#include "text-renderer.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// FIXME: maybe this should be a configure option?
// Matlab defaults to "Helvetica", but that causes problems for many
// gnuplot users.
#if ! defined (OCTAVE_DEFAULT_FONTNAME)
#define OCTAVE_DEFAULT_FONTNAME "*"
#endif

// ---------------------------------------------------------------------

class OCTINTERP_API base_scaler
{
public:
  base_scaler (void) { }

  virtual ~base_scaler (void) = default;

  virtual Matrix scale (const Matrix&) const
  {
    error ("invalid axis scale");
  }

  virtual NDArray scale (const NDArray&) const
  {
    error ("invalid axis scale");
  }

  virtual double scale (double) const
  {
    error ("invalid axis scale");
  }

  virtual double unscale (double) const
  {
    error ("invalid axis scale");
  }

  virtual base_scaler * clone () const
  { return new base_scaler (); }

  virtual bool is_linear (void) const
  { return false; }
};

class lin_scaler : public base_scaler
{
public:
  lin_scaler (void) { }

  Matrix scale (const Matrix& m) const { return m; }

  NDArray scale (const NDArray& m) const { return m; }

  double scale (double d) const { return d; }

  double unscale (double d) const { return d; }

  base_scaler * clone (void) const { return new lin_scaler (); }

  bool is_linear (void) const { return true; }
};

class log_scaler : public base_scaler
{
public:
  log_scaler (void) { }

  Matrix scale (const Matrix& m) const
  {
    Matrix retval (m.rows (), m.cols ());

    do_scale (m.data (), retval.fortran_vec (), m.numel ());

    return retval;
  }

  NDArray scale (const NDArray& m) const
  {
    NDArray retval (m.dims ());

    do_scale (m.data (), retval.fortran_vec (), m.numel ());

    return retval;
  }

  double scale (double d) const
  { return log10 (d); }

  double unscale (double d) const
  { return std::pow (10.0, d); }

  base_scaler * clone (void) const
  { return new log_scaler (); }

private:
  void do_scale (const double *src, double *dest, int n) const
  {
    for (int i = 0; i < n; i++)
      dest[i] = log10 (src[i]);
  }
};

class OCTINTERP_API neg_log_scaler : public base_scaler
{
public:
  neg_log_scaler (void) { }

  Matrix scale (const Matrix& m) const
  {
    Matrix retval (m.rows (), m.cols ());

    do_scale (m.data (), retval.fortran_vec (), m.numel ());

    return retval;
  }

  NDArray scale (const NDArray& m) const
  {
    NDArray retval (m.dims ());

    do_scale (m.data (), retval.fortran_vec (), m.numel ());

    return retval;
  }

  double scale (double d) const
  { return -log10 (-d); }

  double unscale (double d) const
  { return -std::pow (10.0, -d); }

  base_scaler * clone (void) const
  { return new neg_log_scaler (); }

private:
  void do_scale (const double *src, double *dest, int n) const
  {
    for (int i = 0; i < n; i++)
      dest[i] = -log10 (-src[i]);
  }
};

class OCTINTERP_API scaler
{
public:
  scaler (void) : m_rep (new base_scaler ()) { }

  scaler (const scaler& s) : m_rep (s.m_rep->clone ()) { }

  scaler (const std::string& s)
    : m_rep (s == "log"
             ? new log_scaler ()
             : (s == "neglog"
                ? new neg_log_scaler ()
                : (s == "linear"
                   ? new lin_scaler ()
                   : new base_scaler ())))
  { }

  ~scaler (void) { delete m_rep; }

  Matrix scale (const Matrix& m) const
  { return m_rep->scale (m); }

  NDArray scale (const NDArray& m) const
  { return m_rep->scale (m); }

  double scale (double d) const
  { return m_rep->scale (d); }

  double unscale (double d) const
  { return m_rep->unscale (d); }

  bool is_linear (void) const
  { return m_rep->is_linear (); }

  scaler& operator = (const scaler& s)
  {
    if (&s != this)
      {
        if (m_rep)
          {
            delete m_rep;
            m_rep = nullptr;
          }

        m_rep = s.m_rep->clone ();
      }

    return *this;
  }

  scaler& operator = (const std::string& s)
  {
    if (m_rep)
      {
        delete m_rep;
        m_rep = nullptr;
      }

    if (s == "log")
      m_rep = new log_scaler ();
    else if (s == "neglog")
      m_rep = new neg_log_scaler ();
    else if (s == "linear")
      m_rep = new lin_scaler ();
    else
      m_rep = new base_scaler ();

    return *this;
  }

private:
  base_scaler *m_rep;
};

// ---------------------------------------------------------------------

class OCTINTERP_API property;

// FIXME: These values should probably be defined inside a namespace or
// class, but which one is most appropriate?  For now, prefix with
// "GCB_" to avoid conflict with PERSISTENT token ID used in the lexer.
// The lexer token IDs should probably also be fixed...

enum listener_mode { GCB_POSTSET, GCB_PERSISTENT, GCB_PREDELETE };

class OCTINTERP_API base_property
{
public:
  friend class property;

public:
  base_property (void)
    : m_id (-1), m_count (1), m_name (), m_parent (), m_hidden (),
      m_listeners ()
  { }

  base_property (const std::string& s, const graphics_handle& h)
    : m_id (-1), m_count (1), m_name (s), m_parent (h), m_hidden (false),
      m_listeners ()
  { }

  base_property (const base_property& p)
    : m_id (-1), m_count (1), m_name (p.m_name), m_parent (p.m_parent),
      m_hidden (p.m_hidden), m_listeners ()
  { }

  virtual ~base_property (void) = default;

  bool ok (void) const { return m_parent.ok (); }

  std::string get_name (void) const { return m_name; }

  void set_name (const std::string& s) { m_name = s; }

  graphics_handle get_parent (void) const { return m_parent; }

  void set_parent (const graphics_handle& h) { m_parent = h; }

  bool is_hidden (void) const { return m_hidden; }

  void set_hidden (bool flag) { m_hidden = flag; }

  virtual bool is_radio (void) const { return false; }

  int get_id (void) const { return m_id; }

  void set_id (int d) { m_id = d; }

  // Sets property value, notifies graphics toolkit.
  // If do_run is true, runs associated listeners.
  OCTINTERP_API bool set (const octave_value& v, bool do_run = true,
                          bool do_notify_toolkit = true);

  virtual octave_value get (void) const
  {
    error (R"(get: invalid property "%s")", m_name.c_str ());
  }

  virtual std::string values_as_string (void) const
  {
    error (R"(values_as_string: invalid property "%s")", m_name.c_str ());
  }

  virtual Cell values_as_cell (void) const
  {
    error (R"(values_as_cell: invalid property "%s")", m_name.c_str ());
  }

  base_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  void add_listener (const octave_value& v, listener_mode mode = GCB_POSTSET)
  {
    octave_value_list& l = m_listeners[mode];
    l.resize (l.length () + 1, v);
  }

  void delete_listener (const octave_value& v = octave_value (),
                        listener_mode mode = GCB_POSTSET)
  {
    octave_value_list& l = m_listeners[mode];

    if (v.is_defined ())
      {
        bool found = false;
        int i;

        for (i = 0; i < l.length (); i++)
          {
            if (v.internal_rep () == l(i).internal_rep ())
              {
                found = true;
                break;
              }
          }
        if (found)
          {
            for (int j = i; j < l.length () - 1; j++)
              l(j) = l(j + 1);

            l.resize (l.length () - 1);
          }
      }
    else
      {
        if (mode == GCB_PERSISTENT)
          l.resize (0);
        else
          {
            octave_value_list lnew (0);
            octave_value_list& lp = m_listeners[GCB_PERSISTENT];
            for (int i = l.length () - 1; i >= 0 ; i--)
              {
                for (int j = 0; j < lp.length (); j++)
                  {
                    if (l(i).internal_rep () == lp(j).internal_rep ())
                      {
                        lnew.resize (lnew.length () + 1, l(i));
                        break;
                      }
                  }
              }
            l = lnew;
          }
      }

  }

  OCTINTERP_API void run_listeners (listener_mode mode = GCB_POSTSET);

  virtual base_property * clone (void) const
  { return new base_property (*this); }

protected:
  virtual bool do_set (const octave_value&)
  {
    error (R"(set: invalid property "%s")", m_name.c_str ());
  }

private:
  typedef std::map<listener_mode, octave_value_list> listener_map;
  typedef std::map<listener_mode, octave_value_list>::iterator
    listener_map_iterator;
  typedef std::map<listener_mode, octave_value_list>::const_iterator
    listener_map_const_iterator;

private:
  int m_id;
  octave::refcount<octave_idx_type> m_count;
  std::string m_name;
  graphics_handle m_parent;
  bool m_hidden;
  listener_map m_listeners;
};

// ---------------------------------------------------------------------

class OCTINTERP_API string_property : public base_property
{
public:
  string_property (const std::string& s, const graphics_handle& h,
                   const std::string& val = "")
    : base_property (s, h), m_str (val) { }

  string_property (const string_property& p)
    : base_property (p), m_str (p.m_str) { }

  octave_value get (void) const
  { return octave_value (m_str); }

  std::string string_value (void) const { return m_str; }

  string_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const { return new string_property (*this); }

protected:
  bool do_set (const octave_value& val)
  {
    if (! val.is_string ())
      error (R"(set: invalid string property value for "%s")",
             get_name ().c_str ());

    std::string new_str = val.string_value ();

    if (new_str != m_str)
      {
        m_str = new_str;
        return true;
      }
    return false;
  }

private:
  std::string m_str;
};

// ---------------------------------------------------------------------

class OCTINTERP_API string_array_property : public base_property
{
public:
  enum desired_enum { string_t, cell_t };

  string_array_property (const std::string& s, const graphics_handle& h,
                         const std::string& val = "", const char& sep = '|',
                         const desired_enum& typ = string_t)
    : base_property (s, h), m_desired_type (typ), m_separator (sep), m_str ()
  {
    std::size_t pos = 0;

    while (true)
      {
        std::size_t new_pos = val.find_first_of (m_separator, pos);

        if (new_pos == std::string::npos)
          {
            m_str.append (val.substr (pos));
            break;
          }
        else
          m_str.append (val.substr (pos, new_pos - pos));

        pos = new_pos + 1;
      }
  }

  string_array_property (const std::string& s, const graphics_handle& h,
                         const Cell& c, const char& sep = '|',
                         const desired_enum& typ = string_t)
    : base_property (s, h), m_desired_type (typ), m_separator (sep), m_str ()
  {
    if (! c.iscellstr ())
      error (R"(set: invalid order property value for "%s")",
             get_name ().c_str ());

    string_vector strings (c.numel ());

    for (octave_idx_type i = 0; i < c.numel (); i++)
      strings[i] = c(i).string_value ();

    m_str = strings;
  }

  string_array_property (const string_array_property& p)
    : base_property (p), m_desired_type (p.m_desired_type),
      m_separator (p.m_separator), m_str (p.m_str) { }

  octave_value get (void) const
  {
    if (m_desired_type == string_t)
      return octave_value (string_value ());
    else
      return octave_value (cell_value ());
  }

  std::string string_value (void) const
  {
    std::string s;

    for (octave_idx_type i = 0; i < m_str.numel (); i++)
      {
        s += m_str[i];
        if (i != m_str.numel () - 1)
          s += m_separator;
      }

    return s;
  }

  Cell cell_value (void) const {return Cell (m_str);}

  string_vector string_vector_value (void) const { return m_str; }

  string_array_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const
  { return new string_array_property (*this); }

protected:
  bool do_set (const octave_value& val)
  {
    if (val.is_string () && val.rows () == 1)
      {
        bool replace = false;
        std::string new_str = val.string_value ();
        string_vector strings;
        std::size_t pos = 0;

        // Split single string on delimiter (usually '|')
        while (pos != std::string::npos)
          {
            std::size_t new_pos = new_str.find_first_of (m_separator, pos);

            if (new_pos == std::string::npos)
              {
                strings.append (new_str.substr (pos));
                break;
              }
            else
              strings.append (new_str.substr (pos, new_pos - pos));

            pos = new_pos + 1;
          }

        if (m_str.numel () == strings.numel ())
          {
            for (octave_idx_type i = 0; i < m_str.numel (); i++)
              if (strings[i] != m_str[i])
                {
                  replace = true;
                  break;
                }
          }
        else
          replace = true;

        m_desired_type = string_t;

        if (replace)
          {
            m_str = strings;
            return true;
          }
      }
    else if (val.is_string ())  // multi-row character matrix
      {
        bool replace = false;
        charMatrix chm = val.char_matrix_value ();
        octave_idx_type nel = chm.rows ();
        string_vector strings (nel);

        if (nel != m_str.numel ())
          replace = true;
        for (octave_idx_type i = 0; i < nel; i++)
          {
            strings[i] = chm.row_as_string (i);
            if (! replace && strings[i] != m_str[i])
              replace = true;
          }

        m_desired_type = string_t;

        if (replace)
          {
            m_str = strings;
            return true;
          }
      }
    else if (val.iscellstr ())
      {
        bool replace = false;
        Cell new_cell = val.cell_value ();

        string_vector strings = new_cell.cellstr_value ();

        octave_idx_type nel = strings.numel ();

        if (nel != m_str.numel ())
          replace = true;
        else
          {
            for (octave_idx_type i = 0; i < nel; i++)
              {
                if (strings[i] != m_str[i])
                  {
                    replace = true;
                    break;
                  }
              }
          }

        m_desired_type = cell_t;

        if (replace)
          {
            m_str = strings;
            return true;
          }
      }
    else
      error (R"(set: invalid string property value for "%s")",
             get_name ().c_str ());

    return false;
  }

private:
  desired_enum m_desired_type;
  char m_separator;
  string_vector m_str;
};

// ---------------------------------------------------------------------

class OCTINTERP_API text_label_property : public base_property
{
public:
  enum type { char_t, cellstr_t };

  text_label_property (const std::string& s, const graphics_handle& h,
                       const std::string& val = "")
    : base_property (s, h), m_value (val), m_stored_type (char_t)
  { }

  text_label_property (const std::string& s, const graphics_handle& h,
                       const NDArray& nda)
    : base_property (s, h), m_stored_type (char_t)
  {
    octave_idx_type nel = nda.numel ();

    m_value.resize (nel);

    for (octave_idx_type i = 0; i < nel; i++)
      {
        std::ostringstream buf;
        buf << nda(i);
        m_value[i] = buf.str ();
      }
  }

  text_label_property (const std::string& s, const graphics_handle& h,
                       const Cell& c)
    : base_property (s, h), m_stored_type (cellstr_t)
  {
    octave_idx_type nel = c.numel ();

    m_value.resize (nel);

    for (octave_idx_type i = 0; i < nel; i++)
      {
        octave_value tmp = c(i);

        if (tmp.is_string ())
          m_value[i] = c(i).string_value ();
        else
          {
            double d = c(i).double_value ();

            std::ostringstream buf;
            buf << d;
            m_value[i] = buf.str ();
          }
      }
  }

  text_label_property (const text_label_property& p)
    : base_property (p), m_value (p.m_value), m_stored_type (p.m_stored_type)
  { }

  bool empty (void) const
  {
    octave_value tmp = get ();
    return tmp.isempty ();
  }

  octave_value get (void) const
  {
    if (m_stored_type == char_t)
      return octave_value (char_value ());
    else
      return octave_value (cell_value ());
  }

  std::string string_value (void) const
  {
    return m_value.empty () ? "" : m_value[0];
  }

  string_vector string_vector_value (void) const { return m_value; }

  charMatrix char_value (void) const { return charMatrix (m_value, ' '); }

  Cell cell_value (void) const {return Cell (m_value); }

  text_label_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const { return new text_label_property (*this); }

protected:

  bool do_set (const octave_value& val)
  {
    if (val.is_string ())
      {
        m_value = val.string_vector_value ();

        m_stored_type = char_t;
      }
    else if (val.iscell ())
      {
        Cell c = val.cell_value ();

        octave_idx_type nel = c.numel ();

        m_value.resize (nel);

        for (octave_idx_type i = 0; i < nel; i++)
          {
            octave_value tmp = c(i);

            if (tmp.is_string ())
              m_value[i] = c(i).string_value ();
            else
              {
                double d = c(i).double_value ();

                std::ostringstream buf;
                buf << d;
                m_value[i] = buf.str ();
              }
          }

        m_stored_type = cellstr_t;
      }
    else
      {
        NDArray nda;

        try
          {
            nda = val.array_value ();
          }
        catch (octave::execution_exception& ee)
          {
            error (ee, R"(set: invalid string property value for "%s")",
                   get_name ().c_str ());
          }

        octave_idx_type nel = nda.numel ();

        m_value.resize (nel);

        for (octave_idx_type i = 0; i < nel; i++)
          {
            std::ostringstream buf;
            buf << nda(i);
            m_value[i] = buf.str ();
          }

        m_stored_type = char_t;
      }

    return true;
  }

private:
  string_vector m_value;
  type m_stored_type;
};

// ---------------------------------------------------------------------

class OCTINTERP_API radio_values
{
public:
  OCTINTERP_API radio_values (const std::string& opt_string = "");

  radio_values (const radio_values& a)
    : m_default_val (a.m_default_val), m_possible_vals (a.m_possible_vals) { }

  radio_values& operator = (const radio_values& a)
  {
    if (&a != this)
      {
        m_default_val = a.m_default_val;
        m_possible_vals = a.m_possible_vals;
      }

    return *this;
  }

  std::string default_value (void) const { return m_default_val; }

  bool validate (const std::string& val, std::string& match)
  {
    bool retval = true;

    if (! contains (val, match))
      error ("invalid value = %s", val.c_str ());

    return retval;
  }

  bool contains (const std::string& val, std::string& match)
  {
    std::size_t k = 0;

    std::size_t len = val.length ();

    std::string first_match;

    for (const auto& possible_val : m_possible_vals)
      {
        if (possible_val.compare (val, len))
          {
            if (len == possible_val.length ())
              {
                // We found a full match (consider the case of val == "replace"
                // with possible values "replace" and "replacechildren").  Any
                // other matches are irrelevant, so set match and return now.
                match = possible_val;
                return true;
              }
            else
              {
                if (k == 0)
                  first_match = possible_val;

                k++;
              }
          }
      }

    if (k == 1)
      {
        match = first_match;
        return true;
      }
    else
      return false;
  }

  OCTINTERP_API std::string values_as_string (void) const;

  OCTINTERP_API Cell values_as_cell (void) const;

  octave_idx_type nelem (void) const { return m_possible_vals.size (); }

private:
  // Might also want to cache
  std::string m_default_val;
  std::set<caseless_str> m_possible_vals;
};

class OCTINTERP_API radio_property : public base_property
{
public:
  radio_property (const std::string& nm, const graphics_handle& h,
                  const radio_values& v = radio_values ())
    : base_property (nm, h),
      m_vals (v), m_current_val (v.default_value ()) { }

  radio_property (const std::string& nm, const graphics_handle& h,
                  const std::string& v)
    : base_property (nm, h),
      m_vals (v), m_current_val (m_vals.default_value ()) { }

  radio_property (const std::string& nm, const graphics_handle& h,
                  const radio_values& v, const std::string& def)
    : base_property (nm, h),
      m_vals (v), m_current_val (def) { }

  radio_property (const radio_property& p)
    : base_property (p), m_vals (p.m_vals), m_current_val (p.m_current_val) { }

  octave_value get (void) const { return octave_value (m_current_val); }

  const std::string& current_value (void) const { return m_current_val; }

  std::string values_as_string (void) const
  { return m_vals.values_as_string (); }

  Cell values_as_cell (void) const { return m_vals.values_as_cell (); }

  bool is (const caseless_str& v) const
  { return v.compare (m_current_val); }

  bool is_radio (void) const { return true; }

  radio_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const { return new radio_property (*this); }

protected:
  bool do_set (const octave_value& newval)
  {
    if (! newval.is_string ())
      error (R"(set: invalid value for radio property "%s")",
             get_name ().c_str ());

    std::string s = newval.string_value ();

    std::string match;

    if (! m_vals.validate (s, match))
      error (R"(set: invalid value for radio property "%s" (value = %s))",
             get_name ().c_str (), s.c_str ());

    if (match != m_current_val)
      {
        if (s.length () != match.length ())
          warning_with_id ("Octave:abbreviated-property-match",
                           "%s: allowing %s to match %s value %s",
                           "set", s.c_str (), get_name ().c_str (),
                           match.c_str ());
        m_current_val = match;
        return true;
      }
    return false;
  }

private:
  radio_values m_vals;
  std::string m_current_val;
};

// ---------------------------------------------------------------------

class OCTINTERP_API color_values
{
public:
  color_values (double r = 0, double g = 0, double b = 1)
    : m_rgb (1, 3)
  {
    m_rgb(0) = r;
    m_rgb(1) = g;
    m_rgb(2) = b;

    validate ();
  }

  color_values (const std::string& str)
    : m_rgb (1, 3)
  {
    if (! str2rgb (str))
      error ("invalid color specification: %s", str.c_str ());
  }

  color_values (const color_values& c)
    : m_rgb (c.m_rgb)
  { }

  color_values& operator = (const color_values& c)
  {
    if (&c != this)
      m_rgb = c.m_rgb;

    return *this;
  }

  bool operator == (const color_values& c) const
  {
    return (m_rgb(0) == c.m_rgb(0)
            && m_rgb(1) == c.m_rgb(1)
            && m_rgb(2) == c.m_rgb(2));
  }

  bool operator != (const color_values& c) const
  { return ! (*this == c); }

  Matrix rgb (void) const { return m_rgb; }

  operator octave_value (void) const { return m_rgb; }

  void validate (void) const
  {
    for (int i = 0; i < 3; i++)
      {
        if (m_rgb(i) < 0 ||  m_rgb(i) > 1)
          error ("invalid RGB color specification");
      }
  }

private:
  Matrix m_rgb;

  OCTINTERP_API bool str2rgb (const std::string& str);
};

class OCTINTERP_API color_property : public base_property
{
public:
  color_property (const color_values& c, const radio_values& v)
    : base_property ("", graphics_handle ()),
      m_current_type (color_t), m_color_val (c), m_radio_val (v),
      m_current_val (v.default_value ())
  { }

  color_property (const radio_values& v, const color_values& c)
    : base_property ("", graphics_handle ()),
      m_current_type (radio_t), m_color_val (c), m_radio_val (v),
      m_current_val (v.default_value ())
  { }

  color_property (const std::string& nm, const graphics_handle& h,
                  const color_values& c = color_values (),
                  const radio_values& v = radio_values ())
    : base_property (nm, h),
      m_current_type (color_t), m_color_val (c), m_radio_val (v),
      m_current_val (v.default_value ())
  { }

  color_property (const std::string& nm, const graphics_handle& h,
                  const radio_values& v)
    : base_property (nm, h),
      m_current_type (radio_t), m_color_val (color_values ()), m_radio_val (v),
      m_current_val (v.default_value ())
  { }

  color_property (const std::string& nm, const graphics_handle& h,
                  const std::string& v)
    : base_property (nm, h),
      m_current_type (radio_t), m_color_val (color_values ()), m_radio_val (v),
      m_current_val (m_radio_val.default_value ())
  { }

  color_property (const std::string& nm, const graphics_handle& h,
                  const color_property& v)
    : base_property (nm, h),
      m_current_type (v.m_current_type), m_color_val (v.m_color_val),
      m_radio_val (v.m_radio_val), m_current_val (v.m_current_val)
  { }

  color_property (const color_property& p)
    : base_property (p), m_current_type (p.m_current_type),
      m_color_val (p.m_color_val), m_radio_val (p.m_radio_val),
      m_current_val (p.m_current_val) { }

  octave_value get (void) const
  {
    if (m_current_type == color_t)
      return m_color_val.rgb ();

    return m_current_val;
  }

  bool is_rgb (void) const { return (m_current_type == color_t); }

  bool is_radio (void) const { return (m_current_type == radio_t); }

  bool is (const std::string& v) const
  { return (is_radio () && m_current_val == v); }

  Matrix rgb (void) const
  {
    if (m_current_type != color_t)
      error ("color has no RGB value");

    return m_color_val.rgb ();
  }

  const std::string& current_value (void) const
  {
    if (m_current_type != radio_t)
      error ("color has no radio value");

    return m_current_val;
  }

  color_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  operator octave_value (void) const { return get (); }

  base_property * clone (void) const { return new color_property (*this); }

  std::string values_as_string (void) const
  { return m_radio_val.values_as_string (); }

  Cell values_as_cell (void) const { return m_radio_val.values_as_cell (); }

protected:
  OCTINTERP_API bool do_set (const octave_value& newval);

private:
  enum current_enum { color_t, radio_t } m_current_type;
  color_values m_color_val;
  radio_values m_radio_val;
  std::string m_current_val;
};

// ---------------------------------------------------------------------

enum finite_type
{
  NO_CHECK,
  FINITE,
  NOT_NAN,
  NOT_INF
};

class OCTINTERP_API double_property : public base_property
{
public:
  double_property (const std::string& nm, const graphics_handle& h,
                   double d = 0)
    : base_property (nm, h),
      m_current_val (d), m_finite_constraint (NO_CHECK),
      m_minval (std::pair<double, bool> (octave_NaN, true)),
      m_maxval (std::pair<double, bool> (octave_NaN, true)) { }

  double_property (const double_property& p)
    : base_property (p), m_current_val (p.m_current_val),
      m_finite_constraint (NO_CHECK),
      m_minval (std::pair<double, bool> (octave_NaN, true)),
      m_maxval (std::pair<double, bool> (octave_NaN, true)) { }

  octave_value get (void) const { return octave_value (m_current_val); }

  double double_value (void) const { return m_current_val; }

  double_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const
  {
    double_property *p = new double_property (*this);

    p->m_finite_constraint = m_finite_constraint;
    p->m_minval = m_minval;
    p->m_maxval = m_maxval;

    return p;
  }

  void add_constraint (const std::string& type, double val, bool inclusive)
  {
    if (type == "min")
      m_minval = std::pair<double, bool> (val, inclusive);
    else if (type == "max")
      m_maxval = std::pair<double, bool> (val, inclusive);
  }

  void add_constraint (const finite_type finite)
  { m_finite_constraint = finite; }

protected:
  bool do_set (const octave_value& v)
  {
    if (! v.is_scalar_type () || ! v.isreal ())
      error (R"(set: invalid value for double property "%s")",
             get_name ().c_str ());

    double new_val = v.double_value ();

    // Check min and max
    if (! octave::math::isnan (m_minval.first))
      {
        if (m_minval.second && m_minval.first > new_val)
          error (R"(set: "%s" must be greater than or equal to %g)",
                 get_name ().c_str (), m_minval.first);
        else if (! m_minval.second && m_minval.first >= new_val)
          error (R"(set: "%s" must be greater than %g)",
                 get_name ().c_str (), m_minval.first);
      }

    if (! octave::math::isnan (m_maxval.first))
      {
        if (m_maxval.second && m_maxval.first < new_val)
          error (R"(set: "%s" must be less than or equal to %g)",
                 get_name ().c_str (), m_maxval.first);
        else if (! m_maxval.second && m_maxval.first <= new_val)
          error (R"(set: "%s" must be less than %g)",
                 get_name ().c_str (), m_maxval.first);
      }

    if (m_finite_constraint == NO_CHECK) { /* do nothing */ }
    else if (m_finite_constraint == FINITE)
      {
        if (! octave::math::isfinite (new_val))
          error (R"(set: "%s" must be finite)", get_name ().c_str ());
      }
    else if (m_finite_constraint == NOT_NAN)
      {
        if (octave::math::isnan (new_val))
          error (R"(set: "%s" must not be nan)", get_name ().c_str ());
      }
    else if (m_finite_constraint == NOT_INF)
      {
        if (octave::math::isinf (new_val))
          error (R"(set: "%s" must not be infinite)", get_name ().c_str ());
      }

    if (new_val != m_current_val)
      {
        m_current_val = new_val;
        return true;
      }

    return false;
  }

private:
  double m_current_val;
  finite_type m_finite_constraint;
  std::pair<double, bool> m_minval, m_maxval;
};

// ---------------------------------------------------------------------

class OCTINTERP_API double_radio_property : public base_property
{
public:
  double_radio_property (double d, const radio_values& v)
    : base_property ("", graphics_handle ()),
      m_current_type (double_t), m_dval (d), m_radio_val (v),
      m_current_val (v.default_value ())
  { }

  double_radio_property (const std::string& nm, const graphics_handle& h,
                         const std::string& v)
    : base_property (nm, h),
      m_current_type (radio_t), m_dval (0), m_radio_val (v),
      m_current_val (m_radio_val.default_value ())
  { }

  double_radio_property (const std::string& nm, const graphics_handle& h,
                         const double_radio_property& v)
    : base_property (nm, h),
      m_current_type (v.m_current_type), m_dval (v.m_dval),
      m_radio_val (v.m_radio_val), m_current_val (v.m_current_val)
  { }

  double_radio_property (const double_radio_property& p)
    : base_property (p), m_current_type (p.m_current_type),
      m_dval (p.m_dval), m_radio_val (p.m_radio_val),
      m_current_val (p.m_current_val) { }

  octave_value get (void) const
  {
    if (m_current_type == double_t)
      return m_dval;

    return m_current_val;
  }

  bool is_double (void) const { return (m_current_type == double_t); }

  bool is_radio (void) const { return (m_current_type == radio_t); }

  bool is (const std::string& v) const
  { return (is_radio () && m_current_val == v); }

  double double_value (void) const
  {
    if (m_current_type != double_t)
      error ("%s: property has no double", get_name ().c_str ());

    return m_dval;
  }

  const std::string& current_value (void) const
  {
    if (m_current_type != radio_t)
      error ("%s: property has no radio value", get_name ().c_str ());

    return m_current_val;
  }

  double_radio_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  operator octave_value (void) const { return get (); }

  base_property * clone (void) const
  { return new double_radio_property (*this); }

protected:
  OCTINTERP_API bool do_set (const octave_value& v);

private:
  enum current_enum { double_t, radio_t } m_current_type;
  double m_dval;
  radio_values m_radio_val;
  std::string m_current_val;
};

// ---------------------------------------------------------------------

class OCTINTERP_API array_property : public base_property
{
public:
  array_property (void)
    : base_property ("", graphics_handle ()), m_data (Matrix ()),
      m_min_val (), m_max_val (), m_min_pos (), m_max_neg (),
      m_type_constraints (), m_size_constraints (),
      m_finite_constraint (NO_CHECK),
      m_minval (std::pair<double, bool> (octave_NaN, true)),
      m_maxval (std::pair<double, bool> (octave_NaN, true))
  {
    get_data_limits ();
  }

  array_property (const std::string& nm, const graphics_handle& h,
                  const octave_value& m)
    : base_property (nm, h), m_data (m.issparse () ? m.full_value () : m),
      m_min_val (), m_max_val (), m_min_pos (), m_max_neg (),
      m_type_constraints (), m_size_constraints (),
      m_finite_constraint (NO_CHECK),
      m_minval (std::pair<double, bool> (octave_NaN, true)),
      m_maxval (std::pair<double, bool> (octave_NaN, true))
  {
    get_data_limits ();
  }

  // This copy constructor is only intended to be used
  // internally to access min/max values; no need to
  // copy constraints.
  array_property (const array_property& p)
    : base_property (p), m_data (p.m_data),
      m_min_val (p.m_min_val), m_max_val (p.m_max_val),
      m_min_pos (p.m_min_pos), m_max_neg (p.m_max_neg),
      m_type_constraints (), m_size_constraints (),
      m_finite_constraint (NO_CHECK),
      m_minval (std::pair<double, bool> (octave_NaN, true)),
      m_maxval (std::pair<double, bool> (octave_NaN, true))
  { }

  octave_value get (void) const { return m_data; }

  void add_constraint (const std::string& type)
  { m_type_constraints.insert (type); }

  void add_constraint (const dim_vector& dims)
  { m_size_constraints.push_back (dims); }

  void add_constraint (const finite_type finite)
  { m_finite_constraint = finite; }

  void add_constraint (const std::string& type, double val, bool inclusive)
  {
    if (type == "min")
      m_minval = std::pair<double, bool> (val, inclusive);
    else if (type == "max")
      m_maxval = std::pair<double, bool> (val, inclusive);
  }

  double min_val (void) const { return m_min_val; }
  double max_val (void) const { return m_max_val; }
  double min_pos (void) const { return m_min_pos; }
  double max_neg (void) const { return m_max_neg; }

  Matrix get_limits (void) const
  {
    Matrix m (1, 4);

    m(0) = min_val ();
    m(1) = max_val ();
    m(2) = min_pos ();
    m(3) = max_neg ();

    return m;
  }

  array_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const
  {
    array_property *p = new array_property (*this);

    p->m_type_constraints = m_type_constraints;
    p->m_size_constraints = m_size_constraints;
    p->m_finite_constraint = m_finite_constraint;
    p->m_minval = m_minval;
    p->m_maxval = m_maxval;

    return p;
  }

protected:
  bool do_set (const octave_value& v)
  {
    octave_value tmp = (v.issparse () ? v.full_value () : v);

    if (! validate (tmp))
      error (R"(invalid value for array property "%s")",
             get_name ().c_str ());

    // FIXME: should we check for actual data change?
    if (! is_equal (tmp))
      {
        m_data = tmp;

        get_data_limits ();

        return true;
      }

    return false;
  }

private:
  OCTINTERP_API bool validate (const octave_value& v);

  OCTINTERP_API bool is_equal (const octave_value& v) const;

  OCTINTERP_API void get_data_limits (void);

protected:
  octave_value m_data;
  double m_min_val;
  double m_max_val;
  double m_min_pos;
  double m_max_neg;
  std::set<std::string> m_type_constraints;
  std::list<dim_vector> m_size_constraints;
  finite_type m_finite_constraint;
  std::pair<double, bool> m_minval, m_maxval;
};

class OCTINTERP_API row_vector_property : public array_property
{
public:
  row_vector_property (const std::string& nm, const graphics_handle& h,
                       const octave_value& m)
    : array_property (nm, h, m)
  {
    add_constraint (dim_vector (-1, 1));
    add_constraint (dim_vector (1, -1));
    add_constraint (dim_vector (0, 0));
  }

  row_vector_property (const row_vector_property& p)
    : array_property (p)
  {
    add_constraint (dim_vector (-1, 1));
    add_constraint (dim_vector (1, -1));
    add_constraint (dim_vector (0, 0));
  }

  void add_constraint (const std::string& type)
  {
    array_property::add_constraint (type);
  }

  void add_constraint (const dim_vector& dims)
  {
    array_property::add_constraint (dims);
  }

  void add_constraint (const finite_type finite)
  {
    array_property::add_constraint (finite);
  }

  void add_constraint (const std::string& type, double val, bool inclusive)
  {
    array_property::add_constraint (type, val, inclusive);
  }

  void add_constraint (octave_idx_type len)
  {
    m_size_constraints.remove (dim_vector (1, -1));
    m_size_constraints.remove (dim_vector (-1, 1));
    m_size_constraints.remove (dim_vector (0, 0));

    add_constraint (dim_vector (1, len));
    add_constraint (dim_vector (len, 1));
  }

  row_vector_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const
  {
    row_vector_property *p = new row_vector_property (*this);

    p->m_type_constraints = m_type_constraints;
    p->m_size_constraints = m_size_constraints;
    p->m_finite_constraint = m_finite_constraint;
    p->m_minval = m_minval;
    p->m_maxval = m_maxval;

    return p;
  }

protected:
  bool do_set (const octave_value& v)
  {
    bool retval = array_property::do_set (v);

    dim_vector dv = m_data.dims ();

    if (dv(0) > 1 && dv(1) == 1)
      {
        int tmp = dv(0);
        dv(0) = dv(1);
        dv(1) = tmp;

        m_data = m_data.reshape (dv);
      }

    return retval;
  }

private:
  OCTINTERP_API bool validate (const octave_value& v);
};

// ---------------------------------------------------------------------

class OCTINTERP_API bool_property : public radio_property
{
public:
  bool_property (const std::string& nm, const graphics_handle& h,
                 bool val)
    : radio_property (nm, h, radio_values (val ? "{on}|off" : "on|{off}"))
  { }

  bool_property (const std::string& nm, const graphics_handle& h,
                 const char *val)
    : radio_property (nm, h, radio_values (std::string (val) == "on" ?
                                           "{on}|off" : "on|{off}"), val)
  { }

  bool_property (const bool_property& p)
    : radio_property (p) { }

  bool is_on (void) const { return is ("on"); }

  bool_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const { return new bool_property (*this); }

protected:
  bool do_set (const octave_value& val)
  {
    if (val.is_bool_scalar ())
      return radio_property::do_set (val.bool_value () ? "on" : "off");
    else
      return radio_property::do_set (val);
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API handle_property : public base_property
{
public:
  handle_property (const std::string& nm, const graphics_handle& h,
                   const graphics_handle& val = graphics_handle ())
    : base_property (nm, h),
      m_current_val (val) { }

  handle_property (const handle_property& p)
    : base_property (p), m_current_val (p.m_current_val) { }

  octave_value get (void) const { return m_current_val.as_octave_value (); }

  graphics_handle handle_value (void) const { return m_current_val; }

  handle_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  handle_property& operator = (const graphics_handle& h)
  {
    set (octave_value (h.value ()));
    return *this;
  }

  void invalidate (void)
  { m_current_val = octave::numeric_limits<double>::NaN (); }

  base_property * clone (void) const { return new handle_property (*this); }

  void add_constraint (const std::string& type)
  { m_type_constraints.insert (type); }

protected:
  OCTINTERP_API bool do_set (const octave_value& v);
  std::set<std::string> m_type_constraints;

private:
  graphics_handle m_current_val;
};

// ---------------------------------------------------------------------

class OCTINTERP_API any_property : public base_property
{
public:
  any_property (const std::string& nm, const graphics_handle& h,
                const octave_value& m = Matrix ())
    : base_property (nm, h), m_data (m) { }

  any_property (const any_property& p)
    : base_property (p), m_data (p.m_data) { }

  octave_value get (void) const { return m_data; }

  any_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const { return new any_property (*this); }

protected:
  bool do_set (const octave_value& v)
  {
    m_data = v;
    return true;
  }

private:
  octave_value m_data;
};

// ---------------------------------------------------------------------

class OCTINTERP_API children_property : public base_property
{
public:
  children_property (void)
    : base_property ("", graphics_handle ()), m_children_list ()
  {
    do_init_children (Matrix ());
  }

  children_property (const std::string& nm, const graphics_handle& h,
                     const Matrix& val)
    : base_property (nm, h), m_children_list ()
  {
    do_init_children (val);
  }

  children_property (const children_property& p)
    : base_property (p), m_children_list ()
  {
    do_init_children (p.m_children_list);
  }

  children_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const { return new children_property (*this); }

  bool remove_child (double val)
  {
    return do_remove_child (val);
  }

  void adopt (double val)
  {
    do_adopt_child (val);
  }

  Matrix get_children (void) const
  {
    return do_get_children (false);
  }

  Matrix get_hidden (void) const
  {
    return do_get_children (true);
  }

  Matrix get_all (void) const
  {
    return do_get_all_children ();
  }

  octave_value get (void) const
  {
    return octave_value (get_children ());
  }

  void delete_children (bool clear = false, bool from_root = false)
  {
    do_delete_children (clear, from_root);
  }

  void renumber (graphics_handle old_gh, graphics_handle new_gh)
  {
    for (auto& hchild : m_children_list)
      {
        if (hchild == old_gh)
          {
            hchild = new_gh.value ();
            return;
          }
      }

    error ("children_list::renumber: child not found!");
  }

private:
  typedef std::list<double>::iterator children_list_iterator;
  typedef std::list<double>::const_iterator const_children_list_iterator;
  std::list<double> m_children_list;

protected:
  bool do_set (const octave_value& val)
  {
    Matrix new_kids;

    try
      {
        new_kids = val.matrix_value ();
      }
    catch (octave::execution_exception& ee)
      {
        error (ee, "set: children must be an array of graphics handles");
      }

    octave_idx_type nel = new_kids.numel ();

    const Matrix new_kids_column = new_kids.reshape (dim_vector (nel, 1));

    bool is_ok = true;
    bool add_hidden = true;

    const Matrix visible_kids = do_get_children (false);
    const Matrix hidden_kids = do_get_children (true);

    if (visible_kids.numel () == new_kids.numel ())
      {
        Matrix t1 = visible_kids.sort ();
        Matrix t2 = new_kids_column.sort ();
        Matrix t3 = hidden_kids.sort ();

        if (t1 != t2)
          is_ok = false;

        if (t1 == t3)
          add_hidden = false;
      }
    else
      is_ok = false;

    if (! is_ok)
      error ("set: new children list must be a permutation of existing "
             "children with visible handles");

    m_children_list.clear ();

    // Don't use do_init_children here, as that reverses the
    // order of the list, and we don't want to do that if setting
    // the child list directly.
    for (octave_idx_type i = 0; i < new_kids_column.numel (); i++)
      m_children_list.push_back (new_kids_column.xelem (i));

    if (add_hidden)
      for (octave_idx_type i = 0; i < hidden_kids.numel (); i++)
        m_children_list.push_back (hidden_kids.xelem (i));

    return true;
  }

private:
  void do_init_children (const Matrix& val)
  {
    m_children_list.clear ();
    for (octave_idx_type i = 0; i < val.numel (); i++)
      m_children_list.push_front (val.xelem (i));
  }

  void do_init_children (const std::list<double>& val)
  {
    m_children_list.clear ();
    m_children_list = val;
  }

  OCTINTERP_API Matrix do_get_children (bool return_hidden) const;

  Matrix do_get_all_children (void) const
  {
    Matrix retval (m_children_list.size (), 1);
    octave_idx_type i = 0;

    for (const auto& hchild : m_children_list)
      retval(i++) = hchild;

    return retval;
  }

  bool do_remove_child (double child)
  {
    for (auto it = m_children_list.begin (); it != m_children_list.end (); it++)
      {
        if (*it == child)
          {
            m_children_list.erase (it);
            return true;
          }
      }
    return false;
  }

  void do_adopt_child (double val)
  {
    m_children_list.push_front (val);
  }

  void do_delete_children (bool clear, bool from_root);
};

// ---------------------------------------------------------------------

class OCTINTERP_API callback_property : public base_property
{
public:
  callback_property (const std::string& nm, const graphics_handle& h,
                     const octave_value& m)
    : base_property (nm, h), m_callback (m) { }

  callback_property (const callback_property& p)
    : base_property (p), m_callback (p.m_callback) { }

  octave_value get (void) const { return m_callback; }

  OCTINTERP_API void execute (const octave_value& data = octave_value ()) const;

  bool is_defined (void) const
  {
    return (m_callback.is_defined () && ! m_callback.isempty ());
  }

  callback_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property * clone (void) const { return new callback_property (*this); }

protected:
  bool do_set (const octave_value& v)
  {
    if (! validate (v))
      error (R"(invalid value for callback property "%s")",
             get_name ().c_str ());

    m_callback = v;
    return true;
    return false;
  }

private:
  OCTINTERP_API bool validate (const octave_value& v) const;

private:
  octave_value m_callback;
};

// ---------------------------------------------------------------------

class OCTINTERP_API property
{
public:
  property (void) : m_rep (new base_property ("", graphics_handle ()))
  { }

  property (base_property *bp, bool persist = false) : m_rep (bp)
  { if (persist) m_rep->m_count++; }

  property (const property& p) : m_rep (p.m_rep)
  {
    m_rep->m_count++;
  }

  ~property (void)
  {
    if (--m_rep->m_count == 0)
      delete m_rep;
  }

  bool ok (void) const
  { return m_rep->ok (); }

  std::string get_name (void) const
  { return m_rep->get_name (); }

  void set_name (const std::string& name)
  { m_rep->set_name (name); }

  graphics_handle get_parent (void) const
  { return m_rep->get_parent (); }

  void set_parent (const graphics_handle& h)
  { m_rep->set_parent (h); }

  bool is_hidden (void) const
  { return m_rep->is_hidden (); }

  void set_hidden (bool flag)
  { m_rep->set_hidden (flag); }

  bool is_radio (void) const
  { return m_rep->is_radio (); }

  int get_id (void) const
  { return m_rep->get_id (); }

  void set_id (int d)
  { m_rep->set_id (d); }

  octave_value get (void) const
  { return m_rep->get (); }

  bool set (const octave_value& val, bool do_run = true,
            bool do_notify_toolkit = true)
  { return m_rep->set (val, do_run, do_notify_toolkit); }

  std::string values_as_string (void) const
  { return m_rep->values_as_string (); }

  Cell values_as_cell (void) const
  { return m_rep->values_as_cell (); }

  property& operator = (const octave_value& val)
  {
    *m_rep = val;
    return *this;
  }

  property& operator = (const property& p)
  {
    if (m_rep && --m_rep->m_count == 0)
      delete m_rep;

    m_rep = p.m_rep;
    m_rep->m_count++;

    return *this;
  }

  void add_listener (const octave_value& v, listener_mode mode = GCB_POSTSET)
  { m_rep->add_listener (v, mode); }

  void delete_listener (const octave_value& v = octave_value (),
                        listener_mode mode = GCB_POSTSET)
  { m_rep->delete_listener (v, mode); }

  void run_listeners (listener_mode mode = GCB_POSTSET)
  { m_rep->run_listeners (mode); }

  static OCTINTERP_API property
  create (const std::string& name, const graphics_handle& parent,
          const caseless_str& type, const octave_value_list& args);

  property clone (void) const
  { return property (m_rep->clone ()); }

#if 0
  const string_property& as_string_property (void) const
  { return *(dynamic_cast<string_property *> (m_rep)); }

  const radio_property& as_radio_property (void) const
  { return *(dynamic_cast<radio_property *> (m_rep)); }

  const color_property& as_color_property (void) const
  { return *(dynamic_cast<color_property *> (m_rep)); }

  const double_property& as_double_property (void) const
  { return *(dynamic_cast<double_property *> (m_rep)); }

  const bool_property& as_bool_property (void) const
  { return *(dynamic_cast<bool_property *> (m_rep)); }

  const handle_property& as_handle_property (void) const
  { return *(dynamic_cast<handle_property *> (m_rep)); }
#endif

private:
  base_property *m_rep;
};

// ---------------------------------------------------------------------

typedef std::pair<std::string, octave_value> pval_pair;

class OCTINTERP_API pval_vector : public std::vector<pval_pair>
{
public:
  const_iterator find (const std::string pname) const
  {
    const_iterator it;

    for (it = (*this).begin (); it != (*this).end (); it++)
      if (pname == (*it).first)
        return it;

    return (*this).end ();
  }

  iterator find (const std::string pname)
  {
    iterator it;

    for (it = (*this).begin (); it != (*this).end (); it++)
      if (pname == (*it).first)
        return it;

    return (*this).end ();
  }

  octave_value lookup (const std::string pname) const
  {
    octave_value retval;

    const_iterator it = find (pname);

    if (it != (*this).end ())
      retval = (*it).second;

    return retval;
  }

  octave_value& operator [] (const std::string pname)
  {
    iterator it = find (pname);

    if (it == (*this).end ())
      {
        push_back (pval_pair (pname, octave_value ()));
        return (*this).back ().second;
      }

    return (*it).second;
  }

  void erase (const std::string pname)
  {
    iterator it = find (pname);
    if (it != (*this).end ())
      erase (it);
  }

  void erase (iterator it)
  {
    std::vector<pval_pair>::erase (it);
  }

};

class OCTINTERP_API property_list
{
public:
  typedef pval_vector pval_map_type;
  typedef std::map<std::string, pval_map_type> plist_map_type;

  typedef pval_map_type::iterator pval_map_iterator;
  typedef pval_map_type::const_iterator pval_map_const_iterator;

  typedef plist_map_type::iterator plist_map_iterator;
  typedef plist_map_type::const_iterator plist_map_const_iterator;

  property_list (const plist_map_type& m = plist_map_type ())
    : m_plist_map (m) { }

  ~property_list (void) = default;

  OCTINTERP_API void set (const caseless_str& name, const octave_value& val);

  OCTINTERP_API octave_value lookup (const caseless_str& name) const;

  plist_map_iterator begin (void) { return m_plist_map.begin (); }
  plist_map_const_iterator begin (void) const { return m_plist_map.begin (); }

  plist_map_iterator end (void) { return m_plist_map.end (); }
  plist_map_const_iterator end (void) const { return m_plist_map.end (); }

  plist_map_iterator find (const std::string& go_name)
  {
    return m_plist_map.find (go_name);
  }

  plist_map_const_iterator find (const std::string& go_name) const
  {
    return m_plist_map.find (go_name);
  }

  OCTINTERP_API octave_scalar_map
  as_struct (const std::string& prefix_arg) const;

private:
  plist_map_type m_plist_map;
};

// ---------------------------------------------------------------------

class base_graphics_object;
class graphics_object;

class OCTINTERP_API base_properties
{
public:
  base_properties (const std::string& ty = "unknown",
                   const graphics_handle& mh = graphics_handle (),
                   const graphics_handle& p = graphics_handle ());

  virtual ~base_properties (void) = default;

  virtual std::string graphics_object_name (void) const { return "unknown"; }

  OCTINTERP_API void mark_modified (void);

  OCTINTERP_API void override_defaults (base_graphics_object& obj);

  virtual void init_integerhandle (const octave_value&)
  {
    panic_impossible ();
  }

  // Look through DEFAULTS for properties with given CLASS_NAME, and
  // apply them to the current object with set (virtual method).

  OCTINTERP_API void
  set_from_list (base_graphics_object& obj, property_list& defaults);

  void insert_property (const std::string& name, property p)
  {
    p.set_name (name);
    p.set_parent (m___myhandle__);
    m_all_props[name] = p;
  }

  virtual void set (const caseless_str&, const octave_value&);

  virtual octave_value get (const caseless_str& pname) const;

  virtual octave_value get (const std::string& pname) const
  {
    return get (caseless_str (pname));
  }

  virtual octave_value get (const char *pname) const
  {
    return get (caseless_str (pname));
  }

  virtual octave_value get (bool all = false) const;

  // FIXME: It seems like this function should be const, but that is
  // currently not possible with the way that properties are stored as
  // specific types in the graphics_object classes.
  virtual property get_property (const caseless_str& pname);

  virtual bool has_property (const caseless_str&) const
  {
    panic_impossible ();
    return false;
  }

  bool is_modified (void) const { return is___modified__ (); }

  virtual void remove_child (const graphics_handle& h, bool = false)
  {
    if (m_children.remove_child (h.value ()))
      {
        m_children.run_listeners ();
        mark_modified ();
      }
  }

  virtual void adopt (const graphics_handle& h)
  {
    m_children.adopt (h.value ());
    m_children.run_listeners ();
    mark_modified ();
  }

  virtual octave::graphics_toolkit get_toolkit (void) const;

  virtual Matrix
  get_boundingbox (bool /* finternal */ = false,
                   const Matrix& /* parent_pix_size */ = Matrix ()) const
  { return Matrix (1, 4, 0.0); }

  virtual void update_boundingbox (void);

  virtual void update_autopos (const std::string& elem_type);

  virtual void add_listener (const caseless_str&, const octave_value&,
                             listener_mode = GCB_POSTSET);

  virtual void delete_listener (const caseless_str&, const octave_value&,
                                listener_mode = GCB_POSTSET);

  void set_beingdeleted (const octave_value& val)
  {
    m_beingdeleted.set (val, true, false);
    update_beingdeleted ();
  }

  void set_tag (const octave_value& val) { m_tag = val; }

  OCTINTERP_API void set_parent (const octave_value& val);

  Matrix get_children (void) const
  {
    return m_children.get_children ();
  }

  Matrix get_all_children (void) const
  {
    return m_children.get_all ();
  }

  Matrix get_hidden_children (void) const
  {
    return m_children.get_hidden ();
  }

  OCTINTERP_API void
  get_children_of_type (const caseless_str& type, bool get_invisible,
                        bool traverse,
                        std::list<graphics_object> &children_list) const;

  void set_modified (const octave_value& val) { set___modified__ (val); }

  void set___modified__ (const octave_value& val) { m___modified__ = val; }

  // Redirect calls to "uicontextmenu" to "contextmenu".

  graphics_handle get_uicontextmenu (void) const
  {
    return get_contextmenu ();
  }

  void set_uicontextmenu (const octave_value& val)
  {
    set_contextmenu (val);
  }

  void reparent (const graphics_handle& new_parent) { m_parent = new_parent; }

  // Update data limits for AXIS_TYPE (xdata, ydata, etc.) in the parent
  // axes object.

  virtual void update_axis_limits (const std::string& axis_type) const;

  virtual void update_axis_limits (const std::string& axis_type,
                                   const graphics_handle& h) const;

  virtual void update_contextmenu (void) const;

  virtual void delete_children (bool clear = false, bool from_root = false)
  {
    m_children.delete_children (clear, from_root);
  }

  void renumber_child (graphics_handle old_gh, graphics_handle new_gh)
  {
    m_children.renumber (old_gh, new_gh);
  }

  void renumber_parent (graphics_handle new_gh)
  {
    m_parent = new_gh;
  }

  static OCTINTERP_API property_list::pval_map_type factory_defaults (void);

  // FIXME: These functions should be generated automatically by the
  //        genprops.awk script.
  //
  // EMIT_BASE_PROPERTIES_GET_FUNCTIONS

  virtual octave_value get_alim (void) const { return octave_value (); }
  virtual octave_value get_clim (void) const { return octave_value (); }
  virtual octave_value get_xlim (void) const { return octave_value (); }
  virtual octave_value get_ylim (void) const { return octave_value (); }
  virtual octave_value get_zlim (void) const { return octave_value (); }

  virtual bool is_aliminclude (void) const { return false; }
  virtual bool is_climinclude (void) const { return false; }
  virtual bool is_xliminclude (void) const { return false; }
  virtual bool is_yliminclude (void) const { return false; }
  virtual bool is_zliminclude (void) const { return false; }

  OCTINTERP_API bool is_handle_visible (void) const;

  OCTINTERP_API std::set<std::string> dynamic_property_names (void) const;

  OCTINTERP_API bool has_dynamic_property (const std::string& pname) const;

protected:
  std::set<std::string> m_dynamic_properties;

  OCTINTERP_API void
  set_dynamic (const caseless_str& pname, const octave_value& val);

  OCTINTERP_API octave_value get_dynamic (const caseless_str& pname) const;

  OCTINTERP_API octave_value get_dynamic (bool all = false) const;

  OCTINTERP_API property get_property_dynamic (const caseless_str& pname) const;

  BEGIN_BASE_PROPERTIES
    // properties common to all objects
    bool_property beingdeleted s , "off"
    radio_property busyaction , "{queue}|cancel"
    callback_property buttondownfcn , Matrix ()
    children_property children gf , Matrix ()
    bool_property clipping , "on"
    handle_property contextmenu u , graphics_handle ()
    callback_property createfcn , Matrix ()
    callback_property deletefcn , Matrix ()
    radio_property handlevisibility u , "{on}|callback|off"
    bool_property hittest , "on"
    bool_property interruptible , "on"
    handle_property parent fs , p
    radio_property pickableparts , "{visible}|all|none"
    bool_property selected , "off"
    bool_property selectionhighlight , "on"
    string_property tag s , ""
    string_property type frs , ty
    handle_property uicontextmenu gsh , graphics_handle ()
    any_property userdata , Matrix ()
    bool_property visible u , "on"

    // Octave-specific properties
    any_property __appdata__ h , Matrix ()
    bool_property __modified__ hs , "on"
    graphics_handle __myhandle__ fhrs , mh
  END_PROPERTIES

  virtual void update_beingdeleted (void) { };

  virtual void update_handlevisibility (void);

  virtual void update_visible (void) { };

protected:
  struct cmp_caseless_str
  {
  public:
    bool operator () (const caseless_str& a, const caseless_str& b) const
    {
      std::string a1 = a;
      std::transform (a1.begin (), a1.end (), a1.begin (), tolower);
      std::string b1 = b;
      std::transform (b1.begin (), b1.end (), b1.begin (), tolower);

      return a1 < b1;
    }
  };

  std::map<caseless_str, property, cmp_caseless_str> m_all_props;

protected:

  virtual void init (void)
  {
    m_contextmenu.add_constraint ("uicontextmenu");
  }
};

class OCTINTERP_API base_graphics_object
{
public:
  friend class graphics_object;

  base_graphics_object (void) : m_toolkit_flag (false) { }

  // No copying!

  base_graphics_object (const base_graphics_object&) = delete;

  base_graphics_object& operator = (const base_graphics_object&) = delete;

  virtual ~base_graphics_object (void) = default;

  virtual void mark_modified (void)
  {
    if (! valid_object ())
      error ("base_graphics_object::mark_modified: invalid graphics object");

    get_properties ().mark_modified ();
  }

  virtual void override_defaults (base_graphics_object& obj)
  {
    if (! valid_object ())
      error ("base_graphics_object::override_defaults: invalid graphics object");
    get_properties ().override_defaults (obj);
  }

  void build_user_defaults_map (property_list::pval_map_type& def,
                                const std::string go_name) const;

  virtual void set_from_list (property_list& plist)
  {
    if (! valid_object ())
      error ("base_graphics_object::set_from_list: invalid graphics object");

    get_properties ().set_from_list (*this, plist);
  }

  virtual void set (const caseless_str& pname, const octave_value& pval)
  {
    if (! valid_object ())
      error ("base_graphics_object::set: invalid graphics object");

    get_properties ().set (pname, pval);
  }

  virtual void set_defaults (const std::string&)
  {
    error ("base_graphics_object::set_defaults: invalid graphics object");
  }

  // The following version of the get method is not declared virtual
  // because no derived class overrides it.

  octave_value get (bool all = false) const
  {
    if (! valid_object ())
      error ("base_graphics_object::get: invalid graphics object");

    return get_properties ().get (all);
  }

  virtual octave_value get (const caseless_str& pname) const
  {
    if (! valid_object ())
      error ("base_graphics_object::get: invalid graphics object");

    return get_properties ().get (pname);
  }

  virtual octave_value get_default (const caseless_str&) const;

  virtual octave_value get_factory_default (const caseless_str&) const;

  virtual octave_value get_defaults (void) const
  {
    error ("base_graphics_object::get_defaults: invalid graphics object");
  }

  virtual property_list get_defaults_list (void) const
  {
    if (! valid_object ())
      error ("base_graphics_object::get_defaults_list: invalid graphics object");

    return property_list ();
  }

  virtual octave_value get_factory_defaults (void) const
  {
    error ("base_graphics_object::get_factory_defaults: invalid graphics object");
  }

  virtual property_list get_factory_defaults_list (void) const
  {
    error ("base_graphics_object::get_factory_defaults_list: invalid graphics object");
  }

  virtual bool has_readonly_property (const caseless_str& pname) const
  {
    return base_properties::has_readonly_property (pname);
  }

  // FIXME: It seems like this function should be const, but that is
  // currently not possible.
  virtual std::string values_as_string (void);

  // FIXME: It seems like this function should be const, but that is
  // currently not possible.
  virtual std::string value_as_string (const std::string& prop);

  // FIXME: It seems like this function should be const, but that is
  // currently not possible.
  virtual octave_scalar_map values_as_struct (void);

  virtual graphics_handle get_parent (void) const
  {
    if (! valid_object ())
      error ("base_graphics_object::get_parent: invalid graphics object");

    return get_properties ().get_parent ();
  }

  graphics_handle get_handle (void) const
  {
    if (! valid_object ())
      error ("base_graphics_object::get_handle: invalid graphics object");

    return get_properties ().get___myhandle__ ();
  }

  virtual void remove_child (const graphics_handle& h, bool from_root = false)
  {
    if (! valid_object ())
      error ("base_graphics_object::remove_child: invalid graphics object");

    get_properties ().remove_child (h, from_root);
  }

  virtual void adopt (const graphics_handle& h)
  {
    if (! valid_object ())
      error ("base_graphics_object::adopt: invalid graphics object");

    get_properties ().adopt (h);
  }

  virtual void reparent (const graphics_handle& np)
  {
    if (! valid_object ())
      error ("base_graphics_object::reparent: invalid graphics object");

    get_properties ().reparent (np);
  }

  virtual void defaults (void) const
  {
    if (! valid_object ())
      error ("base_graphics_object::default: invalid graphics object");

    std::string msg = (type () + "::defaults");
    err_not_implemented (msg.c_str ());
  }

  virtual base_properties& get_properties (void)
  {
    static base_properties properties;
    warning ("base_graphics_object::get_properties: invalid graphics object");
    return properties;
  }

  virtual const base_properties& get_properties (void) const
  {
    static base_properties properties;
    warning ("base_graphics_object::get_properties: invalid graphics object");
    return properties;
  }

  virtual void update_axis_limits (const std::string& axis_type);

  virtual void update_axis_limits (const std::string& axis_type,
                                   const graphics_handle& h);

  virtual bool valid_object (void) const { return false; }

  bool valid_toolkit_object (void) const { return m_toolkit_flag; }

  virtual std::string type (void) const
  {
    return (valid_object () ? get_properties ().graphics_object_name ()
                            : "unknown");
  }

  bool isa (const std::string& go_name) const
  {
    return type () == go_name;
  }

  virtual octave::graphics_toolkit get_toolkit (void) const
  {
    if (! valid_object ())
      error ("base_graphics_object::get_toolkit: invalid graphics object");

    return get_properties ().get_toolkit ();
  }

  virtual void add_property_listener (const std::string& nm,
                                      const octave_value& v,
                                      listener_mode mode = GCB_POSTSET)
  {
    if (valid_object ())
      get_properties ().add_listener (nm, v, mode);
  }

  virtual void delete_property_listener (const std::string& nm,
                                         const octave_value& v,
                                         listener_mode mode = GCB_POSTSET)
  {
    if (valid_object ())
      get_properties ().delete_listener (nm, v, mode);
  }

  virtual void remove_all_listeners (void);

  virtual void reset_default_properties (void);

protected:
  virtual void initialize (const graphics_object& go)
  {
    if (! m_toolkit_flag)
      m_toolkit_flag = get_toolkit ().initialize (go);
  }

  virtual void finalize (const graphics_object& go)
  {
    if (m_toolkit_flag)
      {
        get_toolkit ().finalize (go);
        m_toolkit_flag = false;
      }
  }

  virtual void update (const graphics_object& go, int id)
  {
    if (m_toolkit_flag)
      get_toolkit ().update (go, id);
  }

protected:

  // A flag telling whether this object is a valid object
  // in the backend context.
  bool m_toolkit_flag;
};

class OCTINTERP_API graphics_object
{
public:

  graphics_object (void) : m_rep (new base_graphics_object ()) { }

  graphics_object (base_graphics_object *new_rep) : m_rep (new_rep) { }

  graphics_object (const graphics_object&) = default;

  graphics_object& operator = (const graphics_object&) = default;

  ~graphics_object (void) = default;

  void mark_modified (void) { m_rep->mark_modified (); }

  void override_defaults (base_graphics_object& obj)
  {
    m_rep->override_defaults (obj);
  }

  void override_defaults (void)
  {
    m_rep->override_defaults (*m_rep);
  }

  void build_user_defaults_map (property_list::pval_map_type& def,
                                const std::string go_name) const
  {
    m_rep->build_user_defaults_map (def, go_name);
  }

  void set_from_list (property_list& plist) { m_rep->set_from_list (plist); }

  void set (const caseless_str& name, const octave_value& val)
  {
    m_rep->set (name, val);
  }

  OCTINTERP_API void set (const octave_value_list& args);

  OCTINTERP_API void set (const Array<std::string>& names, const Cell& values,
                          octave_idx_type row);

  OCTINTERP_API void set (const octave_map& m);

  OCTINTERP_API void set_value_or_default (const caseless_str& name,
                                           const octave_value& val);

  void set_defaults (const std::string& mode) { m_rep->set_defaults (mode); }

  octave_value get (bool all = false) const { return m_rep->get (all); }

  octave_value get (const caseless_str& name) const
  {
    return name.compare ("default")
           ? get_defaults ()
           : (name.compare ("factory")
              ? get_factory_defaults () : m_rep->get (name));
  }

  octave_value get (const std::string& name) const
  {
    return get (caseless_str (name));
  }

  octave_value get (const char *name) const
  {
    return get (caseless_str (name));
  }

  octave_value get_default (const caseless_str& name) const
  {
    return m_rep->get_default (name);
  }

  octave_value get_factory_default (const caseless_str& name) const
  {
    return m_rep->get_factory_default (name);
  }

  octave_value get_defaults (void) const { return m_rep->get_defaults (); }

  property_list get_defaults_list (void) const
  {
    return m_rep->get_defaults_list ();
  }

  octave_value get_factory_defaults (void) const
  {
    return m_rep->get_factory_defaults ();
  }

  property_list get_factory_defaults_list (void) const
  {
    return m_rep->get_factory_defaults_list ();
  }

  bool has_readonly_property (const caseless_str& pname) const
  {
    return m_rep->has_readonly_property (pname);
  }

  // FIXME: It seems like this function should be const, but that is
  // currently not possible.
  std::string values_as_string (void) { return m_rep->values_as_string (); }

  // FIXME: It seems like this function should be const, but that is
  // currently not possible.
  std::string value_as_string (const std::string& prop)
  {
    return m_rep->value_as_string (prop);
  }

  // FIXME: It seems like this function should be const, but that is
  // currently not possible.
  octave_map values_as_struct (void) { return m_rep->values_as_struct (); }

  graphics_handle get_parent (void) const { return m_rep->get_parent (); }

  graphics_handle get_handle (void) const { return m_rep->get_handle (); }

  OCTINTERP_API graphics_object get_ancestor (const std::string& type) const;

  void remove_child (const graphics_handle& h) { m_rep->remove_child (h); }

  void adopt (const graphics_handle& h) { m_rep->adopt (h); }

  void reparent (const graphics_handle& h) { m_rep->reparent (h); }

  void defaults (void) const { m_rep->defaults (); }

  bool isa (const std::string& go_name) const { return m_rep->isa (go_name); }

  base_properties& get_properties (void) { return m_rep->get_properties (); }

  const base_properties& get_properties (void) const
  {
    return m_rep->get_properties ();
  }

  void update_axis_limits (const std::string& axis_type)
  {
    m_rep->update_axis_limits (axis_type);
  }

  void update_axis_limits (const std::string& axis_type,
                           const graphics_handle& h)
  {
    m_rep->update_axis_limits (axis_type, h);
  }

  bool valid_object (void) const { return m_rep->valid_object (); }

  std::string type (void) const { return m_rep->type (); }

  operator bool (void) const { return m_rep->valid_object (); }

  // FIXME: These functions should be generated automatically by the
  //        genprops.awk script.
  //
  // EMIT_GRAPHICS_OBJECT_GET_FUNCTIONS

  octave_value get_alim (void) const
  { return get_properties ().get_alim (); }

  octave_value get_clim (void) const
  { return get_properties ().get_clim (); }

  octave_value get_xlim (void) const
  { return get_properties ().get_xlim (); }

  octave_value get_ylim (void) const
  { return get_properties ().get_ylim (); }

  octave_value get_zlim (void) const
  { return get_properties ().get_zlim (); }

  bool is_aliminclude (void) const
  { return get_properties ().is_aliminclude (); }

  bool is_climinclude (void) const
  { return get_properties ().is_climinclude (); }

  bool is_xliminclude (void) const
  { return get_properties ().is_xliminclude (); }

  bool is_yliminclude (void) const
  { return get_properties ().is_yliminclude (); }

  bool is_zliminclude (void) const
  { return get_properties ().is_zliminclude (); }

  bool is_handle_visible (void) const
  { return get_properties ().is_handle_visible (); }

  octave::graphics_toolkit get_toolkit (void) const
  { return m_rep->get_toolkit (); }

  void add_property_listener (const std::string& nm, const octave_value& v,
                              listener_mode mode = GCB_POSTSET)
  { m_rep->add_property_listener (nm, v, mode); }

  void delete_property_listener (const std::string& nm, const octave_value& v,
                                 listener_mode mode = GCB_POSTSET)
  { m_rep->delete_property_listener (nm, v, mode); }

  void remove_all_listeners (void) { m_rep->remove_all_listeners (); }

  void initialize (void) { m_rep->initialize (*this); }

  void finalize (void) { m_rep->finalize (*this); }

  void update (int id) { m_rep->update (*this, id); }

  void reset_default_properties (void)
  { m_rep->reset_default_properties (); }

private:

  std::shared_ptr<base_graphics_object> m_rep;
};

// ---------------------------------------------------------------------

class OCTINTERP_API root_figure : public base_graphics_object
{
public:

  // The gh_manager constructor creates the single instance of
  // the root_figure object.

  friend class gh_manager;

  class OCTINTERP_API properties : public base_properties
  {
  public:
    OCTINTERP_API void
    remove_child (const graphics_handle& h, bool from_root = false);

    OCTINTERP_API Matrix
    get_boundingbox (bool internal = false,
                     const Matrix& parent_pix_size = Matrix ()) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.

    // FIXME: Properties that still don't have callbacks are:
    // monitorpositions, pointerlocation, pointerwindow.
    // Note that these properties are not yet used by Octave, so setting
    // them will have no effect.

    // FIXME: The commandwindowsize property has been deprecated in Matlab
    //        and is now available through matlab.desktop.comandwindow.size.
    //        Until Octave has something similar, keep this property in root.

    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (root_figure, root)
      handle_property callbackobject Sr , graphics_handle ()
      array_property commandwindowsize r , Matrix (1, 2, 0)
      handle_property currentfigure S , graphics_handle ()
      string_property fixedwidthfontname , "Courier"
      array_property monitorpositions r , default_screensize ()
      array_property pointerlocation , Matrix (1, 2, 0)
      double_property pointerwindow r , 0.0
      double_property screendepth r , default_screendepth ()
      double_property screenpixelsperinch r , default_screenpixelsperinch ()
      array_property screensize r , default_screensize ()
      bool_property showhiddenhandles , "off"
      radio_property units U , "{pixels}|inches|centimeters|points|normalized|characters"
      // Hide base properties which don't make sense for root object
      //radio_property beingdeleted h , "{off}|on"
    END_PROPERTIES
  };

private:

  properties m_properties;

protected:

  root_figure (void)
    : m_properties (0, graphics_handle ()), m_default_properties (),
      m_factory_properties (init_factory_properties ())
  { }

public:

  ~root_figure (void) = default;

  root_figure (const root_figure&) = delete;

  root_figure& operator = (const root_figure&) = delete;

  void mark_modified (void) { }

  void override_defaults (base_graphics_object& obj)
  {
    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (m_default_properties);
  }

  void set (const caseless_str& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      m_default_properties.set (name.substr (7), value);
    else
      m_properties.set (name, value);
  }

  octave_value get (const caseless_str& name) const
  {
    octave_value retval;

    if (name.compare ("default", 7))
      return get_default (name.substr (7));
    else if (name.compare ("factory", 7))
      return get_factory_default (name.substr (7));
    else
      retval = m_properties.get (name);

    return retval;
  }

  octave_value get_default (const caseless_str& name) const
  {
    octave_value retval = m_default_properties.lookup (name);

    if (retval.is_undefined ())
      {
        // no default property found, use factory default
        retval = m_factory_properties.lookup (name);

        if (retval.is_undefined ())
          error ("get: invalid default property '%s'", name.c_str ());
      }

    return retval;
  }

  octave_value get_factory_default (const caseless_str& name) const
  {
    octave_value retval = m_factory_properties.lookup (name);

    if (retval.is_undefined ())
      error ("get: invalid factory default property '%s'", name.c_str ());

    return retval;
  }

  octave_value get_defaults (void) const
  {
    return m_default_properties.as_struct ("default");
  }

  property_list get_defaults_list (void) const
  {
    return m_default_properties;
  }

  octave_value get_factory_defaults (void) const
  {
    return m_factory_properties.as_struct ("factory");
  }

  property_list get_factory_defaults_list (void) const
  {
    return m_factory_properties;
  }

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  OCTINTERP_API void reset_default_properties (void);

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

private:

  property_list m_default_properties;

  property_list m_factory_properties;

  static OCTINTERP_API property_list::plist_map_type
  init_factory_properties (void);
};

// ---------------------------------------------------------------------

class OCTINTERP_API figure : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:
    void init_integerhandle (const octave_value& val)
    {
      m_integerhandle = val;
    }

    OCTINTERP_API void
    remove_child (const graphics_handle& h, bool from_root = false);

    OCTINTERP_API void set_visible (const octave_value& val);

    OCTINTERP_API octave::graphics_toolkit get_toolkit (void) const;

    OCTINTERP_API void set_toolkit (const octave::graphics_toolkit& b);

    OCTINTERP_API void set___graphics_toolkit__ (const octave_value& val);

    OCTINTERP_API void adopt (const graphics_handle& h);

    // Alias "innerposition" to "position".
    octave_value get_innerposition (void) const
    {
      return get_position ();
    }

    void set_innerposition (const octave_value& val)
    {
      set_position (val);
    }

    OCTINTERP_API void set_position (const octave_value& val,
                                     bool do_notify_toolkit = true);

    OCTINTERP_API void set_outerposition (const octave_value& val,
                                          bool do_notify_toolkit = true);

    OCTINTERP_API Matrix bbox2position (const Matrix& bbox) const;

    OCTINTERP_API Matrix
    get_boundingbox (bool internal = false,
                     const Matrix& parent_pix_size = Matrix ()) const;

    OCTINTERP_API void
    set_boundingbox (const Matrix& bb, bool internal = false,
                     bool do_notify_toolkit = true);

    OCTINTERP_API Matrix map_from_boundingbox (double x, double y) const;

    OCTINTERP_API Matrix map_to_boundingbox (double x, double y) const;

    OCTINTERP_API void update_units (const caseless_str& old_units);

    OCTINTERP_API void update_paperunits (const caseless_str& old_paperunits);

    OCTINTERP_API std::string get_title (void) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (figure)
      array_property alphamap , Matrix (64, 1, 1)
      callback_property buttondownfcn , Matrix ()
      callback_property closerequestfcn , "closereq"
      color_property color , color_property (color_values (1, 1, 1), radio_values ("none"))
      array_property colormap , viridis_colormap ()
      handle_property currentaxes S , graphics_handle ()
      string_property currentcharacter r , ""
      handle_property currentobject r , graphics_handle ()
      array_property currentpoint r , Matrix (2, 1, 0)
      bool_property dockcontrols , "on"
      string_property filename , ""
      bool_property graphicssmoothing , "on"
      array_property innerposition sg , default_figure_position ()
      bool_property integerhandle S , "on"
      bool_property inverthardcopy , "on"
      callback_property keypressfcn , Matrix ()
      callback_property keyreleasefcn , Matrix ()
      radio_property menubar , "{figure}|none"
      string_property name , ""
      array_property number rG , Matrix ()
      radio_property nextplot , "{add}|new|replace|replacechildren"
      bool_property numbertitle , "on"
      array_property outerposition s , Matrix (1, 4, -1.0)
      radio_property paperorientation U , "{portrait}|landscape"
      array_property paperposition m , default_figure_paperposition ()
      // FIXME: Matlab default is "auto", but this messes up hgsave BIST test.
      radio_property paperpositionmode au , "{auto}|manual"
      array_property papersize U , default_figure_papersize ()
      radio_property papertype SU , "{usletter}|uslegal|a0|a1|a2|a3|a4|a5|b0|b1|b2|b3|b4|b5|arch-a|arch-b|arch-c|arch-d|arch-e|a|b|c|d|e|tabloid|<custom>"
      radio_property paperunits Su , "{inches}|centimeters|normalized|points"
      radio_property pointer , "{arrow}|crosshair|ibeam|watch|topl|topr|botl|botr|left|top|right|bottom|circle|cross|fleur|custom|hand"
      array_property pointershapecdata , Matrix (16, 16, 1)
      array_property pointershapehotspot , Matrix (1, 2, 1)
      array_property position s , default_figure_position ()
      radio_property renderer m , "{opengl}|painters"
      radio_property renderermode , "{auto}|manual"
      bool_property resize , "on"
      // FIXME: "resizefcn" is no longer recommended by Matlab,
      //        and has been replaced with "sizechangedfcn"
      //        Eventually this will need to be hidden, and then removed.
      callback_property resizefcn , Matrix ()
      radio_property selectiontype , "{normal}|extend|alt|open"
      callback_property sizechangedfcn , Matrix ()
      radio_property toolbar , "{auto}|figure|none"
      radio_property units Su , "{pixels}|normalized|inches|centimeters|points|characters"
      callback_property windowbuttondownfcn , Matrix ()
      callback_property windowbuttonmotionfcn , Matrix ()
      callback_property windowbuttonupfcn , Matrix ()
      callback_property windowkeypressfcn , Matrix ()
      callback_property windowkeyreleasefcn , Matrix ()
      callback_property windowscrollwheelfcn , Matrix ()
      radio_property windowstate , "{normal}|minimized|maximized|fullscreen"
      radio_property windowstyle , "{normal}|modal|docked"

      // Overridden base property
      // Property is not implemented for figures.
      // Hide it and set it to a default value that works.
      radio_property pickableparts h , "{visible}"

      // Octave-specific properties
      mutable string_property __gl_extensions__ hr , ""
      mutable string_property __gl_renderer__ hr , ""
      mutable string_property __gl_vendor__ hr , ""
      mutable string_property __gl_version__ hr , ""
      bool_property __gl_window__ h , "off"
      string_property __graphics_toolkit__ hs , default_graphics_toolkit ()
      any_property __guidata__ h , Matrix ()
      radio_property __mouse_mode__ hS , "{none}|pan|rotate|select|text|zoom"
      bool_property __printing__ h , "off"
      any_property __pan_mode__ h , Matrix ()
      any_property __plot_stream__ h , Matrix ()
      any_property __rotate_mode__ h , Matrix ()
      any_property __zoom_mode__ h , Matrix ()
      double_property __device_pixel_ratio__ hU , 1.0

    END_PROPERTIES

  protected:
    void init (void)
    {
      m_alphamap.add_constraint (dim_vector (-1, 1));
      m_alphamap.add_constraint (dim_vector (1, -1));
      m_colormap.add_constraint (dim_vector (-1, 3));
      m_colormap.add_constraint (dim_vector (0, 0));
      m_outerposition.add_constraint (dim_vector (1, 4));
      m_outerposition.add_constraint (FINITE);
      m_paperposition.add_constraint (dim_vector (1, 4));
      m_paperposition.add_constraint (FINITE);
      m_papersize.add_constraint (dim_vector (1, 2));
      m_papersize.add_constraint (FINITE);
      m_pointershapecdata.add_constraint (dim_vector (16, 16));
      m_pointershapecdata.add_constraint (dim_vector (32, 32));
      m_pointershapehotspot.add_constraint (dim_vector (1, 2));
      m_position.add_constraint (dim_vector (1, 4));
      m_position.add_constraint (FINITE);

      init_toolkit ();
    }

  private:
    OCTINTERP_API Matrix get_auto_paperposition (void);

    void update_paperpositionmode (void)
    {
      if (m_paperpositionmode.is ("auto"))
        m_paperposition.set (get_auto_paperposition ());
    }

    OCTINTERP_API void update_handlevisibility (void);

    OCTINTERP_API void init_toolkit (void);

    octave::graphics_toolkit m_toolkit;
  };

private:
  properties m_properties;

public:
  figure (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p), m_default_properties ()
  { }

  ~figure (void) = default;

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (root object) to override first (properties knows how
    // to find the parent object).
    m_properties.override_defaults (obj);

    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (m_default_properties);
  }

  void set (const caseless_str& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      m_default_properties.set (name.substr (7), value);
    else
      m_properties.set (name, value);
  }

  octave_value get (const caseless_str& name) const
  {
    octave_value retval;

    if (name.compare ("default", 7))
      retval = get_default (name.substr (7));
    else
      retval = m_properties.get (name);

    return retval;
  }

  OCTINTERP_API octave_value get_default (const caseless_str& name) const;

  octave_value get_defaults (void) const
  {
    return m_default_properties.as_struct ("default");
  }

  property_list get_defaults_list (void) const
  {
    return m_default_properties;
  }

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  OCTINTERP_API void reset_default_properties (void);

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

private:
  property_list m_default_properties;
};

// ---------------------------------------------------------------------

class OCTINTERP_API graphics_xform
{
public:

  graphics_xform (void)
    : m_xform (xform_eye ()), m_xform_inv (xform_eye ()),
      m_sx ("linear"), m_sy ("linear"), m_sz ("linear"),  m_zlim (1, 2, 0.0)
  {
    m_zlim(1) = 1.0;
  }

  graphics_xform (const Matrix& xm, const Matrix& xim,
                  const scaler& x, const scaler& y, const scaler& z,
                  const Matrix& zl)
    : m_xform (xm), m_xform_inv (xim), m_sx (x), m_sy (y),
      m_sz (z), m_zlim (zl)
  { }

  graphics_xform (const graphics_xform& g)
    : m_xform (g.m_xform), m_xform_inv (g.m_xform_inv), m_sx (g.m_sx),
      m_sy (g.m_sy), m_sz (g.m_sz), m_zlim (g.m_zlim) { }

  ~graphics_xform (void) = default;

  graphics_xform& operator = (const graphics_xform& g)
  {
    m_xform = g.m_xform;
    m_xform_inv = g.m_xform_inv;
    m_sx = g.m_sx;
    m_sy = g.m_sy;
    m_sz = g.m_sz;
    m_zlim = g.m_zlim;

    return *this;
  }

  static OCTINTERP_API ColumnVector xform_vector (double x, double y, double z);

  static OCTINTERP_API Matrix xform_eye (void);

  OCTINTERP_API ColumnVector
  transform (double x, double y, double z, bool use_scale = true) const;

  OCTINTERP_API ColumnVector
  untransform (double x, double y, double z, bool use_scale = true) const;

  ColumnVector untransform (double x, double y, bool use_scale = true) const
  { return untransform (x, y, (m_zlim(0)+m_zlim(1))/2, use_scale); }

  Matrix xscale (const Matrix& m) const { return m_sx.scale (m); }
  Matrix yscale (const Matrix& m) const { return m_sy.scale (m); }
  Matrix zscale (const Matrix& m) const { return m_sz.scale (m); }

  Matrix scale (const Matrix& m) const
  {
    bool has_z = (m.columns () > 2);

    if (m_sx.is_linear () && m_sy.is_linear ()
        && (! has_z || m_sz.is_linear ()))
      return m;

    Matrix retval (m.dims ());

    int r = m.rows ();

    for (int i = 0; i < r; i++)
      {
        retval(i,0) = m_sx.scale (m(i,0));
        retval(i,1) = m_sy.scale (m(i,1));
        if (has_z)
          retval(i,2) = m_sz.scale (m(i,2));
      }

    return retval;
  }

private:
  Matrix m_xform;
  Matrix m_xform_inv;
  scaler m_sx, m_sy, m_sz;
  Matrix m_zlim;
};

enum
{
  AXE_ANY_DIR   = 0,
  AXE_DEPTH_DIR = 1,
  AXE_HORZ_DIR  = 2,
  AXE_VERT_DIR  = 3
};

class OCTINTERP_API axes : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    OCTINTERP_API void
    set_defaults (base_graphics_object& obj, const std::string& mode);

    OCTINTERP_API void
    remove_child (const graphics_handle& h, bool from_root = false);

    OCTINTERP_API void adopt (const graphics_handle& h);

    const scaler& get_x_scaler (void) const { return m_sx; }
    const scaler& get_y_scaler (void) const { return m_sy; }
    const scaler& get_z_scaler (void) const { return m_sz; }

    OCTINTERP_API Matrix
    get_boundingbox (bool internal = false,
                     const Matrix& parent_pix_size = Matrix ()) const;
    OCTINTERP_API Matrix
    get_extent (bool with_text = false,
                bool only_text_height=false) const;

    OCTINTERP_API double
    get___fontsize_points__ (double box_pix_height = 0) const;

    void update_boundingbox (void)
    {
      if (units_is ("normalized"))
        {
          sync_positions ();
          base_properties::update_boundingbox ();
        }
    }

    OCTINTERP_API void update_camera (void);
    OCTINTERP_API void update_axes_layout (void);
    OCTINTERP_API void update_aspectratios (void);
    void update_transform (void)
    {
      update_aspectratios ();
      update_camera ();
      update_axes_layout ();
    }

    OCTINTERP_API void sync_positions (void);

    // Redirect calls to "activepositionproperty" to "positionconstraint".

    std::string get_activepositionproperty (void) const
    {
      std::string cur_val;

      if (m_positionconstraint.is ("innerposition"))
        cur_val = "position";
      else
        cur_val = "outerposition";

      return cur_val;
    }

    void set_activepositionproperty (const octave_value& val)
    {
      // call set method to validate the input
      m_activepositionproperty.set (val);

      if (val.char_matrix_value ().row_as_string (0) == "position")
        set_positionconstraint ("innerposition");
      else
        set_positionconstraint (val);
    }

    // Redirect calls to "innerposition" to "position".

    octave_value get_innerposition (void) const
    {
      return get_position ();
    }

    void set_innerposition (const octave_value& val)
    {
      set_position (val);
    }

    OCTINTERP_API void update_autopos (const std::string& elem_type);
    OCTINTERP_API void update_xlabel_position (void);
    OCTINTERP_API void update_ylabel_position (void);
    OCTINTERP_API void update_zlabel_position (void);
    OCTINTERP_API void update_title_position (void);

    graphics_xform get_transform (void) const
    {
      return graphics_xform (m_x_render, m_x_render_inv,
                             m_sx, m_sy, m_sz, m_x_zlim);
    }

    Matrix get_transform_matrix (void) const { return m_x_render; }
    Matrix get_inverse_transform_matrix (void) const { return m_x_render_inv; }
    Matrix get_opengl_matrix_1 (void) const { return m_x_gl_mat1; }
    Matrix get_opengl_matrix_2 (void) const { return m_x_gl_mat2; }
    Matrix get_transform_zlim (void) const { return m_x_zlim; }

    int get_xstate (void) const { return m_xstate; }
    int get_ystate (void) const { return m_ystate; }
    int get_zstate (void) const { return m_zstate; }
    double get_xPlane (void) const { return m_xPlane; }
    double get_xPlaneN (void) const { return m_xPlaneN; }
    double get_yPlane (void) const { return m_yPlane; }
    double get_yPlaneN (void) const { return m_yPlaneN; }
    double get_zPlane (void) const { return m_zPlane; }
    double get_zPlaneN (void) const { return m_zPlaneN; }
    double get_xpTick (void) const { return m_xpTick; }
    double get_xpTickN (void) const { return m_xpTickN; }
    double get_ypTick (void) const { return m_ypTick; }
    double get_ypTickN (void) const { return m_ypTickN; }
    double get_zpTick (void) const { return m_zpTick; }
    double get_zpTickN (void) const { return m_zpTickN; }
    double get_x_min (void) const { return std::min (m_xPlane, m_xPlaneN); }
    double get_x_max (void) const { return std::max (m_xPlane, m_xPlaneN); }
    double get_y_min (void) const { return std::min (m_yPlane, m_yPlaneN); }
    double get_y_max (void) const { return std::max (m_yPlane, m_yPlaneN); }
    double get_z_min (void) const { return std::min (m_zPlane, m_zPlaneN); }
    double get_z_max (void) const { return std::max (m_zPlane, m_zPlaneN); }
    double get_fx (void) const { return m_fx; }
    double get_fy (void) const { return m_fy; }
    double get_fz (void) const { return m_fz; }
    double get_xticklen (void) const { return m_xticklen; }
    double get_yticklen (void) const { return m_yticklen; }
    double get_zticklen (void) const { return m_zticklen; }
    double get_xtickoffset (void) const { return m_xtickoffset; }
    double get_ytickoffset (void) const { return m_ytickoffset; }
    double get_ztickoffset (void) const { return m_ztickoffset; }
    bool get_x2Dtop (void) const { return m_x2Dtop; }
    bool get_y2Dright (void) const { return m_y2Dright; }
    bool get_layer2Dtop (void) const { return m_layer2Dtop; }
    bool get_is2D (bool include_kids = false) const
    { return (include_kids ? (m_is2D && ! m_has3Dkids) : m_is2D); }
    void set_has3Dkids (bool val) { m_has3Dkids = val; }
    bool get_xySym (void) const { return m_xySym; }
    bool get_xyzSym (void) const { return m_xyzSym; }
    bool get_zSign (void) const { return m_zSign; }
    bool get_nearhoriz (void) const { return m_nearhoriz; }

    ColumnVector pixel2coord (double px, double py) const
    {
      return get_transform ().untransform (px, py,
                                           (m_x_zlim(0)+m_x_zlim(1))/2);
    }

    ColumnVector coord2pixel (double x, double y, double z) const
    { return get_transform ().transform (x, y, z); }

    OCTINTERP_API void
    zoom_about_point (const std::string& mode, double x, double y,
                      double factor, bool push_to_zoom_stack = true);
    OCTINTERP_API void
    zoom (const std::string& mode, double factor,
          bool push_to_zoom_stack = true);
    OCTINTERP_API void
    zoom (const std::string& mode, const Matrix& xl, const Matrix& yl,
          bool push_to_zoom_stack = true);

    OCTINTERP_API void
    translate_view (const std::string& mode,
                    double x0, double x1, double y0, double y1,
                    bool push_to_zoom_stack = true);

    OCTINTERP_API void
    pan (const std::string& mode, double factor,
         bool push_to_zoom_stack = true);

    OCTINTERP_API void
    rotate3d (double x0, double x1, double y0, double y1,
              bool push_to_zoom_stack = true);

    OCTINTERP_API void
    rotate_view (double delta_az, double delta_el,
                 bool push_to_zoom_stack = true);

    OCTINTERP_API void unzoom (void);
    OCTINTERP_API void update_handlevisibility (void);
    OCTINTERP_API void push_zoom_stack (void);
    OCTINTERP_API void clear_zoom_stack (bool do_unzoom = true);

    OCTINTERP_API void update_units (const caseless_str& old_units);

    OCTINTERP_API void update_font (std::string prop = "");

    OCTINTERP_API void update_fontunits (const caseless_str& old_fontunits);

    void increase_num_lights (void) { m_num_lights++; }
    void decrease_num_lights (void) { m_num_lights--; }
    unsigned int get_num_lights (void) const { return m_num_lights; }

  private:

    scaler m_sx = scaler ();
    scaler m_sy = scaler ();
    scaler m_sz = scaler ();

    Matrix m_x_render = Matrix ();
    Matrix m_x_render_inv = Matrix ();
    Matrix m_x_gl_mat1 = Matrix ();
    Matrix m_x_gl_mat2 = Matrix ();
    Matrix m_x_zlim = Matrix ();

    std::list<octave_value> m_zoom_stack = std::list<octave_value> ();

    // Axes layout data
    int m_xstate = 0;
    int m_ystate = 0;
    int m_zstate = 0;

    double m_xPlane = 0.0;
    double m_yPlane = 0.0;
    double m_zPlane = 0.0;

    double m_xPlaneN = 0.0;
    double m_yPlaneN = 0.0;
    double m_zPlaneN = 0.0;

    double m_xpTick = 0.0;
    double m_ypTick = 0.0;
    double m_zpTick = 0.0;

    double m_xpTickN = 0.0;
    double m_ypTickN = 0.0;
    double m_zpTickN = 0.0;

    double m_fx = 0.0;
    double m_fy = 0.0;
    double m_fz = 0.0;

    double m_xticklen = 0.0;
    double m_yticklen = 0.0;
    double m_zticklen = 0.0;

    double m_xtickoffset = 0.0;
    double m_ytickoffset = 0.0;
    double m_ztickoffset = 0.0;

    bool m_x2Dtop = false;
    bool m_y2Dright = false;
    bool m_layer2Dtop = false;
    bool m_is2D = false;
    bool m_has3Dkids = false;
    bool m_xySym = false;
    bool m_xyzSym = false;
    bool m_zSign = false;
    bool m_nearhoriz = false;

    unsigned int m_num_lights = 0;

    // Text renderer, used for calculation of text (tick labels) size
    octave::text_renderer m_txt_renderer;

    OCTINTERP_API void
    set_text_child (handle_property& h, const std::string& who,
                    const octave_value& v);

    OCTINTERP_API void
    delete_text_child (handle_property& h, bool from_root = false);

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (axes)
      radio_property activepositionproperty gsh , "{outerposition}|position"
      row_vector_property alim m , default_lim ()
      radio_property alimmode , "{auto}|manual"
      // FIXME: not yet implemented
      array_property alphamap , Matrix ()
      radio_property alphascale , "{linear}|log"
      color_property ambientlightcolor , color_values (1, 1, 1)
      bool_property box u , "off"
      radio_property boxstyle , "{back}|full"
      row_vector_property cameraposition mu , Matrix (1, 3, 0.0)
      radio_property camerapositionmode u , "{auto}|manual"
      row_vector_property cameratarget mu , Matrix (1, 3, 0.0)
      radio_property cameratargetmode u , "{auto}|manual"
      row_vector_property cameraupvector mu , Matrix (1, 3, 0.0)
      radio_property cameraupvectormode u , "{auto}|manual"
      double_property cameraviewangle mu , 6.6086
      radio_property cameraviewanglemode u , "{auto}|manual"
      row_vector_property clim m , default_lim ()
      radio_property climmode al , "{auto}|manual"
      radio_property clippingstyle , "{3dbox}|rectangle"
      color_property color , color_property (color_values (1, 1, 1), radio_values ("none"))
      array_property colormap sg , Matrix ()
      array_property colororder , default_colororder ()
      double_property colororderindex , 1.0
      radio_property colorscale , "{linear}|log"
      array_property currentpoint , Matrix (2, 3, 0.0)
      row_vector_property dataaspectratio mu , Matrix (1, 3, 1.0)
      radio_property dataaspectratiomode u , "{auto}|manual"
      radio_property fontangle u , "{normal}|italic"
      string_property fontname u , OCTAVE_DEFAULT_FONTNAME
      double_property fontsize mu , 10
      // FIXME: not yet implemented
      radio_property fontsizemode , "{auto}|manual"
      bool_property fontsmoothing u , "on"
      radio_property fontunits SU , "{points}|inches|centimeters|normalized|pixels"
      radio_property fontweight u , "{normal}|bold"
      double_property gridalpha m , 0.15
      radio_property gridalphamode , "{auto}|manual"
      color_property gridcolor m , color_property (color_values (0.15, 0.15, 0.15), radio_values ("none"))
      radio_property gridcolormode , "{auto}|manual"
      radio_property gridlinestyle , "{-}|--|:|-.|none"
      array_property innerposition sg , default_axes_position ()
      // FIXME: Should be an array of "interaction objects".
      // Make it read-only for now.
      any_property interactions r , Matrix ()
      double_property labelfontsizemultiplier u , 1.1
      radio_property layer u , "{bottom}|top"
      // FIXME: Should be a "layoutoptions" object. Make it read-only for now.
      handle_property layout r , graphics_handle ()
      // FIXME: Should be a "legend" object. Make it read-only for now.
      handle_property legend r , graphics_handle ()
      // FIXME: should be kind of string array.
      any_property linestyleorder S , "-"
      double_property linestyleorderindex , 1.0
      double_property linewidth , 0.5
      double_property minorgridalpha m , 0.25
      radio_property minorgridalphamode , "{auto}|manual"
      color_property minorgridcolor m , color_property (color_values (0.1, 0.1, 0.1), radio_values ("none"))
      radio_property minorgridcolormode , "{auto}|manual"
      radio_property minorgridlinestyle , "{:}|-|--|-.|none"
      radio_property nextplot , "{replace}|add|replacechildren"
      double_property nextseriesindex r , 1.0
      array_property outerposition u , default_axes_outerposition ()
      row_vector_property plotboxaspectratio mu , Matrix (1, 3, 1.0)
      radio_property plotboxaspectratiomode u , "{auto}|manual"
      array_property position u , default_axes_position ()
      radio_property positionconstraint , "{outerposition}|innerposition"
      radio_property projection , "{orthographic}|perspective"
      radio_property sortmethod , "{depth}|childorder"
      radio_property tickdir mu , "{in}|out"
      radio_property tickdirmode u , "{auto}|manual"
      // FIXME: Added recently to Matlab, should replace interpreter property.
      radio_property ticklabelinterpreter u , "{tex}|latex|none"
      array_property ticklength u , default_axes_ticklength ()
      array_property tightinset r , Matrix (1, 4, 0.0)
      handle_property title SOf , make_graphics_handle ("text", m___myhandle__, false, false, false)
      double_property titlefontsizemultiplier u , 1.1
      radio_property titlefontweight u , "{bold}|normal"
      // FIXME: Should be a "axestoolbar" object. Make it read-only for now.
      handle_property toolbar r , graphics_handle ()
      radio_property units SU , "{normalized}|inches|centimeters|points|pixels|characters"
      array_property view u , default_axes_view ()
      // FIXME: Should be a "ruler" object. Make it read-only for now.
      handle_property xaxis r , graphics_handle ()
      radio_property xaxislocation u , "{bottom}|top|origin"
      color_property xcolor mu , color_property (color_values (0.15, 0.15, 0.15), radio_values ("none"))
      radio_property xcolormode , "{auto}|manual"
      radio_property xdir u , "{normal}|reverse"
      bool_property xgrid , "off"
      handle_property xlabel SOf , make_graphics_handle ("text", m___myhandle__, false, false, false)
      row_vector_property xlim mu , default_lim ()
      radio_property xlimitmethod u , "{tickaligned}|tight|padded"
      radio_property xlimmode al , "{auto}|manual"
      bool_property xminorgrid , "off"
      bool_property xminortick , "off"
      radio_property xscale alu , "{linear}|log"
      row_vector_property xtick mu , default_axes_tick ()
      // FIXME: should be kind of string array.
      any_property xticklabel S , ""
      radio_property xticklabelmode u , "{auto}|manual"
      double_property xticklabelrotation , 0.0
      radio_property xtickmode u , "{auto}|manual"
      // FIXME: Should be a "ruler" object. Make it read-only for now.
      handle_property yaxis r , graphics_handle ()
      radio_property yaxislocation u , "{left}|right|origin"
      color_property ycolor mu , color_property (color_values (0.15, 0.15, 0.15), radio_values ("none"))
      radio_property ycolormode , "{auto}|manual"
      radio_property ydir u , "{normal}|reverse"
      bool_property ygrid , "off"
      handle_property ylabel SOf , make_graphics_handle ("text", m___myhandle__, false, false, false)
      row_vector_property ylim mu , default_lim ()
      radio_property ylimitmethod u , "{tickaligned}|tight|padded"
      radio_property ylimmode al , "{auto}|manual"
      bool_property yminorgrid , "off"
      bool_property yminortick , "off"
      radio_property yscale alu , "{linear}|log"
      row_vector_property ytick mu , default_axes_tick ()
      any_property yticklabel S , ""
      radio_property yticklabelmode u , "{auto}|manual"
      double_property yticklabelrotation , 0.0
      radio_property ytickmode u , "{auto}|manual"
      // FIXME: Should be a "ruler" object. Make it read-only for now.
      handle_property zaxis r , graphics_handle ()
      color_property zcolor mu , color_property (color_values (0.15, 0.15, 0.15), radio_values ("none"))
      radio_property zcolormode , "{auto}|manual"
      radio_property zdir u , "{normal}|reverse"
      bool_property zgrid , "off"
      handle_property zlabel SOf , make_graphics_handle ("text", m___myhandle__, false, false, false)
      row_vector_property zlim mu , default_lim ()
      radio_property zlimitmethod u , "{tickaligned}|tight|padded"
      radio_property zlimmode al , "{auto}|manual"
      bool_property zminorgrid , "off"
      bool_property zminortick , "off"
      radio_property zscale alu , "{linear}|log"
      row_vector_property ztick mu , default_axes_tick ()
      any_property zticklabel S , ""
      radio_property zticklabelmode u , "{auto}|manual"
      double_property zticklabelrotation , 0.0
      radio_property ztickmode u , "{auto}|manual"

      // Octave-specific properties
      array_property __colormap__ hu , Matrix ()
      double_property mousewheelzoom , 0.5

      // hidden properties for alignment of subplots
      radio_property __autopos_tag__ h , "{none}|subplot"
      // hidden properties for inset
      array_property looseinset hu , Matrix (1, 4, 0.0)
      // hidden properties for minor ticks
      row_vector_property xminortickvalues h , Matrix ()
      row_vector_property yminortickvalues h , Matrix ()
      row_vector_property zminortickvalues h , Matrix ()
      // hidden property for text rendering
      double_property __fontsize_points__ hgr , 0
   END_PROPERTIES

  protected:
    OCTINTERP_API void init (void);

  private:

    std::string
    get_scale (const std::string& scale, const Matrix& lims)
    {
      std::string retval = scale;

      if (scale == "log" && lims.numel () > 1 && lims(0) < 0 && lims(1) < 0)
        retval = "neglog";

      return retval;
    }

    void update_xscale (void)
    {
      m_sx = get_scale (get_xscale (), m_xlim.get ().matrix_value ());
    }

    void update_yscale (void)
    {
      m_sy = get_scale (get_yscale (), m_ylim.get ().matrix_value ());
    }

    void update_zscale (void)
    {
      m_sz = get_scale (get_zscale (), m_zlim.get ().matrix_value ());
    }

    OCTINTERP_API void
    update_label_color (handle_property label, color_property col);
    void update_xcolor (void)
    { update_label_color (m_xlabel, m_xcolor); }

    void update_ycolor (void)
    { update_label_color (m_ylabel, m_ycolor); }

    void update_zcolor (void)
    { update_label_color (m_zlabel, m_zcolor); }

    void update_view (void) { sync_positions (); }

    void update_cameraposition (void) { update_transform (); }
    void update_cameratarget (void) { update_transform (); }
    void update_cameraupvector (void) { update_transform (); }
    void update_cameraviewangle (void) { update_transform (); }

    void update_camerapositionmode (void)
    {
      if (camerapositionmode_is ("auto"))
        update_cameraposition ();
    }
    void update_cameratargetmode (void)
    {
      if (cameratargetmode_is ("auto"))
        update_cameratarget ();
    }
    void update_cameraupvectormode (void)
    {
      if (cameraupvectormode_is ("auto"))
        update_cameraupvector ();
    }
    void update_cameraviewanglemode (void)
    {
      if (cameraviewanglemode_is ("auto"))
        update_cameraviewangle ();
    }

    void update_dataaspectratio (void) { sync_positions (); }
    void update_dataaspectratiomode (void) { sync_positions (); }
    void update_plotboxaspectratio (void) { sync_positions (); }
    void update_plotboxaspectratiomode (void) { sync_positions (); }

    void update_layer (void) { update_axes_layout (); }
    void update_box (void)
    {
      if (m_xticklabelmode.is ("auto"))
        calc_ticklabels (m_xtick, m_xticklabel, m_xscale.is ("log"),
                         xaxislocation_is ("origin"),
                         m_yscale.is ("log") ? 2 :
                           (yaxislocation_is ("origin") ? 0 :
                             (yaxislocation_is ("left") ? -1 : 1)),
                         m_xlim);
      if (m_yticklabelmode.is ("auto"))
        calc_ticklabels (m_ytick, m_yticklabel, m_yscale.is ("log"),
                         yaxislocation_is ("origin"),
                         m_xscale.is ("log") ? 2 :
                           (xaxislocation_is ("origin") ? 0 :
                             (xaxislocation_is ("bottom") ? -1 : 1)),
                         m_ylim);
    }
    void update_yaxislocation (void)
    {
      sync_positions ();
      update_axes_layout ();
      if (m_xticklabelmode.is ("auto"))
        calc_ticklabels (m_xtick, m_xticklabel, m_xscale.is ("log"),
                         xaxislocation_is ("origin"),
                         m_yscale.is ("log") ? 2 :
                           (yaxislocation_is ("origin") ? 0 :
                             (yaxislocation_is ("left") ? -1 : 1)),
                         m_xlim);
      if (m_yticklabelmode.is ("auto"))
        calc_ticklabels (m_ytick, m_yticklabel, m_yscale.is ("log"),
                         yaxislocation_is ("origin"),
                         m_xscale.is ("log") ? 2 :
                           (xaxislocation_is ("origin") ? 0 :
                             (xaxislocation_is ("bottom") ? -1 : 1)),
                         m_ylim);
      update_ylabel_position ();
    }
    void update_xaxislocation (void)
    {
      sync_positions ();
      update_axes_layout ();
      if (m_xticklabelmode.is ("auto"))
        calc_ticklabels (m_xtick, m_xticklabel, m_xscale.is ("log"),
                         xaxislocation_is ("origin"),
                         m_yscale.is ("log") ? 2 :
                           (yaxislocation_is ("origin") ? 0 :
                             (yaxislocation_is ("left") ? -1 : 1)),
                         m_xlim);
      if (m_yticklabelmode.is ("auto"))
        calc_ticklabels (m_ytick, m_yticklabel, m_yscale.is ("log"),
                         yaxislocation_is ("origin"),
                         m_xscale.is ("log") ? 2 :
                           (xaxislocation_is ("origin") ? 0 :
                             (xaxislocation_is ("bottom") ? -1 : 1)),
                         m_ylim);
      update_xlabel_position ();
    }

    void update_xdir (void) { update_camera (); update_axes_layout (); }
    void update_ydir (void) { update_camera (); update_axes_layout (); }
    void update_zdir (void) { update_camera (); update_axes_layout (); }

    void update_ticklength (void);
    void update_tickdir (void) { update_ticklength (); }
    void update_tickdirmode (void) { update_ticklength (); }

    void update_ticklabelinterpreter (void)
    {
      update_xtick (false);
      update_ytick (false);
      update_ztick (true);
    }

    void update_xtick (bool sync_pos = true)
    {
      calc_ticks_and_lims (m_xlim, m_xtick, m_xminortickvalues,
                           m_xlimmode.is ("auto"), m_xtickmode.is ("auto"),
                           m_xscale.is ("log"), m_xlimitmethod.is ("padded"),
                           m_xlimitmethod.is ("tight"));
      if (m_xticklabelmode.is ("auto"))
        calc_ticklabels (m_xtick, m_xticklabel, m_xscale.is ("log"),
                         xaxislocation_is ("origin"),
                         m_yscale.is ("log") ? 2 :
                           (yaxislocation_is ("origin") ? 0 :
                             (yaxislocation_is ("left") ? -1 : 1)),
                         m_xlim);

      if (sync_pos)
        sync_positions ();
    }
    void update_ytick (bool sync_pos = true)
    {
      calc_ticks_and_lims (m_ylim, m_ytick, m_yminortickvalues,
                           m_ylimmode.is ("auto"), m_ytickmode.is ("auto"),
                           m_yscale.is ("log"), m_ylimitmethod.is ("padded"),
                           m_ylimitmethod.is ("tight"));
      if (m_yticklabelmode.is ("auto"))
        calc_ticklabels (m_ytick, m_yticklabel, m_yscale.is ("log"),
                         yaxislocation_is ("origin"),
                         m_xscale.is ("log") ? 2 :
                           (xaxislocation_is ("origin") ? 0 :
                             (xaxislocation_is ("bottom") ? -1 : 1)),
                         m_ylim);

      if (sync_pos)
        sync_positions ();
    }
    void update_ztick (bool sync_pos = true)
    {
      calc_ticks_and_lims (m_zlim, m_ztick, m_zminortickvalues,
                           m_zlimmode.is ("auto"), m_ztickmode.is ("auto"),
                           m_zscale.is ("log"), m_zlimitmethod.is ("padded"),
                           m_zlimitmethod.is ("tight"));
      if (m_zticklabelmode.is ("auto"))
        calc_ticklabels (m_ztick, m_zticklabel, m_zscale.is ("log"), false,
                         2, m_zlim);

      if (sync_pos)
        sync_positions ();
    }

    void update_xtickmode (void)
    {
      if (m_xtickmode.is ("auto"))
        update_xtick ();
    }
    void update_ytickmode (void)
    {
      if (m_ytickmode.is ("auto"))
        update_ytick ();
    }
    void update_ztickmode (void)
    {
      if (m_ztickmode.is ("auto"))
        update_ztick ();
    }

    void update_xticklabelmode (void)
    {
      if (m_xticklabelmode.is ("auto"))
        calc_ticklabels (m_xtick, m_xticklabel, m_xscale.is ("log"),
                         xaxislocation_is ("origin"),
                         m_yscale.is ("log") ? 2 :
                           (yaxislocation_is ("origin") ? 0 :
                             (yaxislocation_is ("left") ? -1 : 1)),
                         m_xlim);
    }
    void update_yticklabelmode (void)
    {
      if (m_yticklabelmode.is ("auto"))
        calc_ticklabels (m_ytick, m_yticklabel, m_yscale.is ("log"),
                         yaxislocation_is ("origin"),
                         m_xscale.is ("log") ? 2 :
                           (xaxislocation_is ("origin") ? 0 :
                             (xaxislocation_is ("bottom") ? -1 : 1)),
                         m_ylim);
    }
    void update_zticklabelmode (void)
    {
      if (m_zticklabelmode.is ("auto"))
        calc_ticklabels (m_ztick, m_zticklabel, m_zscale.is ("log"),
                         false, 2, m_zlim);
    }

    void update_fontname (void)
    {
      update_font ("fontname");
      sync_positions ();
    }
    void update_fontsize (void)
    {
      update_font ("fontsize");
      sync_positions ();
    }
    void update_fontsmoothing (void)
    {
      update_font ("fontsmoothing");
    }
    void update_fontangle (void)
    {
      update_font ("fontangle");
      sync_positions ();
    }
    void update_fontweight (void)
    {
      update_font ("fontweight");
      sync_positions ();
    }

    void update_titlefontsizemultiplier (void)
    {
      // update_font handles title and axis labels
      update_font ("fontsize");
      sync_positions ();
    }

    void update_labelfontsizemultiplier (void)
    {
      update_font ("fontsize");
      sync_positions ();
    }

    void update_titlefontweight (void)
    {
      // update_font handles title and axis labels
      update_font ("fontweight");
      sync_positions ();
    }

    OCTINTERP_API void update_outerposition (void);
    OCTINTERP_API void update_position (void);
    OCTINTERP_API void update_looseinset (void);

    OCTINTERP_API double calc_tick_sep (double minval, double maxval);
    OCTINTERP_API void
    calc_ticks_and_lims (array_property& lims, array_property& ticks,
                         array_property& mticks, bool limmode_is_auto,
                         bool tickmode_is_auto, bool is_logscale,
                         bool method_is_padded, bool method_is_tight);
    OCTINTERP_API void
    calc_ticklabels (const array_property& ticks, any_property& labels,
                     bool is_logscale, const bool is_origin,
                     const int other_axislocation,
                     const array_property& axis_lims);
    OCTINTERP_API Matrix
    get_ticklabel_extents (const Matrix& ticks,
                           const string_vector& ticklabels,
                           const Matrix& limits);

    void fix_limits (array_property& lims)
    {
      if (lims.get ().isempty ())
        return;

      Matrix l = lims.get ().matrix_value ();
      if (l(0) > l(1))
        {
          l(0) = 0;
          l(1) = 1;
          lims = l;
        }
      else if (l(0) == l(1))
        {
          l(0) -= 0.5;
          l(1) += 0.5;
          lims = l;
        }
    }

    OCTINTERP_API Matrix calc_tightbox (const Matrix& init_pos);

    void set_colormap (const octave_value& val)
    {
      set___colormap__ (val);
    }

    void update___colormap__ (void)
    {
      m_colormap.run_listeners (GCB_POSTSET);
    }

    OCTINTERP_API octave_value get_colormap (void) const;

  public:
    OCTINTERP_API Matrix
    get_axis_limits (double xmin, double xmax,
                     double min_pos, double max_neg,
                     const bool logscale, const std::string& method);

    OCTINTERP_API void
    check_axis_limits (Matrix& limits, const Matrix kids,
                       const bool logscale, char& update_type);

    void update_xlim ()
    {
      update_axis_limits ("xlim");

      calc_ticks_and_lims (m_xlim, m_xtick, m_xminortickvalues,
                           m_xlimmode.is ("auto"), m_xtickmode.is ("auto"),
                           m_xscale.is ("log"), m_xlimitmethod.is ("padded"),
                           m_xlimitmethod.is ("tight"));
      if (m_xticklabelmode.is ("auto"))
        calc_ticklabels (m_xtick, m_xticklabel, m_xscale.is ("log"),
                         m_xaxislocation.is ("origin"),
                         m_yscale.is ("log") ? 2 :
                           (yaxislocation_is ("origin") ? 0 :
                             (yaxislocation_is ("left") ? -1 : 1)),
                         m_xlim);

      fix_limits (m_xlim);

      update_xscale ();

      update_axes_layout ();
    }

    void update_xlimitmethod ()
    {
      update_xlim ();
    }

    void update_ylim (void)
    {
      update_axis_limits ("ylim");

      calc_ticks_and_lims (m_ylim, m_ytick, m_yminortickvalues,
                           m_ylimmode.is ("auto"), m_ytickmode.is ("auto"),
                           m_yscale.is ("log"), m_ylimitmethod.is ("padded"),
                           m_ylimitmethod.is ("tight"));
      if (m_yticklabelmode.is ("auto"))
        calc_ticklabels (m_ytick, m_yticklabel, m_yscale.is ("log"),
                         yaxislocation_is ("origin"),
                         m_xscale.is ("log") ? 2 :
                           (xaxislocation_is ("origin") ? 0 :
                             (xaxislocation_is ("bottom") ? -1 : 1)),
                         m_ylim);

      fix_limits (m_ylim);

      update_yscale ();

      update_axes_layout ();
    }

    void update_ylimitmethod ()
    {
      update_ylim ();
    }

    void update_zlim (void)
    {
      update_axis_limits ("zlim");

      calc_ticks_and_lims (m_zlim, m_ztick, m_zminortickvalues,
                           m_zlimmode.is ("auto"), m_ztickmode.is ("auto"),
                           m_zscale.is ("log"), m_zlimitmethod.is ("padded"),
                           m_zlimitmethod.is ("tight"));
      if (m_zticklabelmode.is ("auto"))
        calc_ticklabels (m_ztick, m_zticklabel, m_zscale.is ("log"), false,
                         2, m_zlim);

      fix_limits (m_zlim);

      update_zscale ();

      update_axes_layout ();
    }

    void update_zlimitmethod ()
    {
      update_zlim ();
    }

    void trigger_normals_calc (void);

  };

private:
  properties m_properties;

public:
  axes (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p), m_default_properties ()
  {
    m_properties.update_transform ();
  }

  ~axes (void) = default;

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (figure) to override first (properties knows how
    // to find the parent object).
    m_properties.override_defaults (obj);

    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (m_default_properties);
  }

  void set (const caseless_str& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      m_default_properties.set (name.substr (7), value);
    else
      m_properties.set (name, value);
  }

  void set_defaults (const std::string& mode)
  {
    m_properties.set_defaults (*this, mode);
  }

  octave_value get (const caseless_str& name) const
  {
    octave_value retval;

    // FIXME: finish this.
    if (name.compare ("default", 7))
      retval = get_default (name.substr (7));
    else
      retval = m_properties.get (name);

    return retval;
  }

  OCTINTERP_API octave_value get_default (const caseless_str& name) const;

  octave_value get_defaults (void) const
  {
    return m_default_properties.as_struct ("default");
  }

  property_list get_defaults_list (void) const
  {
    return m_default_properties;
  }

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  OCTINTERP_API void update_axis_limits (const std::string& axis_type);

  OCTINTERP_API void update_axis_limits (const std::string& axis_type,
                                         const graphics_handle& h);

  bool valid_object (void) const { return true; }

  OCTINTERP_API void reset_default_properties (void);

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

protected:
  OCTINTERP_API void initialize (const graphics_object& go);

private:
  property_list m_default_properties;
};

// ---------------------------------------------------------------------

class OCTINTERP_API line : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (line)
      color_property color , color_property (color_values (0, 0, 0), radio_values ("none"))
      string_property displayname , ""
      radio_property linejoin , "{round}|miter|chamfer"
      radio_property linestyle , "{-}|--|:|-.|none"
      double_property linewidth , 0.5
      radio_property marker , "{none}|+|o|*|.|x|||_|s|square|d|diamond|^|v|>|<|p|pentagram|h|hexagram"
      color_property markeredgecolor , color_property (radio_values ("{auto}|none"), color_values (0, 0, 0))
      color_property markerfacecolor , color_property (radio_values ("auto|{none}"), color_values (0, 0, 0))
      double_property markersize , 6
      row_vector_property xdata u , default_data ()
      string_property xdatasource , ""
      row_vector_property ydata u , default_data ()
      string_property ydatasource , ""
      row_vector_property zdata u , Matrix ()
      string_property zdatasource , ""

      // hidden properties for limit computation
      row_vector_property xlim hlr , default_data_lim ()
      row_vector_property ylim hlr , default_data_lim ()
      row_vector_property zlim hlr , Matrix ()
      bool_property xliminclude hl , "on"
      bool_property yliminclude hl , "on"
      bool_property zliminclude hl , "on"
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_linewidth.add_constraint ("min", 0, false);
      m_markersize.add_constraint ("min", 0, false);
    }

  private:
    OCTINTERP_API Matrix compute_xlim (void) const;
    OCTINTERP_API Matrix compute_ylim (void) const;

    void update_xdata (void) { set_xlim (compute_xlim ()); }

    void update_ydata (void) { set_ylim (compute_ylim ()); }

    void update_zdata (void) { set_zlim (m_zdata.get_limits ()); }
  };

private:
  properties m_properties;

public:
  line (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~line (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API text : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    OCTINTERP_API double
    get___fontsize_points__ (double box_pix_height = 0) const;

    OCTINTERP_API void update_text_extent (void);

    OCTINTERP_API void update_font (void);

    void set_position (const octave_value& val)
    {
      octave_value new_val (val);

      if (new_val.numel () == 2)
        {
          dim_vector dv (1, 3);

          new_val = new_val.resize (dv, true);
        }

      if (m_position.set (new_val, false))
        {
          set_positionmode ("manual");
          update_position ();
          m_position.run_listeners (GCB_POSTSET);
          mark_modified ();
        }
      else
        set_positionmode ("manual");
    }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (text)
      color_property backgroundcolor , color_property (radio_values ("{none}"), color_values (1, 1, 1))
      color_property color u , color_property (color_values (0, 0, 0), radio_values ("none"))
      color_property edgecolor , color_property (radio_values ("{none}"), color_values (0, 0, 0))
      bool_property editing , "off"
      array_property extent rG , Matrix (1, 4, 0.0)
      radio_property fontangle u , "{normal}|italic"
      string_property fontname u , OCTAVE_DEFAULT_FONTNAME
      double_property fontsize u , 10
      bool_property fontsmoothing u , "on"
      radio_property fontunits SU , "inches|centimeters|normalized|{points}|pixels"
      radio_property fontweight u , "{normal}|bold"
      radio_property horizontalalignment mu , "{left}|center|right"
      radio_property interpreter u , "{tex}|none|latex"
      radio_property linestyle , "{-}|--|:|-.|none"
      double_property linewidth , 0.5
      double_property margin , 3
      array_property position smu , Matrix (1, 3, 0.0)
      double_property rotation mu , 0
      text_label_property string u , ""
      radio_property units u , "{data}|pixels|normalized|inches|centimeters|points"
      radio_property verticalalignment mu , "top|cap|{middle}|baseline|bottom"

      // hidden properties for limit computation
      row_vector_property xlim hlr , Matrix ()
      row_vector_property ylim hlr , Matrix ()
      row_vector_property zlim hlr , Matrix ()
      bool_property xliminclude hl , "off"
      bool_property yliminclude hl , "off"
      bool_property zliminclude hl , "off"
      // hidden properties for auto-positioning
      radio_property positionmode hu , "{auto}|manual"
      radio_property rotationmode hu , "{auto}|manual"
      radio_property horizontalalignmentmode hu , "{auto}|manual"
      radio_property verticalalignmentmode hu , "{auto}|manual"
      radio_property __autopos_tag__ h , "{none}|xlabel|ylabel|zlabel|title"
      // hidden property for text rendering
      double_property __fontsize_points__ hgr , 0
    END_PROPERTIES

    OCTINTERP_API Matrix get_data_position (void) const;
    OCTINTERP_API Matrix get_extent_matrix (bool rotated = false) const;
    const uint8NDArray& get_pixels (void) const { return m_pixels; }

    // Text renderer, used for calculation of text size
    octave::text_renderer m_txt_renderer;

  protected:
    void init (void)
    {
      m_position.add_constraint (dim_vector (1, 3));
      m_fontsize.add_constraint ("min", 0.0, false);
      m_linewidth.add_constraint ("min", 0.0, false);
      m_margin.add_constraint ("min", 0.0, false);
      m_cached_units = get_units ();
      update_font ();
    }

  private:
    void update_position (void)
    {
      Matrix pos = get_data_position ();
      Matrix lim;

      lim = Matrix (1, 4, pos(0));
      lim(2) = (lim(2) <= 0 ? octave::numeric_limits<double>::Inf () : lim(2));
      lim(3) = (lim(3) >= 0 ? -octave::numeric_limits<double>::Inf () : lim(3));
      set_xlim (lim);

      lim = Matrix (1, 4, pos(1));
      lim(2) = (lim(2) <= 0 ? octave::numeric_limits<double>::Inf () : lim(2));
      lim(3) = (lim(3) >= 0 ? -octave::numeric_limits<double>::Inf () : lim(3));
      set_ylim (lim);

      if (pos.numel () == 3)
        {
          lim = Matrix (1, 4, pos(2));
          lim(2) = (lim(2) <= 0 ? octave::numeric_limits<double>::Inf ()
                                : lim(2));
          lim(3) = (lim(3) >= 0 ? -octave::numeric_limits<double>::Inf ()
                                : lim(3));
          set_zlim (lim);
        }
    }

    OCTINTERP_API void request_autopos (void);
    void update_positionmode (void) { request_autopos (); }
    void update_rotationmode (void) { request_autopos (); }
    void update_horizontalalignmentmode (void) { request_autopos (); }
    void update_verticalalignmentmode (void) { request_autopos (); }

    void update_string (void) { request_autopos (); update_text_extent (); }
    void update_rotation (void) { update_text_extent (); }
    void update_fontname (void) { update_font (); update_text_extent (); }
    void update_fontsize (void) { update_font (); update_text_extent (); }
    void update_fontsmoothing (void) { update_font (); update_text_extent (); }

    void update_color (void)
    {
      if (! m_color.is ("none"))
        {
          update_font ();
          update_text_extent ();
        }
    }

    void update_fontangle (void)
    {
      update_font ();
      update_text_extent ();
    }
    void update_fontweight (void) { update_font (); update_text_extent (); }

    void update_interpreter (void) { update_text_extent (); }
    void update_horizontalalignment (void) { update_text_extent (); }
    void update_verticalalignment (void) { update_text_extent (); }

    OCTINTERP_API void update_units (void);
    OCTINTERP_API void update_fontunits (const caseless_str& old_fontunits);

  private:
    std::string m_cached_units;
    uint8NDArray m_pixels;
  };

private:
  properties m_properties;

public:
  text (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  {
    m_properties.set_clipping ("off");
  }

  ~text (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API image : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    bool is_aliminclude (void) const
    { return (m_aliminclude.is_on () && m_alphadatamapping.is ("scaled")); }
    std::string get_aliminclude (void) const
    { return m_aliminclude.current_value (); }

    bool is_climinclude (void) const
    { return (m_climinclude.is_on () && m_cdatamapping.is ("scaled")); }
    std::string get_climinclude (void) const
    { return m_climinclude.current_value (); }

    OCTINTERP_API octave_value get_color_data (void) const;

    void initialize_data (void) { update_cdata (); }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (image)
      array_property alphadata u , Matrix (1, 1, 1.0)
      radio_property alphadatamapping al , "{none}|direct|scaled"
      array_property cdata u , default_image_cdata ()
      radio_property cdatamapping al , "scaled|{direct}"
      row_vector_property xdata mu , Matrix ()
      row_vector_property ydata mu , Matrix ()
      // hidden properties for limit computation
      row_vector_property alim hlr , Matrix ()
      row_vector_property clim hlr , Matrix ()
      row_vector_property xlim hlr , Matrix ()
      row_vector_property ylim hlr , Matrix ()
      bool_property aliminclude hlg , "on"
      bool_property climinclude hlg , "on"
      bool_property xliminclude hl , "on"
      bool_property yliminclude hl , "on"
      radio_property xdatamode ha , "{auto}|manual"
      radio_property ydatamode ha , "{auto}|manual"
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_xdata.add_constraint (2);
      m_xdata.add_constraint (dim_vector (0, 0));
      m_ydata.add_constraint (2);
      m_ydata.add_constraint (dim_vector (0, 0));
      m_cdata.add_constraint ("double");
      m_cdata.add_constraint ("single");
      m_cdata.add_constraint ("logical");
      m_cdata.add_constraint ("int8");
      m_cdata.add_constraint ("int16");
      m_cdata.add_constraint ("int32");
      m_cdata.add_constraint ("int64");
      m_cdata.add_constraint ("uint8");
      m_cdata.add_constraint ("uint16");
      m_cdata.add_constraint ("uint32");
      m_cdata.add_constraint ("uint64");
      m_cdata.add_constraint ("real");
      m_cdata.add_constraint (dim_vector (-1, -1));
      m_cdata.add_constraint (dim_vector (-1, -1, 3));
      m_alphadata.add_constraint ("double");
      m_alphadata.add_constraint ("uint8");
      m_alphadata.add_constraint (dim_vector (-1, -1));
    }

  private:
    void update_alphadata (void)
    {
      if (alphadatamapping_is ("scaled"))
        set_alim (m_alphadata.get_limits ());
      else
        m_alim = m_alphadata.get_limits ();
    }

    void update_cdata (void)
    {
      if (cdatamapping_is ("scaled"))
        set_clim (m_cdata.get_limits ());
      else
        m_clim = m_cdata.get_limits ();

      if (m_xdatamode.is ("auto"))
        update_xdata ();

      if (m_ydatamode.is ("auto"))
        update_ydata ();
    }

    void update_xdata (void)
    {
      if (m_xdata.get ().isempty ())
        set_xdatamode ("auto");

      if (m_xdatamode.is ("auto"))
        {
          set_xdata (get_auto_xdata ());
          set_xdatamode ("auto");
        }

      Matrix limits = m_xdata.get_limits ();
      float dp = pixel_xsize ();

      limits(0) = limits(0) - dp;
      limits(1) = limits(1) + dp;
      set_xlim (limits);
    }

    void update_ydata (void)
    {
      if (m_ydata.get ().isempty ())
        set_ydatamode ("auto");

      if (m_ydatamode.is ("auto"))
        {
          set_ydata (get_auto_ydata ());
          set_ydatamode ("auto");
        }

      Matrix limits = m_ydata.get_limits ();
      float dp = pixel_ysize ();

      limits(0) = limits(0) - dp;
      limits(1) = limits(1) + dp;
      set_ylim (limits);
    }

    Matrix get_auto_xdata (void)
    {
      dim_vector dv = get_cdata ().dims ();
      Matrix data;
      if (dv(1) > 0.)
        {
          data = Matrix (1, 2, 1);
          data(1) = dv(1);
        }
      return data;
    }

    Matrix get_auto_ydata (void)
    {
      dim_vector dv = get_cdata ().dims ();
      Matrix data;
      if (dv(0) > 0.)
        {
          data = Matrix (1, 2, 1);
          data(1) = dv(0);
        }
      return data;
    }

    float pixel_size (octave_idx_type dim, const Matrix limits)
    {
      octave_idx_type l = dim - 1;
      float dp;

      if (l > 0 && limits(0) != limits(1))
        dp = (limits(1) - limits(0))/(2*l);
      else
        {
          if (limits(1) == limits(2))
            dp = 0.5;
          else
            dp = (limits(1) - limits(0))/2;
        }
      return dp;
    }

  public:
    float pixel_xsize (void)
    {
      return pixel_size ((get_cdata ().dims ())(1), m_xdata.get_limits ());
    }

    float pixel_ysize (void)
    {
      return pixel_size ((get_cdata ().dims ())(0), m_ydata.get_limits ());
    }
  };

private:
  properties m_properties;

public:
  image (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  {
    m_properties.initialize_data ();
  }

  ~image (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API light : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (light)
      color_property color , color_values (1, 1, 1)
      array_property position , default_light_position ()
      radio_property style , "{infinite}|local"
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_position.add_constraint (dim_vector (1, 3));
    }

  private:
    OCTINTERP_API void update_visible (void);
  };

private:
  properties m_properties;

public:
  light (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~light (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

protected:
  OCTINTERP_API void initialize (const graphics_object& go);
};

// ---------------------------------------------------------------------

class OCTINTERP_API patch : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    octave_value get_color_data (void) const;

    // Matlab allows incoherent data to be stored into patch properties.
    // The patch should then be ignored by the renderer.
    bool has_bad_data (std::string& msg) const
    {
      msg = m_bad_data_msg;
      return ! msg.empty ();
    }

    bool is_aliminclude (void) const
    { return (m_aliminclude.is_on () && m_alphadatamapping.is ("scaled")); }
    std::string get_aliminclude (void) const
    { return m_aliminclude.current_value (); }

    bool is_climinclude (void) const
    { return (m_climinclude.is_on () && m_cdatamapping.is ("scaled")); }
    std::string get_climinclude (void) const
    { return m_climinclude.current_value (); }

    OCTINTERP_API bool get_do_lighting (void) const;

    std::vector<std::vector<octave_idx_type>> m_coplanar_last_idx;

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (patch)
      radio_property alphadatamapping l , "none|{scaled}|direct"
      double_property ambientstrength , 0.3
      radio_property backfacelighting , "unlit|lit|{reverselit}"
      array_property cdata u , Matrix ()
      radio_property cdatamapping l , "{scaled}|direct"
      double_property diffusestrength , 0.6
      string_property displayname , ""
      double_radio_property edgealpha , double_radio_property (1.0, radio_values ("flat|interp"))
      color_property edgecolor , color_property (color_values (0, 0, 0), radio_values ("none|flat|interp"))
      radio_property edgelighting u , "{none}|flat|gouraud|phong"
      double_radio_property facealpha , double_radio_property (1.0, radio_values ("flat|interp"))
      color_property facecolor , color_property (color_values (0, 0, 0), radio_values ("none|flat|interp"))
      radio_property facelighting u , "none|{flat}|gouraud|phong"
      array_property facenormals m , Matrix ()
      radio_property facenormalsmode u , "{auto}|manual"
      array_property faces u , default_patch_faces ()
      array_property facevertexalphadata , Matrix ()
      array_property facevertexcdata u , Matrix ()
      radio_property linestyle , "{-}|--|:|-.|none"
      double_property linewidth , 0.5
      radio_property marker , "{none}|+|o|*|.|x|||_|s|square|d|diamond|^|v|>|<|p|pentagram|h|hexagram"
      color_property markeredgecolor , color_property (radio_values ("none|{auto}|flat"), color_values (0, 0, 0))
      color_property markerfacecolor , color_property (radio_values ("{none}|auto|flat"), color_values (0, 0, 0))
      double_property markersize , 6
      double_property specularcolorreflectance , 1.0
      double_property specularexponent , 10.0
      double_property specularstrength , 0.9
      array_property vertexnormals m , Matrix ()
      radio_property vertexnormalsmode u , "{auto}|manual"
      array_property vertices u , default_patch_vertices ()
      array_property xdata u , default_patch_xdata ()
      array_property ydata u , default_patch_ydata ()
      array_property zdata u , Matrix ()

      // hidden properties for limit computation
      row_vector_property alim hlr , Matrix ()
      row_vector_property clim hlr , Matrix ()
      row_vector_property xlim hlr , Matrix ()
      row_vector_property ylim hlr , Matrix ()
      row_vector_property zlim hlr , Matrix ()
      bool_property aliminclude hlg , "on"
      bool_property climinclude hlg , "on"
      bool_property xliminclude hl , "on"
      bool_property yliminclude hl , "on"
      bool_property zliminclude hl , "on"
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_xdata.add_constraint (dim_vector (-1, -1));
      m_ydata.add_constraint (dim_vector (-1, -1));
      m_zdata.add_constraint (dim_vector (-1, -1));
      m_faces.add_constraint (dim_vector (-1, -1));
      m_vertices.add_constraint (dim_vector (-1, 2));
      m_vertices.add_constraint (dim_vector (-1, 3));
      m_cdata.add_constraint ("double");
      m_cdata.add_constraint ("single");
      m_cdata.add_constraint ("logical");
      m_cdata.add_constraint ("int8");
      m_cdata.add_constraint ("int16");
      m_cdata.add_constraint ("int32");
      m_cdata.add_constraint ("int64");
      m_cdata.add_constraint ("uint8");
      m_cdata.add_constraint ("uint16");
      m_cdata.add_constraint ("uint32");
      m_cdata.add_constraint ("uint64");
      m_cdata.add_constraint ("real");
      m_cdata.add_constraint (dim_vector (-1, -1));
      m_cdata.add_constraint (dim_vector (-1, -1, 3));
      m_facevertexcdata.add_constraint (dim_vector (-1, 1));
      m_facevertexcdata.add_constraint (dim_vector (-1, 3));
      m_facevertexcdata.add_constraint (dim_vector (0, 0));
      m_facevertexalphadata.add_constraint (dim_vector (-1, 1));
      m_facevertexalphadata.add_constraint (dim_vector (0, 0));
      m_facenormals.add_constraint (dim_vector (-1, 3));
      m_facenormals.add_constraint (dim_vector (0, 0));
      m_vertexnormals.add_constraint (dim_vector (-1, 3));
      m_vertexnormals.add_constraint (dim_vector (0, 0));

      m_ambientstrength.add_constraint ("min", 0.0, true);
      m_ambientstrength.add_constraint ("max", 1.0, true);
      m_diffusestrength.add_constraint ("min", 0.0, true);
      m_diffusestrength.add_constraint ("max", 1.0, true);
      m_linewidth.add_constraint ("min", 0.0, false);
      m_markersize.add_constraint ("min", 0.0, false);
      m_specularcolorreflectance.add_constraint ("min", 0.0, true);
      m_specularcolorreflectance.add_constraint ("max", 1.0, true);
      m_specularexponent.add_constraint ("min", 0.0, false);
      m_specularstrength.add_constraint ("min", 0.0, true);
      m_specularstrength.add_constraint ("max", 1.0, true);
    }

  public:
    void update_normals (bool reset, bool force = false)
    {
      update_face_normals (reset, force);
      update_vertex_normals (reset, force);
    }


  private:
    std::string m_bad_data_msg;

    void update_faces (void) { update_data ();}

    void update_vertices (void) { update_data ();}

    void update_facevertexcdata (void) { update_data ();}

    OCTINTERP_API void update_fvc (void);

    void update_xdata (void)
    {
      if (get_xdata ().isempty ())
        {
          // For compatibility with matlab behavior,
          // if x/ydata are set empty, silently empty other *data and
          // faces properties while vertices remain unchanged.
          set_ydata (Matrix ());
          set_zdata (Matrix ());
          set_cdata (Matrix ());
          set_faces (Matrix ());
        }
      else
        {
          update_fvc ();
          update_normals (true);
        }

      set_xlim (m_xdata.get_limits ());
    }

    void update_ydata (void)
    {
      if (get_ydata ().isempty ())
        {
          set_xdata (Matrix ());
          set_zdata (Matrix ());
          set_cdata (Matrix ());
          set_faces (Matrix ());
        }
      else
        {
          update_fvc ();
          update_normals (true);
        }

      set_ylim (m_ydata.get_limits ());
    }

    void update_zdata (void)
    {
      update_fvc ();
      update_normals (true);
      set_zlim (m_zdata.get_limits ());
    }

    void update_cdata (void)
    {
      update_fvc ();
      update_normals (false);

      if (cdatamapping_is ("scaled"))
        set_clim (m_cdata.get_limits ());
      else
        m_clim = m_cdata.get_limits ();
    }

    OCTINTERP_API void update_data (void);

    OCTINTERP_API void calc_face_normals (Matrix& normals);
    OCTINTERP_API void update_face_normals (bool reset, bool force = false);
    OCTINTERP_API void update_vertex_normals (bool reset, bool force = false);

    void update_edgelighting (void)
    {
      update_normals (false);
    }

    void update_facelighting (void)
    {
      update_normals (false);
    }

    void update_facenormalsmode (void)
    {
      update_face_normals (false);
    }

    void update_vertexnormalsmode (void)
    {
      update_vertex_normals (false);
    }

    void update_visible (void)
    {
      if (is_visible ())
        update_normals (false);
    }
  };

private:
  properties m_properties;
  property_list m_default_properties;

public:
  patch (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~patch (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

  OCTINTERP_API void reset_default_properties (void);

protected:
  OCTINTERP_API void initialize (const graphics_object& go);

};

// ---------------------------------------------------------------------

class OCTINTERP_API scatter : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    OCTINTERP_API octave_value get_color_data (void) const;

    // Matlab allows incoherent data to be stored in scatter properties.
    // The scatter object should then be ignored by the renderer.
    bool has_bad_data (std::string& msg) const
    {
      msg = m_bad_data_msg;
      return ! msg.empty ();
    }

    bool is_aliminclude (void) const
    { return m_aliminclude.is_on (); }
    std::string get_aliminclude (void) const
    { return m_aliminclude.current_value (); }

    bool is_climinclude (void) const
    { return m_climinclude.is_on (); }
    std::string get_climinclude (void) const
    { return m_climinclude.current_value (); }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (scatter)
      array_property annotation , Matrix ()
      array_property cdata mu , Matrix ()
      radio_property cdatamode u , "{auto}|manual"
      string_property cdatasource , ""
      array_property datatiptemplate , Matrix ()
      string_property displayname , ""
      array_property latitudedata , Matrix ()
      string_property latitudedatasource , ""
      double_property linewidth , 0.5
      array_property longitudedata , Matrix ()
      string_property longitudedatasource , ""
      radio_property marker , "{o}|+|*|.|x|||_|s|square|d|diamond|^|v|>|<|p|pentagram|h|hexagram|none"
      double_property markeredgealpha , 1.0
      color_property markeredgecolor , color_property (radio_values ("{flat}|none"), color_values (0, 0, 0))
      double_property markerfacealpha , 1.0
      color_property markerfacecolor , color_property (radio_values ("{none}|auto|flat"), color_values (0, 0, 0))
      array_property rdata , Matrix ()
      string_property rdatasource , ""
      array_property seriesindex u , Matrix ()
      array_property sizedata u , Matrix ()
      string_property sizedatasource , ""
      array_property thetadata , Matrix ()
      string_property thetadatasource , ""
      array_property xdata u , Matrix ()
      string_property xdatasource , ""
      array_property ydata u , Matrix ()
      string_property ydatasource , ""
      array_property zdata u , Matrix ()
      string_property zdatasource , ""

      // hidden properties for limit computation
      row_vector_property alim hlr , Matrix ()
      row_vector_property clim hlr , Matrix ()
      row_vector_property xlim hlr , Matrix ()
      row_vector_property ylim hlr , Matrix ()
      row_vector_property zlim hlr , Matrix ()
      bool_property aliminclude hlg , "on"
      bool_property climinclude hlg , "on"
      bool_property xliminclude hl , "on"
      bool_property yliminclude hl , "on"
      bool_property zliminclude hl , "on"
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_xdata.add_constraint (dim_vector (-1, 1));
      m_xdata.add_constraint (dim_vector (1, -1));
      m_xdata.add_constraint (dim_vector (-1, 0));
      m_xdata.add_constraint (dim_vector (0, -1));
      m_ydata.add_constraint (dim_vector (-1, 1));
      m_ydata.add_constraint (dim_vector (1, -1));
      m_ydata.add_constraint (dim_vector (-1, 0));
      m_ydata.add_constraint (dim_vector (0, -1));
      m_zdata.add_constraint (dim_vector (-1, 1));
      m_zdata.add_constraint (dim_vector (1, -1));
      m_zdata.add_constraint (dim_vector (-1, 0));
      m_zdata.add_constraint (dim_vector (0, -1));
      m_sizedata.add_constraint ("min", 0.0, false);
      m_sizedata.add_constraint (dim_vector (-1, 1));
      m_sizedata.add_constraint (dim_vector (1, -1));
      m_sizedata.add_constraint (dim_vector (-1, 0));
      m_sizedata.add_constraint (dim_vector (0, -1));
      m_cdata.add_constraint ("double");
      m_cdata.add_constraint ("single");
      m_cdata.add_constraint ("logical");
      m_cdata.add_constraint ("int8");
      m_cdata.add_constraint ("int16");
      m_cdata.add_constraint ("int32");
      m_cdata.add_constraint ("int64");
      m_cdata.add_constraint ("uint8");
      m_cdata.add_constraint ("uint16");
      m_cdata.add_constraint ("uint32");
      m_cdata.add_constraint ("uint64");
      m_cdata.add_constraint ("real");
      m_cdata.add_constraint (dim_vector (-1, 1));
      m_cdata.add_constraint (dim_vector (-1, 3));
      m_cdata.add_constraint (dim_vector (-1, 0));
      m_cdata.add_constraint (dim_vector (0, -1));

      m_linewidth.add_constraint ("min", 0.0, false);
      m_seriesindex.add_constraint (dim_vector (1, 1));
      m_seriesindex.add_constraint (dim_vector (-1, 0));
      m_seriesindex.add_constraint (dim_vector (0, -1));
    }

  public:
    OCTINTERP_API void update_color (void);

  private:
    std::string m_bad_data_msg;

    void update_xdata (void)
    {
      if (get_xdata ().isempty ())
        {
          // For compatibility with Matlab behavior,
          // if x/ydata are set empty, silently empty other *data properties.
          set_ydata (Matrix ());
          set_zdata (Matrix ());
          bool cdatamode_auto = m_cdatamode.is ("auto");
          set_cdata (Matrix ());
          if (cdatamode_auto)
            set_cdatamode ("auto");
        }

      set_xlim (m_xdata.get_limits ());

      update_data ();
    }

    void update_ydata (void)
    {
      if (get_ydata ().isempty ())
        {
          set_xdata (Matrix ());
          set_zdata (Matrix ());
          bool cdatamode_auto = m_cdatamode.is ("auto");
          set_cdata (Matrix ());
          if (cdatamode_auto)
            set_cdatamode ("auto");
        }

      set_ylim (m_ydata.get_limits ());

      update_data ();
    }

    void update_zdata (void)
    {
      set_zlim (m_zdata.get_limits ());

      update_data ();
    }

    void update_sizedata (void)
    {
      update_data ();
    }

    void update_cdata (void)
    {
      if (get_cdata ().matrix_value ().rows () == 1)
        set_clim (m_cdata.get_limits ());
      else
        m_clim = m_cdata.get_limits ();

      update_data ();
    }

    void update_cdatamode (void)
    {
      if (m_cdatamode.is ("auto"))
        update_color ();
    }

    void update_seriesindex (void)
    {
      if (m_cdatamode.is ("auto"))
        update_color ();
    }

    void update_data (void);

  };

private:
  properties m_properties;
  property_list m_default_properties;

public:
  scatter (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  {
    // FIXME: seriesindex should increment by one each time a new scatter
    // object is added to the axes.
  }

  ~scatter (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

protected:
  OCTINTERP_API void initialize (const graphics_object& go);

};

// ---------------------------------------------------------------------

class OCTINTERP_API surface : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    octave_value get_color_data (void) const;

    bool is_aliminclude (void) const
    { return (m_aliminclude.is_on () && m_alphadatamapping.is ("scaled")); }
    std::string get_aliminclude (void) const
    { return m_aliminclude.current_value (); }

    bool is_climinclude (void) const
    { return (m_climinclude.is_on () && m_cdatamapping.is ("scaled")); }
    std::string get_climinclude (void) const
    { return m_climinclude.current_value (); }

    OCTINTERP_API bool get_do_lighting (void) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (surface)
      array_property alphadata u , Matrix (1, 1, 1.0)
      radio_property alphadatamapping l , "none|direct|{scaled}"
      double_property ambientstrength , 0.3
      radio_property backfacelighting , "unlit|lit|{reverselit}"
      array_property cdata u , default_surface_cdata ()
      radio_property cdatamapping al , "{scaled}|direct"
      string_property cdatasource , ""
      double_property diffusestrength , 0.6
      string_property displayname , ""
      double_radio_property edgealpha , double_radio_property (1.0, radio_values ("flat|interp"))
      color_property edgecolor , color_property (color_values (0, 0, 0), radio_values ("none|flat|interp"))
      radio_property edgelighting u , "{none}|flat|gouraud|phong"
      double_radio_property facealpha , double_radio_property (1.0, radio_values ("flat|interp|texturemap"))
      color_property facecolor , color_property (radio_values ("none|{flat}|interp|texturemap"), color_values (0, 0, 0))
      radio_property facelighting u , "none|{flat}|gouraud|phong"
      array_property facenormals m , Matrix ()
      radio_property facenormalsmode u , "{auto}|manual"
      radio_property linestyle , "{-}|--|:|-.|none"
      double_property linewidth , 0.5
      radio_property marker , "{none}|+|o|*|.|x|||_|s|square|d|diamond|^|v|>|<|p|pentagram|h|hexagram"
      color_property markeredgecolor , color_property (radio_values ("none|{auto}|flat"), color_values (0, 0, 0))
      color_property markerfacecolor , color_property (radio_values ("{none}|auto|flat"), color_values (0, 0, 0))
      double_property markersize , 6
      radio_property meshstyle , "{both}|row|column"
      double_property specularcolorreflectance , 1
      double_property specularexponent , 10
      double_property specularstrength , 0.9
      array_property vertexnormals m , Matrix ()
      radio_property vertexnormalsmode u , "{auto}|manual"
      array_property xdata u , default_surface_xdata ()
      string_property xdatasource , ""
      array_property ydata u , default_surface_ydata ()
      string_property ydatasource , ""
      array_property zdata u , default_surface_zdata ()
      string_property zdatasource , ""

      // hidden properties for limit computation
      row_vector_property alim hlr , Matrix ()
      row_vector_property clim hlr , Matrix ()
      row_vector_property xlim hlr , Matrix ()
      row_vector_property ylim hlr , Matrix ()
      row_vector_property zlim hlr , Matrix ()
      bool_property aliminclude hlg , "on"
      bool_property climinclude hlg , "on"
      bool_property xliminclude hl , "on"
      bool_property yliminclude hl , "on"
      bool_property zliminclude hl , "on"
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_xdata.add_constraint (dim_vector (-1, -1));
      m_ydata.add_constraint (dim_vector (-1, -1));
      m_zdata.add_constraint (dim_vector (-1, -1));
      m_cdata.add_constraint ("double");
      m_cdata.add_constraint ("single");
      m_cdata.add_constraint ("logical");
      m_cdata.add_constraint ("int8");
      m_cdata.add_constraint ("int16");
      m_cdata.add_constraint ("int32");
      m_cdata.add_constraint ("int64");
      m_cdata.add_constraint ("uint8");
      m_cdata.add_constraint ("uint16");
      m_cdata.add_constraint ("uint32");
      m_cdata.add_constraint ("uint64");
      m_cdata.add_constraint ("real");
      m_cdata.add_constraint (dim_vector (-1, -1));
      m_cdata.add_constraint (dim_vector (-1, -1, 3));
      m_alphadata.add_constraint ("double");
      m_alphadata.add_constraint ("uint8");
      m_alphadata.add_constraint (dim_vector (-1, -1));
      m_facenormals.add_constraint (dim_vector (-1, -1, 3));
      m_facenormals.add_constraint (dim_vector (0, 0));
      m_vertexnormals.add_constraint (dim_vector (-1, -1, 3));
      m_vertexnormals.add_constraint (dim_vector (0, 0));

      m_ambientstrength.add_constraint ("min", 0.0, true);
      m_ambientstrength.add_constraint ("max", 1.0, true);
      m_diffusestrength.add_constraint ("min", 0.0, true);
      m_diffusestrength.add_constraint ("max", 1.0, true);
      m_linewidth.add_constraint ("min", 0.0, false);
      m_markersize.add_constraint ("min", 0.0, false);
      m_specularcolorreflectance.add_constraint ("min", 0.0, true);
      m_specularcolorreflectance.add_constraint ("max", 1.0, true);
      m_specularexponent.add_constraint ("min", 0.0, false);
      m_specularstrength.add_constraint ("min", 0.0, true);
      m_specularstrength.add_constraint ("max", 1.0, true);
    }

  public:
    void update_normals (bool reset, bool force = false)
    {
      update_face_normals (reset, force);
      update_vertex_normals (reset, force);
    }


  private:
    void update_alphadata (void)
    {
      if (alphadatamapping_is ("scaled"))
        set_alim (m_alphadata.get_limits ());
      else
        m_alim = m_alphadata.get_limits ();
    }

    void update_cdata (void)
    {
      if (cdatamapping_is ("scaled"))
        set_clim (m_cdata.get_limits ());
      else
        m_clim = m_cdata.get_limits ();
    }

    void update_xdata (void)
    {
      update_normals (true);
      set_xlim (m_xdata.get_limits ());
    }

    void update_ydata (void)
    {
      update_normals (true);
      set_ylim (m_ydata.get_limits ());
    }

    void update_zdata (void)
    {
      update_normals (true);
      set_zlim (m_zdata.get_limits ());
    }

    OCTINTERP_API void update_face_normals (bool reset, bool force = false);
    OCTINTERP_API void update_vertex_normals (bool reset, bool force = false);

    void update_facenormalsmode (void)
    { update_face_normals (false); }

    void update_vertexnormalsmode (void)
    { update_vertex_normals (false); }

    void update_edgelighting (void)
    { update_normals (false); }

    void update_facelighting (void)
    { update_normals (false); }

    void update_visible (void)
    {
      if (is_visible ())
        update_normals (false);
    }

  };

private:
  properties m_properties;

public:
  surface (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~surface (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API hggroup : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    OCTINTERP_API void
    remove_child (const graphics_handle& h, bool from_root = false);

    OCTINTERP_API void adopt (const graphics_handle& h);

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (hggroup)
      string_property displayname , ""

      // hidden properties for limit computation
      row_vector_property alim hr , Matrix ()
      row_vector_property clim hr , Matrix ()
      row_vector_property xlim hr , Matrix ()
      row_vector_property ylim hr , Matrix ()
      row_vector_property zlim hr , Matrix ()
      bool_property aliminclude h , "on"
      bool_property climinclude h , "on"
      bool_property xliminclude h , "on"
      bool_property yliminclude h , "on"
      bool_property zliminclude h , "on"
    END_PROPERTIES

  private:
    OCTINTERP_API void update_limits (void) const;

    OCTINTERP_API void update_limits (const graphics_handle& h) const;

  protected:
    void init (void)
    { }

  };

private:
  properties m_properties;

public:
  hggroup (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~hggroup (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  OCTINTERP_API void update_axis_limits (const std::string& axis_type);

  OCTINTERP_API void update_axis_limits (const std::string& axis_type,
                                         const graphics_handle& h);

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

class OCTINTERP_API uimenu : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    void remove_child (const graphics_handle& h, bool from_root = false)
    {
      base_properties::remove_child (h, from_root);
    }

    void adopt (const graphics_handle& h)
    {
      base_properties::adopt (h);
    }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uimenu)
      string_property accelerator , ""
      // Deprecated in R2017b (replaced by "MenuSelectedFcn")
      callback_property callback hgs , Matrix ()
      bool_property checked , "off"
      bool_property enable , "on"
      color_property foregroundcolor , color_values (0, 0, 0)
      // Deprecated in R2017b (replaced by "Text")
      string_property label hgs , ""
      callback_property menuselectedfcn , Matrix ()
      // Deprecated in R2017b, but replacement of re-ordering "children"
      // property of parent does not work yet in Octave.
      double_property position , 0
      bool_property separator , "off"
      string_property text , ""

      // Octave-specific properties
      string_property __fltk_label__ h , ""
      any_property __object__ h , Matrix ()
    END_PROPERTIES

    // Make "Label" an alias for "Text".
    std::string get_label (void) const
    {
      return get_text ();
    }

    void set_label (const octave_value& val)
    {
      set_text (val);
    }

    // Make "Callback" an alias for "MenuSelectedFcn".
    octave_value get_callback (void) const
    {
      return get_menuselectedfcn ();
    }

    void set_callback (const octave_value& val)
    {
      set_menuselectedfcn (val);
    }

  protected:
    void init (void)
    {
      m_position.add_constraint ("min", 0, true);
    }
  };

private:
  properties m_properties;

public:
  uimenu (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~uimenu (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

// FIXME: This class has been renamed to "contextmenu" in Matlab R2020a.
class OCTINTERP_API uicontextmenu : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    void add_dependent_obj (graphics_handle gh)
    { m_dependent_obj_list.push_back (gh); }

    // FIXME: the list may contain duplicates.
    //        Should we return only unique elements?
    const std::list<graphics_handle> get_dependent_obj_list (void)
    { return m_dependent_obj_list; }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uicontextmenu)
      callback_property callback , Matrix ()
      array_property position , Matrix (1, 2, 0.0)

      // Octave-specific properties
      any_property __object__ h , Matrix ()
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_position.add_constraint (dim_vector (1, 2));
      m_position.add_constraint (dim_vector (2, 1));
      m_visible.set (octave_value (false));
    }

  private:
    // List of objects that might depend on this uicontextmenu object
    std::list<graphics_handle> m_dependent_obj_list;

    OCTINTERP_API void update_beingdeleted (void);

  };

private:
  properties m_properties;

public:
  uicontextmenu (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~uicontextmenu (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

class OCTINTERP_API uicontrol : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    OCTINTERP_API Matrix
    get_boundingbox (bool internal = false,
                     const Matrix& parent_pix_size = Matrix ()) const;

    OCTINTERP_API double
    get___fontsize_points__ (double box_pix_height = 0) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uicontrol)
      color_property backgroundcolor , color_values (0.94, 0.94, 0.94)
      callback_property callback , Matrix ()
      array_property cdata , Matrix ()
      bool_property clipping , "on"
      radio_property enable , "{on}|inactive|off"
      array_property extent rG , Matrix (1, 4, 0.0)
      radio_property fontangle u , "{normal}|italic"
      string_property fontname u , OCTAVE_DEFAULT_FONTNAME
      double_property fontsize u , 10
      radio_property fontunits S , "inches|centimeters|normalized|{points}|pixels"
      radio_property fontweight u , "{normal}|bold"
      color_property foregroundcolor , color_values (0, 0, 0)
      radio_property horizontalalignment , "left|{center}|right"
      callback_property keypressfcn , Matrix ()
      double_property listboxtop , 1
      double_property max , 1
      double_property min , 0
      array_property position , default_control_position ()
      array_property sliderstep , default_control_sliderstep ()
      string_array_property string u , ""
      radio_property style S , "{pushbutton}|togglebutton|radiobutton|checkbox|edit|text|slider|frame|listbox|popupmenu"
      string_property tooltipstring , ""
      radio_property units u , "normalized|inches|centimeters|points|{pixels}|characters"
      row_vector_property value , Matrix (1, 1, 0.0)
      radio_property verticalalignment , "top|{middle}|bottom"

      // Octave-specific properties
      bool_property __focus__ h , "off"
      any_property __object__ h , Matrix ()
    END_PROPERTIES

  private:
    std::string m_cached_units;

  protected:
    void init (void)
    {
      m_cdata.add_constraint ("double");
      m_cdata.add_constraint ("single");
      m_cdata.add_constraint ("uint8");
      m_cdata.add_constraint (dim_vector (-1, -1, 3));
      m_cdata.add_constraint (dim_vector (0, 0));
      m_position.add_constraint (dim_vector (1, 4));
      m_sliderstep.add_constraint (dim_vector (1, 2));
      m_fontsize.add_constraint ("min", 0.0, false);
      m_cached_units = get_units ();
    }

    OCTINTERP_API void update_text_extent (void);

    void update_string (void) { update_text_extent (); }
    void update_fontname (void) { update_text_extent (); }
    void update_fontsize (void) { update_text_extent (); }
    void update_fontangle (void)
    {
      update_text_extent ();
    }
    void update_fontweight (void) { update_text_extent (); }

    OCTINTERP_API void update_fontunits (const caseless_str& old_units);

    OCTINTERP_API void update_units (void);

  };

private:
  properties m_properties;

public:
  uicontrol (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~uicontrol (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API uibuttongroup : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    OCTINTERP_API void
    remove_child (const graphics_handle& h, bool from_root = false);

    OCTINTERP_API void adopt (const graphics_handle& h);

    OCTINTERP_API Matrix
    get_boundingbox (bool internal = false,
                     const Matrix& parent_pix_size = Matrix ()) const;

    OCTINTERP_API double
    get___fontsize_points__ (double box_pix_height = 0) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uibuttongroup)
      color_property backgroundcolor , color_values (0.94, 0.94, 0.94)
      radio_property bordertype , "none|{etchedin}|etchedout|beveledin|beveledout|line"
      double_property borderwidth , 1
      bool_property clipping , "on"
      radio_property fontangle , "{normal}|italic"
      string_property fontname , OCTAVE_DEFAULT_FONTNAME
      double_property fontsize , 10
      radio_property fontunits S , "inches|centimeters|normalized|{points}|pixels"
      radio_property fontweight , "{normal}|bold"
      color_property foregroundcolor , color_values (0, 0, 0)
      color_property highlightcolor , color_values (1, 1, 1)
      array_property position S , default_panel_position ()
      // FIXME: "resizefcn" is no longer recommended by Matlab,
      //        and has been replaced with "sizechangedfcn"
      //        Eventually this will need to be hidden, and then removed.
      callback_property resizefcn , Matrix ()
      handle_property selectedobject S , graphics_handle ()
      callback_property selectionchangedfcn , Matrix ()
      color_property shadowcolor , color_values (0.7, 0.7, 0.7)
      callback_property sizechangedfcn , Matrix ()
      radio_property units S , "{normalized}|inches|centimeters|points|pixels|characters"
      string_property title , ""
      radio_property titleposition , "{lefttop}|centertop|righttop|leftbottom|centerbottom|rightbottom"

      // Octave-specific properties
      any_property __object__ h , Matrix ()
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_position.add_constraint (dim_vector (1, 4));
      m_borderwidth.add_constraint ("min", 0.0, true);
      m_fontsize.add_constraint ("min", 0.0, false);
    }

    // void update_text_extent (void);
    // void update_string (void) { update_text_extent (); }
    // void update_fontname (void) { update_text_extent (); }
    // void update_fontsize (void) { update_text_extent (); }
    // void update_fontangle (void) { update_text_extent (); }
    // void update_fontweight (void) { update_fontweight (); }

    OCTINTERP_API void update_units (const caseless_str& old_units);
    OCTINTERP_API void update_fontunits (const caseless_str& old_units);

  };

private:
  properties m_properties;

public:
  uibuttongroup (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~uibuttongroup (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

class OCTINTERP_API uipanel : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    OCTINTERP_API Matrix
    get_boundingbox (bool internal = false,
                     const Matrix& parent_pix_size = Matrix ()) const;

    OCTINTERP_API double
    get___fontsize_points__ (double box_pix_height = 0) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uipanel)
      color_property backgroundcolor , color_values (0.94, 0.94, 0.94)
      radio_property bordertype , "none|{etchedin}|etchedout|beveledin|beveledout|line"
      double_property borderwidth , 1
      radio_property fontangle , "{normal}|italic"
      string_property fontname , OCTAVE_DEFAULT_FONTNAME
      double_property fontsize , 10
      radio_property fontunits S , "inches|centimeters|normalized|{points}|pixels"
      radio_property fontweight , "{normal}|bold"
      color_property foregroundcolor , color_values (0, 0, 0)
      color_property highlightcolor , color_values (1, 1, 1)
      array_property position S , default_panel_position ()
      // FIXME: "resizefcn" is no longer recommended by Matlab,
      //        and has been replaced with "sizechangedfcn"
      //        Eventually this will need to be hidden, and then removed.
      callback_property resizefcn , Matrix ()
      color_property shadowcolor , color_values (0.7, 0.7, 0.7)
      callback_property sizechangedfcn , Matrix ()
      string_property title , ""
      radio_property titleposition , "{lefttop}|centertop|righttop|leftbottom|centerbottom|rightbottom"
      radio_property units S , "{normalized}|inches|centimeters|points|pixels|characters"
      // Octave-specific properties
      any_property __object__ h , Matrix ()
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_borderwidth.add_constraint ("min", 0.0, true);
      m_fontsize.add_constraint ("min", 0.0, false);
      m_position.add_constraint (dim_vector (1, 4));
    }

    OCTINTERP_API void update_units (const caseless_str& old_units);
    OCTINTERP_API void update_fontunits (const caseless_str& old_units);

  };

private:
  properties m_properties;

public:
  uipanel (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~uipanel (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API uitable : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    OCTINTERP_API Matrix
    get_boundingbox (bool internal = false,
                     const Matrix& parent_pix_size = Matrix ()) const;

    OCTINTERP_API double
    get___fontsize_points__ (double box_pix_height = 0) const;

    OCTINTERP_API double
    get_fontsize_pixels (double box_pix_height = 0) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    // FIXME: keypressfcn, keyreleasefcn, rearrangeablecolumns properties
    //        seem to have been removed from Matlab.

    BEGIN_PROPERTIES (uitable)
      any_property __object__ h , Matrix ()
      array_property backgroundcolor , default_table_backgroundcolor ()
      callback_property celleditcallback , Matrix ()
      callback_property cellselectioncallback , Matrix ()
      row_vector_property columneditable , Matrix ()
      any_property columnformat S , Cell ()
      any_property columnname , "numbered"
      any_property columnwidth S , "auto"
      any_property data u , Matrix ()
      bool_property enable , "on"
      array_property extent rG , Matrix (1, 4, 0.0)
      radio_property fontangle u , "{normal}|italic"
      string_property fontname u , OCTAVE_DEFAULT_FONTNAME
      double_property fontsize u , 10
      radio_property fontunits S , "inches|centimeters|normalized|{points}|pixels"
      radio_property fontweight u , "{normal}|bold"
      color_property foregroundcolor , color_values (0, 0, 0)
      callback_property keypressfcn , Matrix ()
      callback_property keyreleasefcn , Matrix ()
      array_property position , default_table_position ()
      bool_property rearrangeablecolumns , "off"
      any_property rowname , "numbered"
      bool_property rowstriping , "on"
      string_property tooltipstring , ""
      radio_property units S , "normalized|inches|centimeters|points|{pixels}|characters"
    END_PROPERTIES

    OCTINTERP_API Matrix get_extent_matrix (void) const;

    OCTINTERP_API Matrix get_backgroundcolor_rgb (void);

    OCTINTERP_API Matrix get_alternatebackgroundcolor_rgb (void);

  protected:
    void init (void)
    {
      m_position.add_constraint (dim_vector (1, 4));
      m_extent.add_constraint (dim_vector (1, 4));
      m_backgroundcolor.add_constraint ("double");
      m_backgroundcolor.add_constraint (dim_vector (-1, 3));
      m_columneditable.add_constraint ("logical");
    }

    OCTINTERP_API void update_units (const caseless_str& old_units);
    OCTINTERP_API void update_fontunits (const caseless_str& old_units);
    void update_table_extent (void) { };
    void update_data (void) { update_table_extent (); }
    void update_fontname (void) { update_table_extent (); }
    void update_fontsize (void) { update_table_extent (); }
    void update_fontangle (void)
    {
      update_table_extent ();
    }
    void update_fontweight (void) { update_table_extent (); }
  };

private:
  properties m_properties;

public:
  uitable (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~uitable (void) { }

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API uitoolbar : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uitoolbar)
      // Octave-specific properties
      any_property __object__ h , Matrix ()
    END_PROPERTIES

  protected:
    void init (void)
    { }
  };

private:
  properties m_properties;

public:
  uitoolbar (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p), m_default_properties ()
  { }

  ~uitoolbar (void) = default;

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (figure) to override first (properties knows how
    // to find the parent object).
    m_properties.override_defaults (obj);

    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (m_default_properties);
  }

  void set (const caseless_str& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      m_default_properties.set (name.substr (7), value);
    else
      m_properties.set (name, value);
  }

  octave_value get (const caseless_str& name) const
  {
    octave_value retval;

    if (name.compare ("default", 7))
      retval = get_default (name.substr (7));
    else
      retval = m_properties.get (name);

    return retval;
  }

  OCTINTERP_API octave_value get_default (const caseless_str& name) const;

  octave_value get_defaults (void) const
  {
    return m_default_properties.as_struct ("default");
  }

  property_list get_defaults_list (void) const
  {
    return m_default_properties;
  }

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  OCTINTERP_API void reset_default_properties (void);

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

private:
  property_list m_default_properties;
};

// ---------------------------------------------------------------------

class OCTINTERP_API uipushtool : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uipushtool)
      array_property cdata , Matrix ()
      callback_property clickedcallback , Matrix ()
      bool_property enable , "on"
      bool_property separator , "off"
      string_property tooltipstring , ""

      // Octave-specific properties
      string_property __named_icon__ , ""
      any_property __object__ h , Matrix ()
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_cdata.add_constraint ("double");
      m_cdata.add_constraint ("single");
      m_cdata.add_constraint ("uint8");
      m_cdata.add_constraint (dim_vector (-1, -1, 3));
      m_cdata.add_constraint (dim_vector (0, 0));
    }
  };

private:
  properties m_properties;

public:
  uipushtool (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~uipushtool (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

class OCTINTERP_API uitoggletool : public base_graphics_object
{
public:

  class OCTINTERP_API properties : public base_properties
  {
  public:

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uitoggletool)
      array_property cdata , Matrix ()
      callback_property clickedcallback , Matrix ()
      bool_property enable , "on"
      callback_property offcallback , Matrix ()
      callback_property oncallback , Matrix ()
      bool_property separator , "off"
      bool_property state , "off"
      string_property tooltipstring , ""

      // Octave-specific properties
      string_property __named_icon__ , ""
      any_property __object__ h , Matrix ()
    END_PROPERTIES

  protected:
    void init (void)
    {
      m_cdata.add_constraint ("double");
      m_cdata.add_constraint ("single");
      m_cdata.add_constraint ("uint8");
      m_cdata.add_constraint (dim_vector (-1, -1, 3));
      m_cdata.add_constraint (dim_vector (0, 0));
    }
  };

private:
  properties m_properties;

public:
  uitoggletool (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), m_properties (mh, p)
  { }

  ~uitoggletool (void) = default;

  base_properties& get_properties (void) { return m_properties; }

  const base_properties& get_properties (void) const { return m_properties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = m_properties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

OCTINTERP_API octave_value
get_property_from_handle (double handle, const std::string& property,
                          const std::string& fcn);
OCTINTERP_API bool
set_property_in_handle (double handle, const std::string& property,
                        const octave_value& arg, const std::string& fcn);

// ---------------------------------------------------------------------

class graphics_event;

class
OCTINTERP_API
base_graphics_event
{
public:
  enum priority { INTERRUPT, QUEUE, CANCEL };

  friend class graphics_event;

  base_graphics_event (void)
    : m_busyaction (QUEUE)
  { };

  base_graphics_event (int busyaction)
    : m_busyaction (busyaction)
  { };

  virtual ~base_graphics_event (void) = default;

  int get_busyaction (void) { return m_busyaction; };

  virtual void execute (void) = 0;

private:
  int m_busyaction;
};

class
OCTINTERP_API
graphics_event
{
public:

  typedef void (*event_fcn) (void *);

  graphics_event (void) = default;

  graphics_event (base_graphics_event *new_rep) : m_rep (new_rep) { }

  graphics_event (const graphics_event&) = default;

  ~graphics_event (void) = default;

  graphics_event& operator = (const graphics_event&) = default;

  int get_busyaction (void)
  {
    if (ok ())
      return m_rep->get_busyaction ();
    else
      error ("graphics_event::busyaction: invalid graphics_event");
  }

  void execute (void)
  {
    if (ok ())
      m_rep->execute ();
  }

  bool ok (void) const { return (m_rep != nullptr); }

  static OCTINTERP_API graphics_event
  create_callback_event (const graphics_handle& h,
                         const std::string& name,
                         const octave_value& data = Matrix (),
                         int busyaction = base_graphics_event::QUEUE);

  static OCTINTERP_API graphics_event
  create_callback_event (const graphics_handle& h,
                         const octave_value& cb,
                         const octave_value& data = Matrix (),
                         int busyaction = base_graphics_event::QUEUE);

  static OCTINTERP_API graphics_event
  create_mcode_event (const graphics_handle& h, const std::string& cmd,
                      int busyaction);

  static OCTINTERP_API graphics_event
  create_function_event (event_fcn fcn, void *data = nullptr);

  static OCTINTERP_API graphics_event
  create_set_event (const graphics_handle& h, const std::string& name,
                    const octave_value& value, bool notify_toolkit = true,
                    bool redraw_figure = false);
private:

  std::shared_ptr <base_graphics_event> m_rep;
};

class OCTINTERP_API gh_manager
{
public:

  typedef std::pair<uint8NDArray /*pixels*/, std::string /*svg*/> latex_data;

  OCTINTERP_API gh_manager (octave::interpreter& interp);

  // FIXME: eventually eliminate these static functions and access
  // gh_manager object through the interpreter.

  OCTINTERP_API graphics_handle get_handle (bool integer_figure_handle);

  OCTINTERP_API void free (const graphics_handle& h, bool from_root = false);

  OCTINTERP_API void renumber_figure (const graphics_handle& old_gh,
                                      const graphics_handle& new_gh);

  graphics_handle lookup (double val) const
  {
    const_iterator p = (octave::math::isnan (val)
                        ? m_handle_map.end () : m_handle_map.find (val));

    return (p != m_handle_map.end ()) ? p->first : graphics_handle ();
  }

  graphics_handle lookup (const octave_value& val) const
  {
    return (val.is_real_scalar ()
            ? lookup (val.double_value ()) : graphics_handle ());
  }

  graphics_object get_object (double val) const
  {
    return get_object (lookup (val));
  }

  graphics_object get_object (const graphics_handle& h) const
  {
    const_iterator p = (h.ok () ? m_handle_map.find (h) : m_handle_map.end ());

    return (p != m_handle_map.end ()) ? p->second : graphics_object ();
  }

  OCTINTERP_API graphics_handle
  make_graphics_handle (const std::string& go_name,
                        const graphics_handle& p,
                        bool integer_figure_handle = false,
                        bool call_createfcn = true,
                        bool notify_toolkit = true);

  OCTINTERP_API graphics_handle
  make_figure_handle (double val, bool notify_toolkit = true);

  OCTINTERP_API void push_figure (const graphics_handle& h);

  OCTINTERP_API void pop_figure (const graphics_handle& h);

  graphics_handle current_figure (void) const
  {
    graphics_handle retval;

    for (const auto& hfig : m_figure_list)
      {
        if (is_handle_visible (hfig))
          retval = hfig;
      }

    return retval;
  }

  Matrix handle_list (bool show_hidden = false)
  {
    Matrix retval (1, m_handle_map.size ());

    octave_idx_type i = 0;
    for (const auto& h_iter : m_handle_map)
      {
        graphics_handle h = h_iter.first;

        if (show_hidden || is_handle_visible (h))
          retval(i++) = h.value ();
      }

    retval.resize (1, i);

    return retval;
  }

  void lock (void) { m_graphics_lock.lock (); }

  bool try_lock (void) { return m_graphics_lock.try_lock (); }

  void unlock (void) { m_graphics_lock.unlock (); }

  Matrix figure_handle_list (bool show_hidden = false)
  {
    Matrix retval (1, m_figure_list.size ());

    octave_idx_type i = 0;
    for (const auto& hfig : m_figure_list)
      {
        if (show_hidden || is_handle_visible (hfig))
          retval(i++) = hfig.value ();
      }

    retval.resize (1, i);

    return retval;
  }

  OCTINTERP_API void
  execute_listener (const graphics_handle& h, const octave_value& l);

  void execute_callback (const graphics_handle& h,
                         const std::string& name,
                         const octave_value& data = Matrix ())
  {
    octave_value cb;

    if (true)
      {
        octave::autolock guard (graphics_lock ());

        graphics_object go = get_object (h);

        if (go.valid_object ())
          cb = go.get (name);
      }

    execute_callback (h, cb, data);
  }

  OCTINTERP_API void
  execute_callback (const graphics_handle& h, const octave_value& cb,
                    const octave_value& data = Matrix ());

  OCTINTERP_API void
  post_callback (const graphics_handle& h, const std::string& name,
                 const octave_value& data = Matrix ());

  OCTINTERP_API void
  post_function (graphics_event::event_fcn fcn, void *fcn_data = nullptr);

  OCTINTERP_API void
  post_set (const graphics_handle& h, const std::string& name,
            const octave_value& value, bool notify_toolkit = true,
            bool redraw_figure = false);

  OCTINTERP_API int process_events (bool force = false);

  OCTINTERP_API void enable_event_processing (bool enable = true);

  bool is_handle_visible (const graphics_handle& h) const
  {
    bool retval = false;

    graphics_object go = get_object (h);

    if (go.valid_object ())
      retval = go.is_handle_visible ();

    return retval;
  }

  OCTINTERP_API void close_all_figures (void);

  OCTINTERP_API void restore_gcbo (void);

  OCTINTERP_API void post_event (const graphics_event& e);

  octave::mutex graphics_lock (void)
  {
    return m_graphics_lock;
  }

  latex_data get_latex_data (const std::string& key) const
  {
    latex_data retval;

    const auto it = m_latex_cache.find (key);

    if (it != m_latex_cache.end ())
      retval = it->second;

    return retval;
  }

  void set_latex_data (const std::string& key, latex_data val)
  {
    // Limit the number of cache entries to 500
    if (m_latex_keys.size () >= 500)
      {
        auto it = m_latex_cache.find (m_latex_keys.front ());

        if (it != m_latex_cache.end ())
          m_latex_cache.erase (it);

        m_latex_keys.pop_front ();
      }

    m_latex_cache[key] = val;
    m_latex_keys.push_back (key);
  }

private:

  typedef std::map<graphics_handle, graphics_object>::iterator iterator;
  typedef std::map<graphics_handle, graphics_object>::const_iterator
    const_iterator;

  typedef std::set<graphics_handle>::iterator free_list_iterator;
  typedef std::set<graphics_handle>::const_iterator const_free_list_iterator;

  typedef std::list<graphics_handle>::iterator figure_list_iterator;
  typedef std::list<graphics_handle>::const_iterator const_figure_list_iterator;

  octave::interpreter& m_interpreter;

  // A map of handles to graphics objects.
  std::map<graphics_handle, graphics_object> m_handle_map;

  // The available graphics handles.
  std::set<graphics_handle> m_handle_free_list;

  // The next handle available if m_handle_free_list is empty.
  double m_next_handle;

  // The allocated figure handles.  Top of the stack is most recently
  // created.
  std::list<graphics_handle> m_figure_list;

  // The lock for accessing the graphics sytsem.
  octave::mutex m_graphics_lock;

  // The list of events queued by graphics toolkits.
  std::list<graphics_event> m_event_queue;

  // The stack of callback objects.
  std::list<graphics_object> m_callback_objects;

  // A flag telling whether event processing must be constantly on.
  int m_event_processing;

  // Cache of already parsed latex strings. Store a separate list of keys
  // to allow for erasing oldest entries if cache size becomes too large.
  std::unordered_map<std::string, latex_data> m_latex_cache;
  std::list<std::string> m_latex_keys;
};

OCTINTERP_API void
get_children_limits (double& min_val, double& max_val,
                     double& min_pos, double& max_neg,
                     const Matrix& kids, char limit_type);

OCTINTERP_API int calc_dimensions (const graphics_object& gh);

// This function is NOT equivalent to the scripting language function gcf.
OCTINTERP_API graphics_handle gcf (void);

// This function is NOT equivalent to the scripting language function gca.
OCTINTERP_API graphics_handle gca (void);

OCTINTERP_API void close_all_figures (void);

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::base_scaler' instead")
typedef octave::base_scaler base_scaler;

OCTAVE_DEPRECATED (7, "use 'octave::lin_scaler' instead")
typedef octave::lin_scaler lin_scaler;

OCTAVE_DEPRECATED (7, "use 'octave::log_scaler' instead")
typedef octave::log_scaler log_scaler;

OCTAVE_DEPRECATED (7, "use 'octave::neg_log_scaler' instead")
typedef octave::neg_log_scaler neg_log_scaler;

OCTAVE_DEPRECATED (7, "use 'octave::scaler' instead")
typedef octave::scaler scaler;

OCTAVE_DEPRECATED (7, "use 'octave::base_property' instead")
typedef octave::base_property base_property;

OCTAVE_DEPRECATED (7, "use 'octave::string_property' instead")
typedef octave::string_property string_property;

OCTAVE_DEPRECATED (7, "use 'octave::string_array_property' instead")
typedef octave::string_array_property string_array_property;

OCTAVE_DEPRECATED (7, "use 'octave::text_label_property' instead")
typedef octave::text_label_property text_label_property;

OCTAVE_DEPRECATED (7, "use 'octave::radio_values' instead")
typedef octave::radio_values radio_values;

OCTAVE_DEPRECATED (7, "use 'octave::radio_property' instead")
typedef octave::radio_property radio_property;

OCTAVE_DEPRECATED (7, "use 'octave::color_values' instead")
typedef octave::color_values color_values;

OCTAVE_DEPRECATED (7, "use 'octave::color_property' instead")
typedef octave::color_property color_property;

OCTAVE_DEPRECATED (7, "use 'octave::double_property' instead")
typedef octave::double_property double_property;

OCTAVE_DEPRECATED (7, "use 'octave::double_radio_property' instead")
typedef octave::double_radio_property double_radio_property;

OCTAVE_DEPRECATED (7, "use 'octave::array_property' instead")
typedef octave::array_property array_property;

OCTAVE_DEPRECATED (7, "use 'octave::row_vector_property' instead")
typedef octave::row_vector_property row_vector_property;

OCTAVE_DEPRECATED (7, "use 'octave::bool_property' instead")
typedef octave::bool_property bool_property;

OCTAVE_DEPRECATED (7, "use 'octave::handle_property' instead")
typedef octave::handle_property handle_property;

OCTAVE_DEPRECATED (7, "use 'octave::any_property' instead")
typedef octave::any_property any_property;

OCTAVE_DEPRECATED (7, "use 'octave::children_property' instead")
typedef octave::children_property children_property;

OCTAVE_DEPRECATED (7, "use 'octave::callback_property' instead")
typedef octave::callback_property callback_property;

OCTAVE_DEPRECATED (7, "use 'octave::property' instead")
typedef octave::property property;

OCTAVE_DEPRECATED (7, "use 'octave::pval_vector' instead")
typedef octave::pval_vector pval_vector;

OCTAVE_DEPRECATED (7, "use 'octave::property_list' instead")
typedef octave::property_list property_list;

OCTAVE_DEPRECATED (7, "use 'octave::base_properties' instead")
typedef octave::base_properties base_properties;

OCTAVE_DEPRECATED (7, "use 'octave::base_graphics_object' instead")
typedef octave::base_graphics_object base_graphics_object;

OCTAVE_DEPRECATED (7, "use 'octave::graphics_object' instead")
typedef octave::graphics_object graphics_object;

OCTAVE_DEPRECATED (7, "use 'octave::root_figure' instead")
typedef octave::root_figure root_figure;

OCTAVE_DEPRECATED (7, "use 'octave::figure' instead")
typedef octave::figure figure;

OCTAVE_DEPRECATED (7, "use 'octave::graphics_xform' instead")
typedef octave::graphics_xform graphics_xform;

OCTAVE_DEPRECATED (7, "use 'octave::axes' instead")
typedef octave::axes axes;

OCTAVE_DEPRECATED (7, "use 'octave::line' instead")
typedef octave::line line;

OCTAVE_DEPRECATED (7, "use 'octave::text' instead")
typedef octave::text text;

OCTAVE_DEPRECATED (7, "use 'octave::image' instead")
typedef octave::image image;

OCTAVE_DEPRECATED (7, "use 'octave::light' instead")
typedef octave::light light;

OCTAVE_DEPRECATED (7, "use 'octave::patch' instead")
typedef octave::patch patch;

OCTAVE_DEPRECATED (7, "use 'octave::scatter' instead")
typedef octave::scatter scatter;

OCTAVE_DEPRECATED (7, "use 'octave::surface' instead")
typedef octave::surface surface;

OCTAVE_DEPRECATED (7, "use 'octave::hggroup' instead")
typedef octave::hggroup hggroup;

OCTAVE_DEPRECATED (7, "use 'octave::uimenu' instead")
typedef octave::uimenu uimenu;

OCTAVE_DEPRECATED (7, "use 'octave::uicontextmenu' instead")
typedef octave::uicontextmenu uicontextmenu;

OCTAVE_DEPRECATED (7, "use 'octave::uicontrol' instead")
typedef octave::uicontrol uicontrol;

OCTAVE_DEPRECATED (7, "use 'octave::uibuttongroup' instead")
typedef octave::uibuttongroup uibuttongroup;

OCTAVE_DEPRECATED (7, "use 'octave::uipanel' instead")
typedef octave::uipanel uipanel;

OCTAVE_DEPRECATED (7, "use 'octave::uitable' instead")
typedef octave::uitable uitable;

OCTAVE_DEPRECATED (7, "use 'octave::uitoolbar' instead")
typedef octave::uitoolbar uitoolbar;

OCTAVE_DEPRECATED (7, "use 'octave::uipushtool' instead")
typedef octave::uipushtool uipushtool;

OCTAVE_DEPRECATED (7, "use 'octave::uitoggletool' instead")
typedef octave::uitoggletool uitoggletool;

OCTAVE_DEPRECATED (7, "use 'octave::base_graphics_event' instead")
typedef octave::base_graphics_event base_graphics_event;

OCTAVE_DEPRECATED (7, "use 'octave::graphics_event' instead")
typedef octave::graphics_event graphics_event;

OCTAVE_DEPRECATED (7, "use 'octave::gh_manager' instead")
typedef octave::gh_manager gh_manager;

#endif

#endif
