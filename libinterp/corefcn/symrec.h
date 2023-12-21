////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_symrec_h)
#define octave_symrec_h 1

#include "octave-config.h"

#include <deque>
#include <list>
#include <memory>
#include <string>

class octave_user_function;

#include "ov.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope_rep;

class symbol_record
{
public:

  typedef std::size_t context_id;

  enum symrec_t : unsigned char
  {
    // generic variable
    LOCAL = 1,
    // formal parameter
    FORMAL = 2,
    // this symbol may NOT become a variable.
    // (symbol added to a static workspace)
    ADDED_STATIC = 4,
    // this symbol was recognized as a variable from syntax
    VARIABLE = 8
  };

private:

  class symbol_record_rep
  {
  public:

    symbol_record_rep () = delete;

    symbol_record_rep (const std::string& nm, symrec_t sc)
      : m_frame_offset (0), m_data_offset (0), m_storage_class (sc),
        m_name (nm)
    { }

    OCTAVE_DEFAULT_COPY_MOVE_DELETE (symbol_record_rep)

    // FIXME: use special storage class instead?
    bool is_valid () const { return ! m_name.empty (); }

    void set_frame_offset (std::size_t offset) { m_frame_offset = offset; }

    std::size_t frame_offset () const { return m_frame_offset; }

    void set_data_offset (std::size_t offset) { m_data_offset = offset; }

    std::size_t data_offset () const { return m_data_offset; }

    bool is_local () const
    {
      return m_storage_class & LOCAL;
    }

    bool is_formal () const
    {
      return m_storage_class & FORMAL;
    }

    bool is_added_static () const
    {
      return m_storage_class & ADDED_STATIC;
    }

    bool is_variable () const
    {
      return m_storage_class & VARIABLE;
    }

    void mark_local ()
    {
      m_storage_class = static_cast<symrec_t> (m_storage_class | LOCAL);
    }

    void mark_formal ()
    {
      // Formal parameters are also variables.
      m_storage_class = static_cast<symrec_t> (m_storage_class
                        | FORMAL | VARIABLE);
    }

    void mark_added_static ()
    {
      m_storage_class = static_cast<symrec_t> (m_storage_class
                        | ADDED_STATIC);
    }

    void mark_variable ()
    {
      m_storage_class = static_cast<symrec_t> (m_storage_class | VARIABLE);
    }

    OCTAVE_DEPRECATED (10, "symbol_record_rep::mark_as_variable is obsolete, use mark_variable")
    void mark_as_variable ()
    {
      m_storage_class = static_cast<symrec_t> (m_storage_class | VARIABLE);
    }


    void unmark_local ()
    {
      m_storage_class = static_cast<symrec_t> (m_storage_class & ~LOCAL);
    }

    void unmark_formal ()
    {
      m_storage_class = static_cast<symrec_t> (m_storage_class & ~FORMAL);
    }

    void unmark_added_static ()
    {
      m_storage_class = static_cast<symrec_t> (m_storage_class
                        & ~ADDED_STATIC);
    }

    void unmark_variable ()
    {
      m_storage_class = static_cast<symrec_t> (m_storage_class & ~VARIABLE);
    }

    OCTAVE_DEPRECATED (10, "symbol_record_rep::unmark_as_variable is obsolete, use unmark_variable")
    void unmark_as_variable ()
    {
      m_storage_class = static_cast<symrec_t> (m_storage_class & ~VARIABLE);
    }

    unsigned int storage_class () const { return m_storage_class; }

    std::shared_ptr<symbol_record_rep> dup () const;

    octave_value dump () const;

    std::string name () const { return m_name; }

    void rename (const std::string& new_name) { m_name = new_name; }

  private:

    std::size_t m_frame_offset;
    std::size_t m_data_offset;

    symrec_t m_storage_class;

    std::string m_name;
  };

public:

  symbol_record (const std::string& nm = "", symrec_t sc = LOCAL)
    : m_rep (new symbol_record_rep (nm, sc))
  { }

  symbol_record (const std::string& nm, const octave_value&,
                 symrec_t sc = LOCAL)
    : m_rep (new symbol_record_rep (nm, sc))
  { }

  symbol_record (const symbol_record&) = default;

  symbol_record& operator = (const symbol_record&) = default;

  ~symbol_record () = default;

  bool is_valid () const { return m_rep->is_valid (); }

  explicit operator bool () const { return is_valid (); }

  void set_frame_offset (std::size_t offset)
  { m_rep->set_frame_offset (offset); }

  std::size_t frame_offset () const { return m_rep->frame_offset (); }

  void set_data_offset (std::size_t offset)
  { m_rep->set_data_offset (offset); }

  std::size_t data_offset () const { return m_rep->data_offset (); }

  symbol_record dup () const { return symbol_record (m_rep->dup ()); }

  std::string name () const { return m_rep->name (); }

  void rename (const std::string& new_name) { m_rep->rename (new_name); }

  bool is_local () const { return m_rep->is_local (); }
  bool is_formal () const { return m_rep->is_formal (); }
  bool is_added_static () const { return m_rep->is_added_static (); }
  bool is_variable () const { return m_rep->is_variable (); }

  void mark_local () { m_rep->mark_local (); }
  void mark_formal () { m_rep->mark_formal (); }
  void mark_added_static () { m_rep->mark_added_static (); }
  void mark_variable () { m_rep->mark_variable (); }
  OCTAVE_DEPRECATED (10, "symbol_record::mark_as_variable is obsolete, use mark_variable")
  void mark_as_variable () { m_rep->mark_variable (); }

  void unmark_local () { m_rep->unmark_local (); }
  void unmark_formal () { m_rep->unmark_formal (); }
  void unmark_added_static () { m_rep->unmark_added_static (); }
  void unmark_variable () { m_rep->unmark_variable (); }
  OCTAVE_DEPRECATED (10, "symbol_record::unmark_as_variable is obsolete, use unmark_variable")
  void unmark_as_variable () { m_rep->unmark_variable (); }

  unsigned int storage_class () const { return m_rep->storage_class (); }

  octave_value dump () const { return m_rep->dump (); }

private:

  std::shared_ptr<symbol_record_rep> m_rep;

  // NEW_REP must be dynamically allocated or nullptr.
  symbol_record (const std::shared_ptr<symbol_record_rep>& new_rep)
    : m_rep (new_rep)
  { }
};

OCTAVE_END_NAMESPACE(octave)

#endif
