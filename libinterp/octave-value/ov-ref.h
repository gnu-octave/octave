////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if ! defined (octave_ov_ref_h)
#define octave_ov_ref_h 1

#include "octave-config.h"

#include "ov-base.h"
#include "ovl.h"
#include "symscope.h"

#include <string>
#include <memory>

// octave_value_ref is to be used by the VM to implement
// global and persistent values.
//
// octave_value_ref need to overload any virtual call
// used by the assign and push slot op-codes.
//
// Any octave_value_ref should never leave the VM slots
// on the VM stack.

// Abstract type
class OCTINTERP_API
octave_value_ref : public octave_base_value
{
public:
    octave_value_ref () = default;
    ~octave_value_ref () = default;

    octave_value_ref * ref_rep () { return this; }
    bool is_ref () const { return true; }

    virtual octave_value deref () = 0;
    virtual void set_value (octave_value val) = 0;
    virtual octave_value & ref () = 0;
    virtual void maybe_save_state  () {};

    virtual bool is_global_ref () { return false; }
    virtual bool is_persistent_ref () { return false; }

    void maybe_call_dtor ();
    octave_value simple_subsasgn (char type, octave_value_list& idx, const octave_value& rhs);
    octave_value subsasgn (const std::string& type, const std::list<octave_value_list>& idx, const octave_value& rhs);
    octave_base_value * unique_clone ();
    bool is_defined () const;
    bool is_maybe_function (void) const;
};

class OCTINTERP_API
octave_value_ref_global : public octave_value_ref
{
public:
    octave_value_ref_global () = default;
    ~octave_value_ref_global () = default;
    octave_value_ref_global (std::string name)
        : m_name (name) {};

    octave_value deref ();
    octave_value & ref ();
    void set_value (octave_value val);
    
    bool is_global_ref () { return true; }

private:
    std::string m_name;

    DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

class OCTINTERP_API
octave_value_ref_persistent : public octave_value_ref
{
public:
    octave_value_ref_persistent () = default;
    ~octave_value_ref_persistent () = default;
    octave_value_ref_persistent (octave::symbol_scope scope, int offset)
        : m_offset (offset), m_scope (scope) {};

    octave_value deref ();
    octave_value & ref ();
    void set_value (octave_value val);
    
    bool is_persistent_ref () { return true; }

private:
    int m_offset;
    octave::symbol_scope m_scope;

    DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
