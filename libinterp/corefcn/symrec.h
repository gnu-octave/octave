/*

Copyright (C) 1993-2017 John W. Eaton
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

#if ! defined (octave_symrec_h)
#define octave_symrec_h 1

#include "octave-config.h"

#include <deque>
#include <list>
#include <string>

#include "oct-refcount.h"

class octave_user_function;

#include "ov.h"
#include "ovl.h"

namespace octave
{
  class fcn_info;
  class scope;

  class symbol_record
  {
  public:

    typedef size_t context_id;

    // generic variable
    static const unsigned int local = 1;

    // varargin, argn, .nargin., .nargout.
    // (FIXME: is this really used now?)
    static const unsigned int automatic = 2;

    // formal parameter
    static const unsigned int formal = 4;

    // not listed or cleared (.nargin., .nargout.)
    static const unsigned int hidden = 8;

    // inherited from parent scope; not cleared at function exit
    static const unsigned int inherited = 16;

    // global (redirects to global scope)
    static const unsigned int global = 32;

    // not cleared at function exit
    static const unsigned int persistent = 64;

    // this symbol may NOT become a variable.
    // (symbol added to a static workspace)
    static const unsigned int added_static = 128;

  private:

    class symbol_record_rep
    {
    public:

      symbol_record_rep (scope *s, const std::string& nm,
                         const octave_value& v, unsigned int sc)
        : m_decl_scope (s), curr_fcn (nullptr), name (nm),
          m_fwd_rep (nullptr), value_stack (),
          storage_class (sc), /* finfo (), */ valid (true), count (1)
      {
        value_stack.push_back (v);
      }

      // No copying!

      symbol_record_rep (const symbol_record_rep& ov) = delete;

      symbol_record_rep& operator = (const symbol_record_rep&) = delete;

      ~symbol_record_rep (void) = default;

      void assign (const octave_value& value)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->assign (value);
            return;
          }

        varref () = value;
      }

      void assign (octave_value::assign_op op,
                   const std::string& type,
                   const std::list<octave_value_list>& idx,
                   const octave_value& value)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->assign (op, type, idx, value);
            return;
          }

        varref().assign (op, type, idx, value);
      }

      void assign (octave_value::assign_op op, const octave_value& value)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->assign (op, value);
            return;
          }

        varref().assign (op, value);
      }

      void do_non_const_unary_op (octave_value::unary_op op)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->do_non_const_unary_op (op);
            return;
          }

        varref().do_non_const_unary_op (op);
      }

      void do_non_const_unary_op (octave_value::unary_op op,
                                  const std::string& type,
                                  const std::list<octave_value_list>& idx)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->do_non_const_unary_op (op, type, idx);
            return;
          }

        varref().do_non_const_unary_op (op, type, idx);
      }

      context_id get_decl_scope_context (void) const;

      octave_value& varref (void)
      {
        if (m_fwd_rep)
          return m_fwd_rep->varref ();

        context_id context = m_decl_scope ? get_decl_scope_context () : 0;

        if (is_global ())
          return xglobal_varref ();
        else if (is_persistent ())
          return xpersistent_varref ();
        else
          {
            context_id n = value_stack.size ();
            while (n++ <= context)
              value_stack.push_back (octave_value ());

            return value_stack[context];
          }
      }

      octave_value varval (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->varval ();

        context_id context = m_decl_scope ? get_decl_scope_context () : 0;

        if (is_global ())
          return xglobal_varval ();
        else if (is_persistent ())
          return xpersistent_varval ();
        else
          {
            if (context < value_stack.size ())
              return value_stack[context];
            else
              return octave_value ();
          }
      }

      void push_context (scope *sid)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->push_context (sid);
            return;
          }

        if (! (is_persistent () || is_global ())
            && sid == decl_scope ())
          value_stack.push_back (octave_value ());
      }

      // If pop_context returns 0, we are out of values and this element
      // of the symbol table should be deleted.  This can happen for
      // functions like
      //
      //   function foo (n)
      //     if (n > 0)
      //       foo (n-1);
      //     else
      //       eval ("x = 1");
      //     endif
      //   endfunction
      //
      // Here, X should only exist in the final stack frame.

      size_t pop_context (scope *sid)
      {
        if (m_fwd_rep)
          return m_fwd_rep->pop_context (sid);

        size_t retval = 1;

        if (! (is_persistent () || is_global ())
            && sid == decl_scope ())
          {
            value_stack.pop_back ();
            retval = value_stack.size ();
          }

        return retval;
      }

      void clear (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->clear ();
            return;
          }

        clear (decl_scope ());
      }

      void clear (scope *sid);

      bool is_defined (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_defined ();

        return varval ().is_defined ();
      }

      bool is_valid (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_valid ();

        return valid;
      }

      bool is_variable (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_variable ();

        return (! is_local () || is_defined ());
      }

      bool is_local (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_local ();

        return storage_class & local;
      }

      bool is_automatic (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_automatic ();

        return storage_class & automatic;
      }

      bool is_formal (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_formal ();

        return storage_class & formal;
      }

      bool is_hidden (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_hidden ();

        return storage_class & hidden;
      }

      bool is_inherited (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_inherited ();

        return storage_class & inherited;
      }

      bool is_global (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_global ();

        return storage_class & global;
      }

      bool is_persistent (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_persistent ();

        return storage_class & persistent;
      }

      bool is_added_static (void) const
      {
        if (m_fwd_rep)
          return m_fwd_rep->is_added_static ();

        return storage_class & added_static;
      }

      void mark_local (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->mark_local ();
            return;
          }

        storage_class |= local;
      }

      void mark_automatic (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->mark_automatic ();
            return;
          }

        storage_class |= automatic;
      }

      void mark_formal (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->mark_formal ();
            return;
          }

        storage_class |= formal;
      }

      void mark_hidden (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->mark_hidden ();
            return;
          }

        storage_class |= hidden;
      }

      void mark_inherited (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->mark_inherited ();
            return;
          }

        storage_class |= inherited;
      }

      void mark_global (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->mark_global ();
            return;
          }

        if (is_persistent ())
          error ("can't make persistent variable %s global", name.c_str ());

        storage_class |= global;
      }

      void mark_persistent (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->mark_persistent ();
            return;
          }

        if (is_global ())
          error ("can't make global variable %s persistent", name.c_str ());

        storage_class |= persistent;
      }

      void mark_added_static (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->mark_added_static ();
            return;
          }

        storage_class |= added_static;
      }

      void unmark_local (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->unmark_local ();
            return;
          }

        storage_class &= ~local;
      }

      void unmark_automatic (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->unmark_automatic ();
            return;
          }

        storage_class &= ~automatic;
      }

      void unmark_formal (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->unmark_formal ();
            return;
          }

        storage_class &= ~formal;
      }

      void unmark_hidden (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->unmark_hidden ();
            return;
          }

        storage_class &= ~hidden;
      }

      void unmark_inherited (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->unmark_inherited ();
            return;
          }

        storage_class &= ~inherited;
      }

      void unmark_global (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->unmark_global ();
            return;
          }

        storage_class &= ~global;
      }

      void unmark_persistent (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->unmark_persistent ();
            return;
          }

        storage_class &= ~persistent;
      }

      void unmark_added_static (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->unmark_added_static ();
            return;
          }

        storage_class &= ~added_static;
      }

      void init_persistent (void);

      void invalidate (void)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->invalidate ();
            return;
          }

        valid = false;
      }

      void erase_persistent (void);

      scope *decl_scope (void)
      {
        if (m_fwd_rep)
          return m_fwd_rep->decl_scope ();

        return m_decl_scope;
      }

      void set_curr_fcn (octave_user_function *fcn)
      {
        if (m_fwd_rep)
          {
            m_fwd_rep->set_curr_fcn (fcn);
            return;
          }

        curr_fcn = fcn;
      }

      // We don't forward more than once, so no need to forward the
      // next two.

      void bind_fwd_rep (symbol_record_rep *fwd_rep) { m_fwd_rep = fwd_rep; }

      void unbind_fwd_rep (void) { m_fwd_rep = nullptr; }

      symbol_record_rep * dup (scope *new_scope) const;

      octave_value dump (void) const;

      scope *m_decl_scope;

      octave_user_function *curr_fcn;

      std::string name;

      symbol_record_rep *m_fwd_rep;

      std::deque<octave_value> value_stack;

      unsigned int storage_class;

      //      fcn_info *finfo;

      bool valid;

      refcount<size_t> count;

    private:

      octave_value& xglobal_varref (void);

      octave_value& xpersistent_varref (void);

      octave_value xglobal_varval (void) const;

      octave_value xpersistent_varval (void) const;
    };

  public:

    symbol_record (void);

    symbol_record (scope *s, const std::string& nm = "",
                   const octave_value& v = octave_value (),
                   unsigned int sc = local)
      : rep (new symbol_record_rep (s, nm, v, sc)) { }

    symbol_record (const symbol_record& sr)
      : rep (sr.rep)
    {
      rep->count++;
    }

    symbol_record& operator = (const symbol_record& sr)
    {
      if (this != &sr)
        {
          if (--rep->count == 0)
            delete rep;

          rep = sr.rep;
          rep->count++;
        }

      return *this;
    }

    ~symbol_record (void)
    {
      if (--rep->count == 0)
        delete rep;
    }

    symbol_record dup (scope *sid) const
    {
      return symbol_record (rep->dup (sid));
    }

    const std::string& name (void) const { return rep->name; }

    void rename (const std::string& new_name) { rep->name = new_name; }

    octave_value
    find (const octave_value_list& args = octave_value_list ()) const;

    void assign (const octave_value& value)
    {
      rep->assign (value);
    }

    void assign (octave_value::assign_op op,
                 const std::string& type,
                 const std::list<octave_value_list>& idx,
                 const octave_value& value)
    {
      rep->assign (op, type, idx, value);
    }

    void assign (octave_value::assign_op op, const octave_value& value)
    {
      rep->assign (op, value);
    }

    void do_non_const_unary_op (octave_value::unary_op op)
    {
      rep->do_non_const_unary_op (op);
    }

    void do_non_const_unary_op (octave_value::unary_op op,
                                const std::string& type,
                                const std::list<octave_value_list>& idx)
    {
      rep->do_non_const_unary_op (op, type, idx);
    }

    // Delete when deprecated varref functions are removed.
    octave_value& varref (void)
    {
      return rep->varref ();
    }

    octave_value varval (void) const
    {
      return rep->varval ();
    }

    void push_context (scope *sid) { rep->push_context (sid); }

    size_t pop_context (scope *sid) { return rep->pop_context (sid); }

    void clear (void) { rep->clear (); }

    void clear (scope *sid) { rep->clear (sid); }

    bool is_defined (void) const
    {
      return rep->is_defined ();
    }

    bool is_undefined (void) const
    {
      return ! rep->is_defined ();
    }

    bool is_valid (void) const
    {
      return rep->is_valid ();
    }

    bool is_variable (void) const
    {
      return rep->is_variable ();
    }

    bool is_local (void) const { return rep->is_local (); }
    bool is_automatic (void) const { return rep->is_automatic (); }
    bool is_formal (void) const { return rep->is_formal (); }
    bool is_global (void) const { return rep->is_global (); }
    bool is_hidden (void) const { return rep->is_hidden (); }
    bool is_inherited (void) const { return rep->is_inherited (); }
    bool is_persistent (void) const { return rep->is_persistent (); }
    bool is_added_static (void) const { return rep->is_added_static (); }

    void mark_local (void) { rep->mark_local (); }
    void mark_automatic (void) { rep->mark_automatic (); }
    void mark_formal (void) { rep->mark_formal (); }
    void mark_hidden (void) { rep->mark_hidden (); }
    void mark_inherited (void) { rep->mark_inherited (); }
    void mark_global (void) { rep->mark_global (); }
    void mark_persistent (void) { rep->mark_persistent (); }
    void mark_added_static (void) { rep->mark_added_static (); }

    void unmark_local (void) { rep->unmark_local (); }
    void unmark_automatic (void) { rep->unmark_automatic (); }
    void unmark_formal (void) { rep->unmark_formal (); }
    void unmark_hidden (void) { rep->unmark_hidden (); }
    void unmark_inherited (void) { rep->unmark_inherited (); }
    void unmark_global (void) { rep->unmark_global (); }
    void unmark_persistent (void) { rep->unmark_persistent (); }
    void unmark_added_static (void) { rep->unmark_added_static (); }

    void init_persistent (void) { rep->init_persistent (); }

    void erase_persistent (void) { rep->erase_persistent (); }

    void invalidate (void) { rep->invalidate (); }

    scope *decl_scope (void) { return rep->decl_scope (); }

    unsigned int xstorage_class (void) const { return rep->storage_class; }

    void set_curr_fcn (octave_user_function *fcn)
    {
      rep->set_curr_fcn (fcn);
    }

    void bind_fwd_rep (const symbol_record& sr)
    {
      rep->bind_fwd_rep (sr.rep);
    }

    void unbind_fwd_rep (void) { rep->unbind_fwd_rep (); }

    octave_value dump (void) const { return rep->dump (); }

    const symbol_record_rep *xrep (void) const { return rep; }

  private:

    static octave_value dummy_octave_value;

    symbol_record_rep *rep;

    symbol_record (symbol_record_rep *new_rep) : rep (new_rep) { }
  };
}

#endif
