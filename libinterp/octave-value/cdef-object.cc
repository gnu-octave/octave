////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "cdef-class.h"
#include "cdef-object.h"
#include "cdef-property.h"
#include "cdef-utils.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "ov-classdef.h"

// Define to 1 to enable debugging statements.
#define DEBUG_TRACE 0

OCTAVE_BEGIN_NAMESPACE(octave)

void
cdef_object_rep::release (const cdef_object& obj)
{
  // We need to be careful to keep a reference to the object if we are
  // calling the delete method.  The object is passed to the delete
  // method as an argument and if the count is already zero when we
  // do that, then we will increment the count while creating the
  // argument list for the delete method and then it will be decremented
  // back to zero and we'll find ourselves in an infinite loop.

  if (m_count - 1 > static_count ())
    {
      --m_count;
      return;
    }

  if (is_handle_object () && ! is_meta_object ())
    {
      unwind_protect frame;

      // Clear interrupts.
      frame.protect_var (octave_interrupt_state);
      octave_interrupt_state = 0;

      // Disallow quit().
      frame.protect_var (quit_allowed);
      quit_allowed = false;

      interpreter& interp = __get_interpreter__ ();

      interpreter_try (frame);

      try
        {
          // Call classdef "delete()" method on object
          get_class ().delete_object (obj);
        }
      catch (const interrupt_exception&)
        {
          interp.recover_from_exception ();

          warning ("interrupt occurred in handle class delete method");
        }
      catch (const execution_exception& ee)
        {
          interp.recover_from_exception ();

          std::string msg = ee.message ();

          warning ("error caught while executing handle class delete method:\n%s\n",
                   msg.c_str ());
        }
      catch (const exit_exception&)
        {
          // This shouldn't happen since we disabled quit above.
          warning ("exit disabled while executing handle class delete method");
        }
      catch (...) // Yes, the black hole.  We're in a d-tor.
        {
          // This shouldn't happen, in theory.
          warning ("internal error: unhandled exception in handle class delete method");
        }
    }

  // Now it is safe to set the count to zero.
  m_count--;

  destroy ();
}

cdef_class
cdef_object_rep::get_class (void) const
{
  err_invalid_object ("get_class");
}

std::string
cdef_object_rep::class_name (void) const
{
  return get_class ().get_name ();
}

string_vector
cdef_object_rep::map_keys (void) const
{
  cdef_class cls = get_class ();

  if (cls.ok ())
    return cls.get_names ();

  return string_vector ();
}

octave_map
cdef_object::map_value (void) const
{
  octave_map retval;

  warning_with_id ("Octave:classdef-to-struct",
                   "struct: converting a classdef object into a struct "
                   "overrides the access restrictions defined for properties. "
                   "All properties are returned, including private and "
                   "protected ones.");

  cdef_class cls = get_class ();

  if (cls.ok ())
    {
      std::map<std::string, cdef_property> props;

      props = cls.get_property_map (cdef_class::property_all);

      // FIXME: Why not const here?
      for (auto& prop_val : props)
        {
          if (is_array ())
            {
              Array<cdef_object> a_obj = array_value ();

              Cell cvalue (a_obj.dims ());

              for (octave_idx_type i = 0; i < a_obj.numel (); i++)
                cvalue (i) = prop_val.second.get_value (a_obj(i), false);

              retval.setfield (prop_val.first, cvalue);
            }
          else
            {
              Cell cvalue (dim_vector (1, 1),
                           prop_val.second.get_value (*this, false));

              retval.setfield (prop_val.first, cvalue);
            }
        }
    }

  return retval;
}

cdef_class
cdef_object::get_class (void) const
{
  return m_rep->get_class ();
}

cdef_class
cdef_object_base::get_class (void) const
{
  return cdef_class (m_klass);
}

void
cdef_object_base::set_class (const cdef_class& cls)
{
  if ((m_klass.ok () && cls.ok () && cls != get_class ())
      || (m_klass.ok () && ! cls.ok ())
      || (! m_klass.ok () && cls.ok ()))
    {
      m_klass = cls;
    }
}

cdef_object_rep *
cdef_object_base::make_array (void) const
{
  cdef_object_rep *r = new cdef_object_array ();

  r->set_class (get_class ());

  return r;
}

octave_value_list
cdef_object_array::subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            int /* nargout */, std::size_t& skip,
                            const cdef_class& /* context */, bool auto_add)
{
  octave_value_list retval;

  skip = 1;

  switch (type[0])
    {
    case '(':
      {
        const octave_value_list& ival = idx.front ();

        if (ival.empty ())
          {
            m_count++;
            retval(0) = to_ov (cdef_object (this));
            break;
          }

        bool is_scalar = true;
        Array<idx_vector> iv (dim_vector (1, ival.length ()));

        for (int i = 0; i < ival.length (); i++)
          {
            try
              {
                iv(i) = ival(i).index_vector ();
              }
            catch (index_exception& ie)
              {
                // Rethrow to allow more info to be reported later.
                ie.set_pos_if_unset (ival.length (), i+1);
                throw;
              }

            is_scalar = is_scalar && iv(i).is_scalar ();
          }

        Array<cdef_object> ires = m_array.index (iv, auto_add);

        // If resizing is enabled (auto_add = true), it's possible
        // indexing was out-of-bound and the result array contains
        // invalid cdef_objects.

        if (auto_add)
          fill_empty_values (ires);

        if (is_scalar)
          retval(0) = to_ov (ires(0));
        else
          {
            cdef_object array_obj (new cdef_object_array (ires));

            array_obj.set_class (get_class ());

            retval(0) = to_ov (array_obj);
          }
      }
      break;

    case '.':
      if (type.size () == 1 && idx.size () == 1)
        {
          Cell c (dims ());

          octave_idx_type n = m_array.numel ();

          // dummy variables
          std::size_t dummy_skip;
          cdef_class dummy_cls;

          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_value_list r = m_array(i).subsref (type, idx, 1,
                                    dummy_skip,
                                    dummy_cls);

              if (r.length () > 0)
                c(i) = r(0);
            }

          retval(0) = octave_value (c, true);

          break;
        }
      OCTAVE_FALLTHROUGH;

    default:
      error ("can't perform indexing operation on array of %s objects",
             class_name ().c_str ());
      break;
    }

  return retval;
}

octave_value
cdef_object_array::subsasgn (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             const octave_value& rhs)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      if (type.length () == 1)
        {
          cdef_object rhs_obj = to_cdef (rhs);

          if (rhs_obj.get_class () != get_class ())
            error ("can't assign %s object into array of %s objects",
                   rhs_obj.class_name ().c_str (),
                   class_name ().c_str ());

          const octave_value_list& ival = idx.front ();
          bool is_scalar = true;
          Array<idx_vector> iv (dim_vector (1, ival.length ()));

          for (int i = 0; i < ival.length (); i++)
            {
              try
                {
                  iv(i) = ival(i).index_vector ();
                }
              catch (index_exception& ie)
                {
                  ie.set_pos_if_unset (ival.length (), i+1);
                  throw;   // var name set in pt-idx.cc / pt-assign.cc
                }

              is_scalar = is_scalar && iv(i).is_scalar ();
            }

          Array<cdef_object> rhs_mat;

          if (! rhs_obj.is_array ())
            {
              rhs_mat = Array<cdef_object> (dim_vector (1, 1));
              rhs_mat(0) = rhs_obj;
            }
          else
            rhs_mat = rhs_obj.array_value ();

          octave_idx_type n = m_array.numel ();

          m_array.assign (iv, rhs_mat, cdef_object ());

          if (m_array.numel () > n)
            fill_empty_values ();

          m_count++;
          retval = to_ov (cdef_object (this));
        }
      else
        {
          const octave_value_list& ivl = idx.front ();

          // Fill in trailing singleton dimensions so that
          // array.index doesn't create a new blank entry (bug #46660).
          const octave_idx_type one = static_cast<octave_idx_type> (1);
          const octave_value_list& ival = ivl.length () >= 2
                                          ? ivl : ((m_array.dims ()(0) == 1)
                                              ? ovl (one, ivl(0))
                                              : ovl (ivl(0), one));

          bool is_scalar = true;

          Array<idx_vector> iv (dim_vector (1, ival.length ()));

          for (int i = 0; i < ival.length (); i++)
            {
              try
                {
                  iv(i) = ival(i).index_vector ();
                }
              catch (index_exception& ie)
                {
                  // Rethrow to allow more info to be reported later.
                  ie.set_pos_if_unset (ival.length (), i+1);
                  throw;
                }

              is_scalar = is_scalar && iv(i).is_scalar ();

              if (! is_scalar)
                error ("subsasgn: invalid indexing for object array assignment"
                       ", the index must reference a single object in the "
                       "array.");
            }

          Array<cdef_object> a = m_array.index (iv, true);

          if (a.numel () != 1)
            error ("subsasgn: invalid indexing for object array assignment");

          cdef_object obj = a(0);

          int ignore_copies = 0;

          // If the object in 'a' is not valid, this means the index
          // was out-of-bound and we need to create a new object.

          if (! obj.ok ())
            obj = get_class ().construct_object (octave_value_list ());
          else
            // Optimize the subsasgn call to come.  There are 2 copies
            // that we can safely ignore:
            // - 1 in "array"
            // - 1 in "a"
            ignore_copies = 2;

          std::list<octave_value_list> next_idx (idx);

          next_idx.erase (next_idx.begin ());

          octave_value tmp = obj.subsasgn (type.substr (1), next_idx,
                                           rhs, ignore_copies);

          cdef_object robj = to_cdef (tmp);

          if (! robj.ok ()
              || robj.is_array ()
              || robj.get_class () != get_class ())
            error ("subsasgn: invalid assignment into array of %s objects",
                   class_name ().c_str ());

          // Small optimization, when dealing with handle
          // objects, we don't need to re-assign the result
          // of subsasgn back into the array.

          if (! robj.is (a(0)))
            {
              Array<cdef_object> rhs_a (dim_vector (1, 1),
                                        robj);

              octave_idx_type n = m_array.numel ();

              m_array.assign (iv, rhs_a);

              if (m_array.numel () > n)
                fill_empty_values ();
            }

          m_count++;

          retval = to_ov (cdef_object (this));
        }
      break;

    default:
      error ("can't perform indexing operation on array of %s objects",
             class_name ().c_str ());
      break;
    }

  return retval;
}

void
cdef_object_array::fill_empty_values (Array<cdef_object>& arr)
{
  cdef_class cls = get_class ();

  cdef_object obj;

  int n = arr.numel ();

  for (int i = 0; i < n; i++)
    {
      if (! arr.xelem (i).ok ())
        {
          if (! obj.ok ())
            {
              obj = cls.construct_object (octave_value_list ());

              arr.xelem (i) = obj;
            }
          else
            arr.xelem (i) = obj.copy ();
        }
    }
}

void
cdef_object_scalar::break_closure_cycles (const std::shared_ptr<stack_frame>& frame)
{
  for (octave_idx_type i = 0; i < m_map.nfields (); i++)
    m_map.contents(i).break_closure_cycles (frame);
}

octave_value_list
cdef_object_scalar::subsref (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             int nargout, std::size_t& skip,
                             const cdef_class& context, bool auto_add)
{
  skip = 0;

  cdef_class cls = (context.ok () ? context : get_class ());

  octave_value_list retval;

  if (! cls.ok ())
    return retval;

  switch (type[0])
    {
    case '.':
      {
        std::string name = (idx.front ())(0).string_value ();

        cdef_method meth = cls.find_method (name);

        if (meth.ok ())
          {
            int _nargout = (type.length () > 2 ? 1 : nargout);

            octave_value_list args;

            skip = 1;

            if (type.length () > 1 && type[1] == '(')
              {
                auto it = idx.begin ();

                args = *++it;

                skip++;
              }

            if (meth.is_static ())
              retval = meth.execute (args, _nargout, true, "subsref");
            else
              {
                m_count++;
                retval = meth.execute (cdef_object (this), args, _nargout,
                                       true, "subsref");
              }
          }

        if (skip == 0)
          {
            cdef_property prop = cls.find_property (name);

            if (! prop.ok ())
              error ("subsref: unknown method or property: %s", name.c_str ());

            if (prop.is_constant ())
              retval(0) = prop.get_value (true, "subsref");
            else
              {
                m_count++;
                retval(0) = prop.get_value (cdef_object (this),
                                            true, "subsref");
              }

            skip = 1;
          }
        break;
      }

    case '(':
      {
        const octave_value_list& ival = idx.front ();

        m_count++;
        cdef_object this_obj (this);

        if (ival.empty ())
          {
            skip++;
            retval(0) = to_ov (this_obj);
          }
        else
          {
            Array<cdef_object> arr (dim_vector (1, 1), this_obj);

            cdef_object new_obj = cdef_object (new cdef_object_array (arr));

            new_obj.set_class (get_class ());

            retval = new_obj.subsref (type, idx, nargout, skip, cls, auto_add);
          }
      }
      break;

    default:
      error ("object cannot be indexed with '%c'", type[0]);
      break;
    }

  return retval;
}

octave_value
cdef_object_scalar::subsasgn (const std::string& type,
                              const std::list<octave_value_list>& idx,
                              const octave_value& rhs)
{
  octave_value retval;

  cdef_class cls = get_class ();

  switch (type[0])
    {
    case '.':
      {
        std::string name = (idx.front ())(0).string_value ();

        cdef_property prop = cls.find_property (name);

        if (! prop.ok ())
          error ("subsasgn: unknown property: %s", name.c_str ());

        if (prop.is_constant ())
          error ("subsasgn: cannot assign constant property: %s",
                 name.c_str ());

        m_count++;

        cdef_object obj (this);

        if (type.length () == 1)
          {
            prop.set_value (obj, rhs, true, "subsasgn");

            retval = to_ov (obj);
          }
        else
          {
            octave_value val = prop.get_value (obj, true, "subsasgn");

            std::list<octave_value_list> args (idx);

            args.erase (args.begin ());

            val = val.assign (octave_value::op_asn_eq,
                              type.substr (1), args, rhs);

            if (val.class_name () != "object"
                || ! to_cdef (val).is_handle_object ())
              prop.set_value (obj, val, true, "subsasgn");

            retval = to_ov (obj);
          }
      }
      break;

    case '(':
      {
        m_count++;

        cdef_object this_obj (this);

        Array<cdef_object> arr (dim_vector (1, 1), this_obj);

        cdef_object new_obj = cdef_object (new cdef_object_array (arr));

        new_obj.set_class (get_class ());

        octave_value tmp = new_obj.subsasgn (type, idx, rhs);

        retval = tmp;
      }
      break;

    default:
      error ("subsasgn: object cannot be index with '%c'", type[0]);
      break;
    }

  return retval;
}

void
cdef_object_scalar::mark_for_construction (const cdef_class& cls)
{
  std::string cls_name = cls.get_name ();

  Cell supcls = cls.get ("SuperClasses").cell_value ();

  std::list<cdef_class> supcls_list = lookup_classes (supcls);

  m_ctor_list[cls] = supcls_list;
}

bool
cdef_object_scalar::is_constructed_for (const cdef_class& cls) const
{
  return (is_constructed ()
          || m_ctor_list.find (cls) == m_ctor_list.end ());
}

bool
cdef_object_scalar::is_partially_constructed_for (const cdef_class& cls) const
{
  if (is_constructed ())
    return true;

  std::map<cdef_class, std::list<cdef_class>>::const_iterator it
      = m_ctor_list.find (cls);

  if (it == m_ctor_list.end () || it->second.empty ())
    return true;

  for (const auto& cdef_cls : it->second)
    if (! is_partially_constructed_for (cdef_cls))
      return false;

  return true;
}

void
cdef_object_scalar::mark_as_constructed (const cdef_class& cls)
{
  m_ctor_list.erase (cls);
}

handle_cdef_object::~handle_cdef_object (void)
{
#if DEBUG_TRACE
  std::cerr << "deleting " << get_class ().get_name ()
            << " object (handle)" << std::endl;
#endif
}

value_cdef_object::~value_cdef_object (void)
{
#if DEBUG_TRACE
  std::cerr << "deleting " << get_class ().get_name ()
            << " object (value)" << std::endl;
#endif
}

OCTAVE_END_NAMESPACE(octave)
