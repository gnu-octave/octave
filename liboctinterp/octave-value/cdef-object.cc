////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2026 The Octave Project Developers
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

#include <algorithm>
#include <memory>
#include <ranges>

#include "cdef-class.h"
#include "cdef-object.h"
#include "cdef-property.h"
#include "cdef-utils.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "ov-classdef.h"

#define OCTAVE_CDEF_OBJECT_DEBUG 0
#if OCTAVE_CDEF_OBJECT_DEBUG
#  include <iostream>
#endif

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
cdef_object_rep::get_class () const
{
  err_invalid_object ("get_class");
}

std::string
cdef_object_rep::class_name () const
{
  cdef_class cls = get_class ();

  if (cls.ok ())
    return cls.get_name ();

  return "unknown";
}

cdef_object
cdef_object_rep::checkelem (octave_idx_type) const
{
  err_invalid_object ("checkelem");
}

cdef_object
cdef_object_rep::checkelem (octave_idx_type, octave_idx_type) const
{
  err_invalid_object ("checkelem");
}

cdef_object
cdef_object_rep::checkelem (octave_idx_type, octave_idx_type,
                            octave_idx_type) const
{
  err_invalid_object ("checkelem");
}

cdef_object
cdef_object_rep::checkelem (const Array<octave_idx_type>&) const
{
  err_invalid_object ("checkelem");
}

string_vector
cdef_object_rep::map_keys () const
{
  cdef_class cls = get_class ();

  if (cls.ok ())
    return cls.get_names ();

  return string_vector ();
}

octave_map
cdef_object::map_value (bool warn, bool for_save) const
{
  octave_map retval;

  if (warn)
    warning_with_id ("Octave:classdef-to-struct",
                     "struct: converting a classdef object into a struct "
                     "overrides the access restrictions defined for "
                     "properties.  All properties are returned, including "
                     "private and protected ones.");

  cdef_class cls = get_class ();

  if (cls.ok ())
    {
      std::map<property_key, cdef_property> props;

      props = cls.get_property_map (cdef_class::member_mode::all);

      // FIXME: Why not const here?
      for (auto& prop_val : props)
        {
          // Do not include properties that have the "Transient" attribute
          // when creating the map for saving the object to a file.
          if (for_save && prop_val.second.get ("Transient").bool_value ())
            continue;

          if (is_array ())
            {
              Array<cdef_object> a_obj = array_value ();

              Cell cvalue (a_obj.dims ());

              for (octave_idx_type i = 0; i < a_obj.numel (); i++)
                cvalue (i) = prop_val.second.get_value (a_obj(i), false);

              retval.setfield (prop_val.second.get_name (), cvalue);
            }
          else
            {
              Cell cvalue (dim_vector (1, 1),
                           prop_val.second.get_value (*this, false));

              retval.setfield (prop_val.second.get_name (), cvalue);
            }
        }
    }

  return retval;
}

cdef_object::cdef_object (const Array<cdef_object>& arr)
  : m_rep (new cdef_object_array (arr))
{
  maybe_mutate ();
}

cdef_object::cdef_object (const Array<cdef_object>& arr, const cdef_class& cls)
  : m_rep (new cdef_object_array (arr, cls))
{
  maybe_mutate ();
}

void
cdef_object::maybe_mutate ()
{
  cdef_object tmp = m_rep->try_narrowing_conversion ();

  if (tmp.ok () && tmp.m_rep != m_rep)
    *this = tmp;
}

cdef_class
cdef_object::get_class () const
{
  return m_rep->get_class ();
}

cdef_class
cdef_object_base::get_class () const
{
  if (! m_klass.ok ())
    return cdef_class ();

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
cdef_object_base::make_array () const
{
  cdef_object_rep *r = new cdef_object_array ();

  r->set_class (get_class ());

  return r;
}

cdef_object
cdef_object_base::checkelem (octave_idx_type n) const
{
  if (n != 0)
    error ("index out of range for scalar classdef object");

  return wrap ();
}

cdef_object_rep *
cdef_object_array::clone () const
{
  Array<cdef_object> new_array (m_array.dims ());

  for (octave_idx_type i = 0; i < m_array.numel (); i++)
    new_array(i) = m_array(i).clone ();

  return new cdef_object_array (new_array, get_class ());
}

cdef_object
cdef_object_array::try_narrowing_conversion ()
{
  // A 1x1 object array is equivalent to the scalar object it contains.
  // Return the element itself, not a copy, so that handle identity is
  // preserved.
  if (m_array.numel () == 1 && m_array(0).ok ())
    return m_array(0);

  return cdef_object ();
}

cdef_object_rep *
cdef_object_array::permute (const Array<int>& vec, bool inv) const
{
  Array<cdef_object> cpy_array = m_array.permute (vec, inv);

  return new cdef_object_array (cpy_array, get_class ());
}

cdef_object_rep *
cdef_object_array::resize (const dim_vector& dv, bool fill) const
{
  cdef_object_array *retval = dynamic_cast<cdef_object_array *> (clone ());
  std::unique_ptr<cdef_object_array> guard (retval);

  retval->m_array.resize (dv, cdef_object ());

  if (fill)
    retval->fill_empty_values ();

  return guard.release ();
}

static cdef_class
derive_array_class (const Array<cdef_object>& a)
{
  cdef_class cls;

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      if (! a(i).ok ())
        continue;

      cdef_class elt_cls = a(i).get_class ();

      if (! cls.ok ())
        cls = elt_cls;
      else if (elt_cls.ok () && elt_cls != cls)
        error ("cannot create classdef array with elements of different "
               "classes ('%s' and '%s')",
               cls.get_name ().c_str (), elt_cls.get_name ().c_str ());
    }

  return cls;
}

cdef_object_array::cdef_object_array (const Array<cdef_object>& a)
  : cdef_object_base (), m_array (a)
{
  cdef_class cls = derive_array_class (m_array);

  if (! cls.ok () && m_array.numel () > 0)
    error ("cannot create a non-empty classdef array with no class");

  if (cls.ok ())
    set_class (cls);
}

cdef_object_array::cdef_object_array (const Array<cdef_object>& a,
                                      const cdef_class& cls)
  : cdef_object_base (), m_array (a)
{
  cdef_class elt_cls = derive_array_class (m_array);

  if (elt_cls.ok () && cls.ok () && elt_cls != cls)
    error ("cannot create classdef array of class '%s' holding elements of "
           "class '%s'",
           cls.get_name ().c_str (), elt_cls.get_name ().c_str ());

  set_class (cls);
}

cdef_object_rep *
cdef_object_array::transpose () const
{
  if (m_array.ndims () > 2)
    error ("transpose not defined for N-D objects");

  auto perm_vec = Array<int> (std::vector<int> {1, 0}, dim_vector (1, 2));
  return permute (perm_vec);
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
            retval(0) = to_ov (wrap ());
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
            cdef_object array_obj (ires, get_class ());

            retval(0) = to_ov (array_obj);
          }
      }
      break;

    case '.':
      if (m_array.numel () == 1)
        {
          // If there is only one element in the array, implicitly index the
          // first element.  In this case, also allow indexing with more than
          // one level.

          // dummy variables
          std::size_t dummy_skip;
          cdef_class dummy_cls;
          retval = m_array(0).subsref (type, idx, 1, dummy_skip, dummy_cls);

          break;
        }
      else if (type.size () == 1 && idx.size () == 1)
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

// Assign the values of the comma-separated list RHS one by one to the
// objects in ELTS, as in "[m.A] = c{:}".  Each object is assigned through
// its own subsasgn with TYPE and IDX, so that access checks and set
// methods apply to every element.  ELTS holds copies of the elements of
// the array; an invalid object marks an index past its end, for which a
// new element of class CLS is constructed.

static void
assign_cs_list (Array<cdef_object>& elts, const std::string& type,
                const std::list<octave_value_list>& idx,
                const octave_value& rhs, cdef_class cls)
{
  octave_value_list vals = rhs.list_value ();

  octave_idx_type n = elts.numel ();

  if (vals.length () != n)
    error ("subsasgn: %" OCTAVE_IDX_TYPE_FORMAT " values for %"
           OCTAVE_IDX_TYPE_FORMAT " elements of an array of %s objects",
           vals.length (), n, cls.get_name ().c_str ());

  for (octave_idx_type i = 0; i < n; i++)
    {
      cdef_object obj = elts(i);

      int ignore_copies = 0;

      if (! obj.ok ())
        obj = cls.construct_array_element ();
      else
        {
          // The copies in the array and in ELTS can be ignored, as in the
          // assignment to a single indexed element below.
          ignore_copies = 2;
        }

      octave_value tmp = obj.subsasgn (type, idx, vals(i), ignore_copies);

      cdef_object robj = to_cdef (tmp);

      if (! robj.ok () || robj.is_array () || robj.get_class () != cls)
        error ("subsasgn: invalid assignment into array of %s objects",
               cls.get_name ().c_str ());

      elts(i) = robj;
    }
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

          if (rhs.isempty ())
            m_array.delete_elements (iv);
          else
            {
              cdef_object rhs_obj = to_cdef (rhs);

              if (rhs_obj.get_class () != get_class ())
                error ("can't assign %s object into array of %s objects",
                       rhs_obj.class_name ().c_str (),
                       class_name ().c_str ());

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
            }

          retval = to_ov (wrap ());
        }
      else
        {
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
                  // Rethrow to allow more info to be reported later.
                  ie.set_pos_if_unset (ival.length (), i+1);
                  throw;
                }

              is_scalar = is_scalar && iv(i).is_scalar ();
            }

          std::list<octave_value_list> next_idx (std::next (idx.begin ()),
                                                 idx.end ());

          if (! is_scalar)
            {
              // Only a comma-separated list may be assigned to a property
              // of several elements, one value to each, as in
              // "[m(2:3).A] = deal (7, 8)".
              if (type.length () != 2 || type[1] != '.'
                  || ! rhs.is_cs_list ())
                error ("subsasgn: invalid indexing for object array assignment,"
                       " the index must reference a single object in the array.");

              Array<cdef_object> elts = m_array.index (iv, true);

              assign_cs_list (elts, type.substr (1), next_idx, rhs,
                              get_class ());

              octave_idx_type n = m_array.numel ();

              m_array.assign (iv, elts);

              if (m_array.numel () > n)
                fill_empty_values ();

              retval = to_ov (wrap ());

              break;
            }

          Array<cdef_object> a = m_array.index (iv, true);

          if (a.numel () != 1)
            error ("subsasgn: invalid indexing for object array assignment");

          cdef_object obj = a(0);

          int ignore_copies = 0;

          // If the object in 'a' is not valid, this means the index
          // was out-of-bound and we need to create a new object.

          if (! obj.ok ())
            obj = get_class ().construct_array_element ();
          else
            // Optimize the subsasgn call to come.  There are 2 copies
            // that we can safely ignore:
            // - 1 in "array"
            // - 1 in "a"
            ignore_copies = 2;

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

          retval = to_ov (wrap ());
        }
      break;

    case '.':
      if (m_array.numel () == 1)
        {
          // If there is only one element in the array, implicitly index the
          // first element.

          retval = m_array(0).subsasgn (type, idx, rhs);

          break;
        }
      else if (type.length () == 1 && rhs.is_cs_list ())
        {
          // A comma-separated list assigns one value to the property of
          // each element, as in "[m.A] = c{:}".

          const Array<cdef_object>& arr = m_array;

          Array<cdef_object> elts (arr.dims ());

          for (octave_idx_type i = 0; i < arr.numel (); i++)
            elts.xelem (i) = arr(i);

          assign_cs_list (elts, type, idx, rhs, get_class ());

          m_array = elts;

          retval = to_ov (wrap ());

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
cdef_object_array::reshape (const dim_vector& new_dims) const
{
  return to_ov (cdef_object (Array<cdef_object> (m_array, new_dims), get_class ()));
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
              obj = cls.construct_array_element ();

              arr.xelem (i) = obj;
            }
          else
            arr.xelem (i) = obj.copy ();
        }
    }
}

std::size_t
cdef_object_array::byte_size () const
{
  std::size_t retval = 0;

  for (octave_idx_type i = 0; i < m_array.numel (); i++)
    retval += m_array.xelem (i).byte_size ();

  return retval;
}

void
cdef_object_array::break_closure_cycles (const std::shared_ptr<stack_frame>& frame)
{
  for (octave_idx_type i = 0; i < m_array.numel (); i++)
    m_array(i).break_closure_cycles (frame);
}

void
cdef_object_array::mark_as_constructed ()
{
  for (octave_idx_type i = 0; i < m_array.numel (); i++)
    m_array(i).mark_as_constructed ();
}

void
cdef_object_array::mark_as_constructed (const cdef_class& cls)
{
  for (octave_idx_type i = 0; i < m_array.numel (); i++)
    m_array(i).mark_as_constructed (cls);
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

        cdef_property prop = cls.find_property (name);

        // Dot syntax reaches a property before a method of the same name,
        // unless the property is private to a class that this context cannot
        // see into, as in Matlab.
        if (prop.ok () && cls.find_method (name).ok ())
          {
            octave_value acc = prop.get ("GetAccess");

            if (acc.is_string () && acc.string_value () == "private"
                && ! prop.check_get_access ())
              prop = cdef_property ();
          }

        if (prop.ok ())
          {
            if (prop.is_constant ())
              retval(0) = prop.get_value (true, "subsref");
            else
              {
                retval(0) = prop.get_value (wrap (), true, "subsref");
              }

            skip = 1;
          }

        if (skip == 0)
          {
            cdef_method meth = cls.find_method (name);

            if (! meth.ok ())
              error ("subsref: unknown method or property: %s", name.c_str ());

            // If the method call is followed by another index operation,
            // the number of outputs of the call will be 1.
            int nargout_mtd = (type.length () > 2
                               || (type.length () == 2 && type[1] != '(')
                               ? 1 : nargout);

            octave_value_list args;

            skip = 1;

            if (type.length () > 1 && type[1] == '(')
              {
                auto it = idx.begin ();

                args = *++it;

                skip++;
              }

            if (meth.is_static ())
              retval = meth.execute (args, nargout_mtd, true, "subsref");
            else
              {
                retval = meth.execute (wrap (), args, nargout_mtd,
                                       true, "subsref");
              }
          }

        break;
      }

    case '(':
      {
        const octave_value_list& ival = idx.front ();

        cdef_object this_obj = wrap ();

        if (ival.empty ())
          {
            skip++;
            retval(0) = to_ov (this_obj);
          }
        else
          {
            Array<cdef_object> arr (dim_vector (1, 1), this_obj);

            cdef_object new_obj = adopt (new cdef_object_array (arr));

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

// Return true if an assignment to VAL with the index chain TYPE and IDX
// reaches a handle object, directly or through structures, cell arrays, and
// arrays, and assigns to one of its properties.  Such an assignment changes
// the handle object and leaves VAL itself unchanged.

static bool
assigns_to_handle (octave_value val, const std::string& type,
                   const std::list<octave_value_list>& idx)
{
  auto it = idx.begin ();

  for (std::size_t i = 0; i < type.length (); i++, it++)
    {
      if (val.is_classdef_object ())
        {
          if (! to_cdef (val).get_class ().is_handle_class ())
            return false;

          if (type[i] == '.')
            return true;
        }
      else if (val.isobject () || val.isjava ())
        return false;

      if (i == type.length () - 1)
        return false;

      try
        {
          val = val.subsref (type.substr (i, 1),
                             std::list<octave_value_list> (1, *it));
        }
      catch (const execution_exception&)
        {
          __get_interpreter__ ().recover_from_exception ();

          return false;
        }

      if (val.is_cs_list ())
        return false;
    }

  return false;
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

        // A constant property is never assigned, but a handle object that
        // it holds may be changed, as in Matlab.
        bool is_constant = prop.is_constant ();

        if (is_constant && type.length () == 1)
          error ("subsasgn: cannot assign constant property: %s",
                 name.c_str ());

        cdef_object obj = wrap ();

        if (type.length () == 1)
          {
            prop.set_value (obj, rhs, true, "subsasgn");

            retval = to_ov (obj);
          }
        else
          {
            octave_value val
              = (is_constant ? prop.get_value (true, "subsasgn")
                             : prop.get_value (obj, true, "subsasgn"));

            cdef_object previous;
            if (val.is_classdef_object ())
              previous = to_cdef (val);

            std::list<octave_value_list> args (std::next (idx.begin ()),
                                               idx.end ());

            // The assignment may change a handle object that a constant
            // property holds, but never the constant property itself.
            if (is_constant)
              {
                if (! assigns_to_handle (val, type.substr (1), args))
                  error ("subsasgn: cannot assign constant property: %s",
                         name.c_str ());

                val.assign (octave_value::op_asn_eq, type.substr (1), args,
                            rhs);

                retval = to_ov (obj);

                break;
              }

            // A property without get and set methods holds the value that is
            // assigned to.  Take the value out of the object while it is
            // changed, so that changing part of it does not copy all of it.
            // Do so only for one level of indexing with neither the value nor
            // the right-hand side an object, so that no code can run and see
            // the property empty, and only when storing the result cannot
            // fail.
            octave_value get_fcn = prop.get ("GetMethod");
            octave_value set_fcn = prop.get ("SetMethod");
            octave_value set_access = prop.get ("SetAccess");
            cdef_class defining_class = prop.get_defining_class ();

            bool take_value
              = (! is_constant && type.length () == 2
                 && ! val.isobject () && ! val.isjava ()
                 && ! rhs.isobject () && ! rhs.isjava ()
                 && ! prop.get ("Dependent").bool_value ()
                 && ! get_fcn.is_function () && ! get_fcn.is_function_handle ()
                 && ! set_fcn.is_function () && ! set_fcn.is_function_handle ()
                 && prop.check_set_access ()
                 && ! (set_access.is_string ()
                       && set_access.string_value () == "immutable"
                       && obj.is_constructed_for (defining_class))
                 && (obj.is_constructed ()
                     || obj.is_partially_constructed_for (defining_class)));

            if (take_value)
              {
                put (name, octave_value ());

                try
                  {
                    val.assign (octave_value::op_asn_eq,
                                type.substr (1), args, rhs);
                  }
                catch (...)
                  {
                    put (name, val);
                    throw;
                  }
              }
            else
              val = val.assign (octave_value::op_asn_eq,
                                type.substr (1), args, rhs);

            // Changing a referenced handle's properties does not assign
            // this property.  Replacing a handle or an array element does.
            bool same_handles = false;
            if (type.find ('.', 1) != std::string::npos
                && previous.ok () && previous.get_class ().is_handle_class ()
                && val.is_classdef_object ())
              {
                cdef_object current = to_cdef (val);
                const auto before = previous.array_value ();
                const auto after = current.array_value ();

                same_handles = (previous.get_class () == current.get_class ()
                                && before.dims () == after.dims ()
                                && std::ranges::all_of
                                  (std::views::iota (octave_idx_type (0),
                                                     before.numel ()),
                                   [&before, &after] (octave_idx_type i)
                                   { return before(i).is (after(i)); }));
              }

            if (! same_handles)
              prop.set_value (obj, val, true, "subsasgn");

            retval = to_ov (obj);
          }
      }
      break;

    case '(':
      {
        cdef_object this_obj = wrap ();

        Array<cdef_object> arr (dim_vector (1, 1), this_obj);

        cdef_object new_obj = adopt (new cdef_object_array (arr));

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

octave_value
cdef_object_scalar::reshape (const dim_vector& new_dims) const
{
  if (new_dims.numel () != 1)
    {
      std::string new_dims_str = new_dims.str ();
      error ("reshape: cannot reshape scalar classdef object to %s array",
             new_dims_str.c_str ());
    }

  return to_ov (adopt (clone ()));
}

cdef_object_rep *
cdef_object_scalar::resize (const dim_vector& dv, bool fill) const
{
  if (dv.numel () == 1)
    return clone ();

  Array<cdef_object> arr (dv);

  if (dv.numel () > 0)
    arr(0) = adopt (clone ());

  cdef_object_array *retval = new cdef_object_array (arr, get_class ());
  std::unique_ptr<cdef_object_array> guard (retval);

  if (fill)
    retval->fill_empty_values ();

  return guard.release ();
}

void
cdef_object_scalar::mark_for_construction (const cdef_class& cls)
{
  cdef_object supcls = cls.get_superclass_list ();

  std::list<cdef_class> supcls_list;

  for (octave_idx_type i = 0; i < supcls.numel (); i++)
    supcls_list.push_back (cdef_class (supcls (i)));

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

std::uint64_t handle_cdef_object::s_creation_count = 0;

handle_cdef_object::~handle_cdef_object ()
{
#if OCTAVE_CDEF_OBJECT_DEBUG
  std::cerr << "deleting " << get_class ().get_name ()
            << " object (handle)" << std::endl;
#endif
}

value_cdef_object::~value_cdef_object ()
{
#if OCTAVE_CDEF_OBJECT_DEBUG
  std::cerr << "deleting " << get_class ().get_name ()
            << " object (value)" << std::endl;
#endif
}

OCTAVE_END_NAMESPACE(octave)
