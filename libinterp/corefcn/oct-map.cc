////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1995-2023 The Octave Project Developers
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

#include "Array-util.h"
#include "error.h"
#include "oct-locbuf.h"
#include "str-vec.h"

#include "oct-map.h"
#include "utils.h"

octave_fields::fields_rep *
octave_fields::nil_rep (void)
{
  static fields_rep nr;
  return &nr;
}

octave_fields::octave_fields (const string_vector& fields)
  : m_rep (new fields_rep)
{
  octave_idx_type n = fields.numel ();
  for (octave_idx_type i = 0; i < n; i++)
    (*m_rep)[fields(i)] = i;
}

octave_fields::octave_fields (const char *const *fields)
  : m_rep (new fields_rep)
{
  octave_idx_type n = 0;
  while (*fields)
    (*m_rep)[std::string (*fields++)] = n++;
}

bool
octave_fields::isfield (const std::string& field) const
{
  return m_rep->find (field) != m_rep->end ();
}

octave_idx_type
octave_fields::getfield (const std::string& field) const
{
  auto p = m_rep->find (field);
  return (p != m_rep->end ()) ? p->second : -1;
}

octave_idx_type
octave_fields::getfield (const std::string& field)
{
  auto p = m_rep->find (field);
  if (p != m_rep->end ())
    return p->second;
  else
    {
      make_unique ();
      octave_idx_type n = m_rep->size ();
      return (*m_rep)[field] = n;
    }
}

octave_idx_type
octave_fields::rmfield (const std::string& field)
{
  auto p = m_rep->find (field);
  if (p == m_rep->end ())
    return -1;
  else
    {
      octave_idx_type n = p->second;
      make_unique ();
      m_rep->erase (field);
      for (auto& fld_idx : *m_rep)
        {
          if (fld_idx.second >= n)
            fld_idx.second--;
        }

      return n;
    }
}

void
octave_fields::orderfields (Array<octave_idx_type>& perm)
{
  octave_idx_type n = m_rep->size ();
  perm.clear (n, 1);

  make_unique ();
  octave_idx_type i = 0;
  for (auto& fld_idx : *m_rep)
    {
      octave_idx_type j = fld_idx.second;
      fld_idx.second = i;
      perm(i++) = j;
    }
}

bool
octave_fields::equal_up_to_order (const octave_fields& other,
                                  octave_idx_type *perm) const
{
  bool retval;

  auto p = begin ();
  auto q = other.begin ();
  for (; p != end () && q != other.end (); p++, q++)
    {
      if (p->first == q->first)
        perm[p->second] = q->second;
      else
        return false;
    }

  retval = (p == end () && q == other.end ());

  return retval;
}

bool
octave_fields::equal_up_to_order (const octave_fields& other,
                                  Array<octave_idx_type>& perm) const
{
  octave_idx_type n = nfields ();
  if (perm.numel () != n)
    perm.clear (1, n);

  return equal_up_to_order (other, perm.fortran_vec ());
}

string_vector
octave_fields::fieldnames (void) const
{
  octave_idx_type n = nfields ();
  string_vector retval(n);

  for (auto& fld_idx : *this)
    retval.xelem (fld_idx.second) = fld_idx.first;

  return retval;
}

octave_scalar_map::octave_scalar_map
(const std::map<std::string, octave_value>& m)
{
  std::size_t sz = m.size ();
  m_vals.resize (sz);
  std::size_t i = 0;
  for (const auto& k_v : m)
    {
      m_keys.getfield (k_v.first);
      m_vals[i++] = k_v.second;
    }
}

octave_value
octave_scalar_map::getfield (const std::string& k) const
{
  octave_idx_type idx = m_keys.getfield (k);
  return (idx >= 0) ? m_vals[idx] : octave_value ();
}

void
octave_scalar_map::setfield (const std::string& k, const octave_value& val)
{
  octave_idx_type idx = m_keys.getfield (k);
  if (idx < static_cast<octave_idx_type> (m_vals.size ()))
    m_vals[idx] = val;
  else
    m_vals.push_back (val);
}

void
octave_scalar_map::rmfield (const std::string& k)
{
  octave_idx_type idx = m_keys.rmfield (k);
  if (idx >= 0)
    m_vals.erase (m_vals.begin () + idx);
}

octave_scalar_map
octave_scalar_map::orderfields (void) const
{
  Array<octave_idx_type> perm;
  return orderfields (perm);
}

octave_scalar_map
octave_scalar_map::orderfields (Array<octave_idx_type>& perm) const
{
  octave_scalar_map retval (m_keys);
  retval.m_keys.orderfields (perm);

  octave_idx_type nf = nfields ();
  for (octave_idx_type i = 0; i < nf; i++)
    retval.m_vals[i] = m_vals[perm.xelem (i)];

  return retval;
}

octave_scalar_map
octave_scalar_map::orderfields (const octave_scalar_map& other,
                                Array<octave_idx_type>& perm) const
{
  if (m_keys.is_same (other.m_keys))
    return *this;
  else
    {
      octave_scalar_map retval (other.m_keys);
      if (! other.m_keys.equal_up_to_order (m_keys, perm))
        error ("orderfields: structs must have same fields up to order");

      octave_idx_type nf = nfields ();
      for (octave_idx_type i = 0; i < nf; i++)
        retval.m_vals[i] = m_vals[perm.xelem (i)];

      return retval;
    }
}

octave_value
octave_scalar_map::contents (const std::string& k) const
{
  return getfield (k);
}

octave_value&
octave_scalar_map::contents (const std::string& k)
{
  octave_idx_type idx = m_keys.getfield (k);
  if (idx >= static_cast<octave_idx_type> (m_vals.size ()))
    m_vals.resize (idx+1);
  return m_vals[idx];
}

octave_map::octave_map (const octave_scalar_map& m)
  : m_keys (m.m_keys), m_vals (), m_dimensions (1, 1)
{
  octave_idx_type nf = m.nfields ();
  m_vals.reserve (nf);
  for (octave_idx_type i = 0; i < nf; i++)
    {
      m_vals.push_back (Cell (m_dimensions));
      m_vals[i].xelem (0) = m.m_vals[i];
    }
}

Cell
octave_map::getfield (const std::string& k) const
{
  octave_idx_type idx = m_keys.getfield (k);
  return (idx >= 0) ? m_vals[idx] : Cell ();
}

void
octave_map::setfield (const std::string& k, const Cell& val)
{
  if (nfields () == 0)
    m_dimensions = val.dims ();

  if (val.dims () != m_dimensions)
    error ("octave_map::setfield: internal error");

  octave_idx_type idx = m_keys.getfield (k);
  if (idx < static_cast<octave_idx_type> (m_vals.size ()))
    m_vals[idx] = val;
  else
    m_vals.push_back (val);
}

void
octave_map::rmfield (const std::string& k)
{
  octave_idx_type idx = m_keys.rmfield (k);
  if (idx >= 0)
    m_vals.erase (m_vals.begin () + idx);
}

octave_map
octave_map::orderfields (void) const
{
  Array<octave_idx_type> perm;
  return orderfields (perm);
}

octave_map
octave_map::orderfields (Array<octave_idx_type>& perm) const
{
  octave_map retval (m_keys);
  retval.m_keys.orderfields (perm);

  octave_idx_type nf = nfields ();
  for (octave_idx_type i = 0; i < nf; i++)
    retval.m_vals[i] = m_vals[perm.xelem (i)];

  return retval;
}

octave_map
octave_map::orderfields (const octave_map& other,
                         Array<octave_idx_type>& perm) const
{
  if (m_keys.is_same (other.m_keys))
    return *this;
  else
    {
      octave_map retval (other.m_keys);
      if (! other.m_keys.equal_up_to_order (m_keys, perm))
        error ("orderfields: structs must have same fields up to order");

      octave_idx_type nf = nfields ();
      for (octave_idx_type i = 0; i < nf; i++)
        retval.m_vals[i] = m_vals[perm.xelem (i)];

      return retval;
    }
}

Cell
octave_map::contents (const std::string& k) const
{
  return getfield (k);
}

Cell&
octave_map::contents (const std::string& k)
{
  octave_idx_type idx = m_keys.getfield (k);
  if (idx >= static_cast<octave_idx_type> (m_vals.size ()))
    m_vals.push_back (Cell (m_dimensions)); // auto-set correct dims.
  return m_vals[idx];
}

void
octave_map::extract_scalar (octave_scalar_map& dest,
                            octave_idx_type idx) const
{
  octave_idx_type nf = nfields ();
  for (octave_idx_type i = 0; i < nf; i++)
    dest.m_vals[i] = m_vals[i](idx);
}

octave_scalar_map
octave_map::elem (octave_idx_type n) const
{
  octave_scalar_map retval (m_keys);

  // Optimize this so that there is just one check.
  extract_scalar (retval, compute_index (n, m_dimensions));

  return retval;
}

octave_scalar_map
octave_map::elem (octave_idx_type i, octave_idx_type j) const
{
  octave_scalar_map retval (m_keys);

  // Optimize this so that there is just one check.
  extract_scalar (retval, compute_index (i, j, m_dimensions));

  return retval;
}

octave_scalar_map
octave_map::elem (const Array<octave_idx_type>& ra_idx) const
{
  octave_scalar_map retval (m_keys);

  // Optimize this so that there is just one check.
  extract_scalar (retval, compute_index (ra_idx, m_dimensions));

  return retval;
}

octave_scalar_map
octave_map::fast_elem_extract (octave_idx_type n) const
{
  octave_scalar_map retval (m_keys);

  extract_scalar (retval, n);

  return retval;
}

bool
octave_map::fast_elem_insert (octave_idx_type n,
                              const octave_scalar_map& rhs)
{
  bool retval = false;

  octave_idx_type nf = nfields ();
  if (rhs.m_keys.is_same (m_keys))
    {
      for (octave_idx_type i = 0; i < nf; i++)
        m_vals[i](n) = rhs.m_vals[i];

      retval = true;
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (octave_idx_type, perm, nf);
      if (m_keys.equal_up_to_order (rhs.m_keys, perm))
        {
          for (octave_idx_type i = 0; i < nf; i++)
            m_vals[i](n) = rhs.m_vals[perm[i]];

          retval = true;
        }
    }

  return retval;
}

octave_map
octave_map::squeeze (void) const
{
  octave_map retval (*this);
  octave_idx_type nf = nfields ();

  retval.m_dimensions = m_dimensions.squeeze ();

  for (octave_idx_type i = 0; i < nf; i++)
    retval.m_vals[i] = m_vals[i].squeeze ();

  retval.optimize_dimensions ();

  return retval;
}

/*
## test preservation of m_keys by squeeze
%!test
%! x(1,1,1,1).d = 10;  x(3,5,1,7).a = "b";  x(2,4,1,7).f = 27;
%! assert (fieldnames (squeeze (x)), {"d"; "a"; "f"});
*/

octave_map
octave_map::permute (const Array<int>& vec, bool inv) const
{
  octave_map retval (m_keys);
  octave_idx_type nf = nfields ();

  for (octave_idx_type i = 0; i < nf; i++)
    retval.m_vals[i] = m_vals[i].permute (vec, inv);

  // FIXME:
  // There is no dim_vector::permute for technical reasons.
  // We pick the dim vector from results if possible, otherwise use a dummy
  // array to get it.  Need (?) a better solution to this problem.
  if (nf > 0)
    retval.m_dimensions = retval.m_vals[0].dims ();
  else
    {
      Array<char> dummy (m_dimensions);
      dummy = dummy.permute (vec, inv);
      retval.m_dimensions = dummy.dims ();
    }

  retval.optimize_dimensions ();

  return retval;
}

/*
## test preservation of key order by permute
%!test
%! x(1,1,1,1).d = 10;  x(3,5,1,7).a = "b";  x(2,4,1,7).f = 27;
%! assert (fieldnames (permute (x, [3, 4, 1, 2])), {"d"; "a"; "f"});
*/

octave_map
octave_map::transpose (void) const
{
  octave_map retval (m_keys);

  retval.m_dimensions = dim_vector (m_dimensions (1), m_dimensions (0));

  octave_idx_type nf = nfields ();
  for (octave_idx_type i = 0; i < nf; i++)
    retval.m_vals[i] = m_vals[i].transpose ();

  retval.optimize_dimensions ();

  return retval;
}

/*
## test preservation of key order by transpose
%!test
%! x(1,1).d = 10;  x(3,5).a = "b";  x(2,4).f = 27;
%! assert (fieldnames (transpose (x)), {"d"; "a"; "f"});
%! assert (fieldnames (x'), {"d"; "a"; "f"});
%! assert (fieldnames (x.'), {"d"; "a"; "f"});
*/

octave_map
octave_map::reshape (const dim_vector& dv) const
{
  octave_map retval (m_keys);
  retval.m_dimensions = dv;

  // When reshaping m_vals the Array constructor chops trailing singletons,
  // hence we need to do the same for the whole map.
  retval.m_dimensions.chop_trailing_singletons ();

  octave_idx_type nf = nfields ();
  if (nf > 0)
    {
      retval.m_vals.reserve (nf);
      for (octave_idx_type i = 0; i < nf; i++)
        retval.m_vals[i] = m_vals[i].reshape (dv);
    }
  else
    {
      // FIXME: Do it with a dummy array, to reuse error message.
      // Need (?) a better solution.
      Array<char> dummy (m_dimensions);
      dummy.reshape (dv);
    }

  retval.optimize_dimensions ();

  return retval;
}

/*
## test preservation of key order by reshape
%!test
%! x(1,1).d = 10;  x(4,6).a = "b";  x(2,4).f = 27;
%! assert (fieldnames (reshape (x, 3, 8)), {"d"; "a"; "f"});

## test chopping of trailing singletons
%!test <*51634>
%! x(1,1).d = 10;  x(4,6).a = "b";  x(2,4).f = 27;
%! reshape (x, 3, 8, 1, 1);

%!test <*46385>
%! M = repmat (struct ('a', ones (100), 'b', true), 1, 2);
%! M = repmat (M, 1, 2);
%! assert (size (M), [1, 4]);

libinterp/corefcn/oct-map.cc

*/

void
octave_map::resize (const dim_vector& dv, bool fill)
{
  octave_idx_type nf = nfields ();
  if (nf > 0)
    {
      for (octave_idx_type i = 0; i < nf; i++)
        {
          if (fill)
            m_vals[i].resize (dv, Matrix ());
          else
            m_vals[i].resize (dv);
        }
    }
  else
    {
      // FIXME: Do it with a dummy array, to reuse error message.
      // Need (?) a better solution.
      Array<char> dummy (m_dimensions);
      dummy.resize (dv);
    }

  m_dimensions = dv;
  optimize_dimensions ();
}

void
octave_map::do_cat (int dim, octave_idx_type n,
                    const octave_scalar_map *map_list,
                    octave_map& retval)
{
  octave_idx_type nf = retval.nfields ();
  retval.m_vals.reserve (nf);

  dim_vector& rd = retval.m_dimensions;
  rd.resize (dim+1, 1);
  rd(0) = rd(1) = 1;
  rd(dim) = n;

  for (octave_idx_type j = 0; j < nf; j++)
    {
      retval.m_vals.push_back (Cell (rd));
      error_unless (retval.m_vals[j].numel () == n);
      for (octave_idx_type i = 0; i < n; i++)
        retval.m_vals[j].xelem (i) = map_list[i].m_vals[j];
    }
}

void
octave_map::do_cat (int dim, octave_idx_type n, const octave_map *map_list,
                    octave_map& retval)
{
  octave_idx_type nf = retval.nfields ();
  retval.m_vals.reserve (nf);

  OCTAVE_LOCAL_BUFFER (Array<octave_value>, field_list, n);

  for (octave_idx_type j = 0; j < nf; j++)
    {
      for (octave_idx_type i = 0; i < n; i++)
        field_list[i] = map_list[i].m_vals[j];

      retval.m_vals.push_back (Array<octave_value>::cat (dim, n, field_list));
      if (j == 0)
        retval.m_dimensions = retval.m_vals[j].dims ();
    }
}

// This is just a wrapper.
void permute_to_correct_order1 (const octave_scalar_map& ref,
                                const octave_scalar_map& src,
                                octave_scalar_map& dest,
                                Array<octave_idx_type>& perm)
{
  dest = src.orderfields (ref, perm);
}

// In non-scalar case, we also promote empty structs without fields.
void permute_to_correct_order1 (const octave_map& ref, const octave_map& src,
                                octave_map& dest, Array<octave_idx_type>& perm)
{
  if (src.nfields () == 0 && src.isempty ())
    dest = octave_map (src.dims (), ref.keys ());
  else
    dest = src.orderfields (ref, perm);
}

template <typename map>
static void
permute_to_correct_order (octave_idx_type n, octave_idx_type nf,
                          octave_idx_type idx, const map *map_list,
                          map *new_map_list)
{
  new_map_list[idx] = map_list[idx];

  Array<octave_idx_type> perm (dim_vector (1, nf));

  try
    {
      for (octave_idx_type i = 0; i < n; i++)
        {
          if (i == idx)
            continue;

          permute_to_correct_order1 (map_list[idx], map_list[i],
                                     new_map_list[i], perm);
        }
    }
  catch (octave::execution_exception& ee)
    {
      error (ee, "cat: field names mismatch in concatenating structs");
    }
}

octave_map
octave_map::cat (int dim, octave_idx_type n, const octave_scalar_map *map_list)
{
  octave_map retval;

  // Allow dim = -1, -2 for compatibility, though it makes no difference here.
  if (dim == -1 || dim == -2)
    dim = -dim - 1;
  else if (dim < 0)
    error ("cat: invalid dimension");

  if (n == 1)
    retval = map_list[0];
  else if (n > 1)
    {
      octave_idx_type idx, nf = 0;
      for (idx = 0; idx < n; idx++)
        {
          nf = map_list[idx].nfields ();
          if (nf > 0)
            {
              retval.m_keys = map_list[idx].m_keys;
              break;
            }
        }

      if (nf > 0)
        {
          // Try the fast case.
          bool all_same = true;
          for (octave_idx_type i = 0; i < n; i++)
            {
              all_same = map_list[idx].m_keys.is_same (map_list[i].m_keys);
              if (! all_same)
                break;
            }

          if (all_same)
            do_cat (dim, n, map_list, retval);
          else
            {
              // permute all structures to common order.
              OCTAVE_LOCAL_BUFFER (octave_scalar_map, new_map_list, n);

              permute_to_correct_order (n, nf, idx, map_list, new_map_list);

              do_cat (dim, n, new_map_list, retval);
            }

        }
      else
        {
          dim_vector& rd = retval.m_dimensions;
          rd.resize (dim+1, 1);
          rd(0) = rd(1) = 1;
          rd(dim) = n;
        }

      retval.optimize_dimensions ();
    }

  return retval;
}

octave_map
octave_map::cat (int dim, octave_idx_type n, const octave_map *map_list)
{
  octave_map retval;

  // Allow dim = -1, -2 for compatibility, though it makes no difference here.
  if (dim == -1 || dim == -2)
    dim = -dim - 1;
  else if (dim < 0)
    error ("cat: invalid dimension");

  if (n == 1)
    retval = map_list[0];
  else if (n > 1)
    {
      octave_idx_type idx, nf = 0;

      for (idx = 0; idx < n; idx++)
        {
          nf = map_list[idx].nfields ();
          if (nf > 0)
            {
              retval.m_keys = map_list[idx].m_keys;
              break;
            }
        }

      // Try the fast case.
      bool all_same = true;

      if (nf > 0)
        {
          for (octave_idx_type i = 0; i < n; i++)
            {
              all_same = map_list[idx].m_keys.is_same (map_list[i].m_keys);

              if (! all_same)
                break;
            }
        }

      if (all_same && nf > 0)
        do_cat (dim, n, map_list, retval);
      else
        {
          if (nf > 0)
            {
              // permute all structures to correct order.
              OCTAVE_LOCAL_BUFFER (octave_map, new_map_list, n);

              permute_to_correct_order (n, nf, idx, map_list, new_map_list);

              do_cat (dim, n, new_map_list, retval);
            }
          else
            {
              dim_vector dv = map_list[0].m_dimensions;

              for (octave_idx_type i = 1; i < n; i++)
                {
                  if (! dv.concat (map_list[i].m_dimensions, dim))
                    error ("dimension mismatch in struct concatenation");
                }

              retval.m_dimensions = dv;
            }
        }

      retval.optimize_dimensions ();
    }

  return retval;
}

/*
## test preservation of key order by concatenation
%!test
%! x(1, 1).d = 10;  x(4, 6).a = "b";  x(2, 4).f = 27;
%! y(1, 6).f = 11;  y(1, 6).a = "c";  y(1, 6).d = 33;
%! assert (fieldnames ([x; y]), {"d"; "a"; "f"});

%!test
%! s = struct ();
%! sr = [s,s];
%! sc = [s;s];
%! sm = [s,s;s,s];
%! assert (numfields (sr), 0);
%! assert (numfields (sc), 0);
%! assert (numfields (sm), 0);
%! assert (size (sr), [1, 2]);
%! assert (size (sc), [2, 1]);
%! assert (size (sm), [2, 2]);
*/

octave_map
octave_map::index (const octave::idx_vector& i, bool resize_ok) const
{
  octave_map retval (m_keys);
  octave_idx_type nf = nfields ();

  for (octave_idx_type k = 0; k < nf; k++)
    retval.m_vals[k] = m_vals[k].index (i, resize_ok);

  if (nf > 0)
    retval.m_dimensions = retval.m_vals[0].dims ();
  else
    {
      // Use dummy array.  FIXME: Need(?) a better solution.
      Array<char> dummy (m_dimensions);
      dummy = dummy.index (i, resize_ok);
      retval.m_dimensions = dummy.dims ();
    }

  retval.optimize_dimensions ();

  return retval;
}

octave_map
octave_map::index (const octave::idx_vector& i, const octave::idx_vector& j,
                   bool resize_ok) const
{
  octave_map retval (m_keys);
  octave_idx_type nf = nfields ();

  for (octave_idx_type k = 0; k < nf; k++)
    retval.m_vals[k] = m_vals[k].index (i, j, resize_ok);

  if (nf > 0)
    retval.m_dimensions = retval.m_vals[0].dims ();
  else
    {
      // Use dummy array.  FIXME: Need(?) a better solution.
      Array<char> dummy (m_dimensions);
      dummy = dummy.index (i, j, resize_ok);
      retval.m_dimensions = dummy.dims ();
    }

  retval.optimize_dimensions ();

  return retval;
}

octave_map
octave_map::index (const Array<octave::idx_vector>& ia, bool resize_ok) const
{
  octave_map retval (m_keys);
  octave_idx_type nf = nfields ();

  for (octave_idx_type k = 0; k < nf; k++)
    retval.m_vals[k] = m_vals[k].index (ia, resize_ok);

  if (nf > 0)
    retval.m_dimensions = retval.m_vals[0].dims ();
  else
    {
      // Use dummy array.  FIXME: Need(?) a better solution.
      Array<char> dummy (m_dimensions);
      dummy = dummy.index (ia, resize_ok);
      retval.m_dimensions = dummy.dims ();
    }

  retval.optimize_dimensions ();

  return retval;
}

octave_map
octave_map::index (const octave_value_list& idx, bool resize_ok) const
{
  octave_idx_type n_idx = idx.length ();
  octave_map retval;

  // If we catch an indexing error in index_vector, we flag an error in
  // index k.  Ensure it is the right value before each idx_vector call.
  // Same variable as used in the for loop in the default case.

  octave_idx_type k = 0;

  try
    {
      switch (n_idx)
        {
        case 1:
          {
            octave::idx_vector i = idx(0).index_vector ();

            retval = index (i, resize_ok);
          }
          break;

        case 2:
          {
            octave::idx_vector i = idx(0).index_vector ();

            k = 1;
            octave::idx_vector j = idx(1).index_vector ();

            retval = index (i, j, resize_ok);
          }
          break;

        default:
          {
            Array<octave::idx_vector> ia (dim_vector (n_idx, 1));

            for (k = 0; k < n_idx; k++)
              ia(k) = idx(k).index_vector ();

            retval = index (ia, resize_ok);
          }
          break;
        }
    }
  catch (octave::index_exception& ie)
    {
      // Rethrow to allow more info to be reported later.
      ie.set_pos_if_unset (n_idx, k+1);
      throw;
    }

  return retval;
}

// Perhaps one day these will be optimized.  Right now, they just call index.
octave_map
octave_map::column (octave_idx_type k) const
{
  return index (octave::idx_vector::colon, k);
}

octave_map
octave_map::page (octave_idx_type k) const
{
  static Array<octave::idx_vector> ia (dim_vector (3, 1),
                                       octave::idx_vector::colon);

  ia(2) = k;
  return index (ia);
}

void
octave_map::assign (const octave::idx_vector& i, const octave_map& rhs)
{
  if (rhs.m_keys.is_same (m_keys))
    {
      octave_idx_type nf = nfields ();

      for (octave_idx_type k = 0; k < nf; k++)
        m_vals[k].assign (i, rhs.m_vals[k], Matrix ());

      if (nf > 0)
        m_dimensions = m_vals[0].dims ();
      else
        {
          // Use dummy array.  FIXME: Need(?) a better solution.
          Array<char> dummy (m_dimensions), rhs_dummy (rhs.m_dimensions);
          dummy.assign (i, rhs_dummy);
          m_dimensions = dummy.dims ();
        }

      optimize_dimensions ();
    }
  else if (nfields () == 0)
    {
      octave_map tmp (m_dimensions, rhs.m_keys);
      tmp.assign (i, rhs);
      *this = tmp;
    }
  else
    {
      Array<octave_idx_type> perm;
      octave_map rhs1;

      try
        {
          rhs1 = rhs.orderfields (*this, perm);
        }
      catch (octave::execution_exception& ee)
        {
          error (ee, "incompatible fields in struct assignment");
        }

      error_unless (rhs1.m_keys.is_same (m_keys));
      assign (i, rhs1);
    }
}

void
octave_map::assign (const octave::idx_vector& i, const octave::idx_vector& j,
                    const octave_map& rhs)
{
  if (rhs.m_keys.is_same (m_keys))
    {
      octave_idx_type nf = nfields ();

      for (octave_idx_type k = 0; k < nf; k++)
        m_vals[k].assign (i, j, rhs.m_vals[k], Matrix ());

      if (nf > 0)
        m_dimensions = m_vals[0].dims ();
      else
        {
          // Use dummy array.  FIXME: Need(?) a better solution.
          Array<char> dummy (m_dimensions), rhs_dummy (rhs.m_dimensions);
          dummy.assign (i, j, rhs_dummy);
          m_dimensions = dummy.dims ();
        }

      optimize_dimensions ();
    }
  else if (nfields () == 0)
    {
      octave_map tmp (m_dimensions, rhs.m_keys);
      tmp.assign (i, j, rhs);
      *this = tmp;
    }
  else
    {
      Array<octave_idx_type> perm;
      octave_map rhs1;

      try
        {
          rhs1 = rhs.orderfields (*this, perm);
        }
      catch (octave::execution_exception& ee)
        {
          error (ee, "incompatible fields in struct assignment");
        }

      error_unless (rhs1.m_keys.is_same (m_keys));
      assign (i, j, rhs1);
    }
}

void
octave_map::assign (const Array<octave::idx_vector>& ia,
                    const octave_map& rhs)
{
  if (rhs.m_keys.is_same (m_keys))
    {
      octave_idx_type nf = nfields ();

      for (octave_idx_type k = 0; k < nf; k++)
        m_vals[k].assign (ia, rhs.m_vals[k], Matrix ());

      if (nf > 0)
        m_dimensions = m_vals[0].dims ();
      else
        {
          // Use dummy array.  FIXME: Need(?) a better solution.
          Array<char> dummy (m_dimensions), rhs_dummy (rhs.m_dimensions);
          dummy.assign (ia, rhs_dummy);
          m_dimensions = dummy.dims ();
        }

      optimize_dimensions ();
    }
  else if (nfields () == 0)
    {
      octave_map tmp (m_dimensions, rhs.m_keys);
      tmp.assign (ia, rhs);
      *this = tmp;
    }
  else
    {
      Array<octave_idx_type> perm;
      octave_map rhs1;

      try
        {
          rhs1 = rhs.orderfields (*this, perm);
        }
      catch (octave::execution_exception& ee)
        {
          error (ee, "incompatible fields in struct assignment");
        }

      error_unless (rhs1.m_keys.is_same (m_keys));
      assign (ia, rhs1);
    }
}

void
octave_map::assign (const octave_value_list& idx, const octave_map& rhs)
{
  octave_idx_type n_idx = idx.length ();

  // If we catch an indexing error in index_vector, we flag an error in
  // index k.  Ensure it is the right value before each idx_vector call.
  // Same variable as used in the for loop in the default case.

  octave_idx_type k = 0;

  try
    {
      switch (n_idx)
        {
        case 1:
          {
            octave::idx_vector i = idx(0).index_vector ();

            assign (i, rhs);
          }
          break;

        case 2:
          {
            octave::idx_vector i = idx(0).index_vector ();

            k = 1;
            octave::idx_vector j = idx(1).index_vector ();

            assign (i, j, rhs);
          }
          break;

        default:
          {
            Array<octave::idx_vector> ia (dim_vector (n_idx, 1));

            for (k = 0; k < n_idx; k++)
              ia(k) = idx(k).index_vector ();

            assign (ia, rhs);
          }
          break;
        }
    }
  catch (octave::index_exception& ie)
    {
      // Rethrow to allow more info to be reported later.
      ie.set_pos_if_unset (n_idx, k+1);
      throw;
    }
}

void
octave_map::assign (const octave_value_list& idx, const std::string& k,
                    const Cell& rhs)
{
  Cell tmp;
  auto p = seek (k);
  Cell& ref = (p != end () ? contents (p) : tmp);

  if (&ref == &tmp)
    ref = Cell (m_dimensions);

  ref.assign (idx, rhs);

  if (ref.dims () != m_dimensions)
    {
      m_dimensions = ref.dims ();

      octave_idx_type nf = nfields ();
      for (octave_idx_type i = 0; i < nf; i++)
        {
          if (&m_vals[i] != &ref)
            m_vals[i].resize (m_dimensions, Matrix ());
        }

      optimize_dimensions ();
    }

  if (&ref == &tmp)
    setfield (k, tmp);
}

/*
%!test
%! rhs.b = 1;
%! a(3) = rhs;
%! assert ({a.b}, {[], [], 1});
*/

void
octave_map::delete_elements (const octave::idx_vector& i)
{
  octave_idx_type nf = nfields ();
  for (octave_idx_type k = 0; k < nf; k++)
    m_vals[k].delete_elements (i);

  if (nf > 0)
    m_dimensions = m_vals[0].dims ();
  else
    {
      // Use dummy array.  FIXME: Need(?) a better solution.
      Array<char> dummy (m_dimensions);
      dummy.delete_elements (i);
      m_dimensions = dummy.dims ();
    }

  optimize_dimensions ();
}

void
octave_map::delete_elements (int dim, const octave::idx_vector& i)
{
  octave_idx_type nf = nfields ();
  for (octave_idx_type k = 0; k < nf; k++)
    m_vals[k].delete_elements (dim, i);

  if (nf > 0)
    m_dimensions = m_vals[0].dims ();
  else
    {
      // Use dummy array.  FIXME: Need(?) a better solution.
      Array<char> dummy (m_dimensions);
      dummy.delete_elements (dim, i);
      m_dimensions = dummy.dims ();
    }

  optimize_dimensions ();
}

void
octave_map::delete_elements (const Array<octave::idx_vector>& ia)
{
  octave_idx_type nf = nfields ();
  for (octave_idx_type k = 0; k < nf; k++)
    m_vals[k].delete_elements (ia);

  if (nf > 0)
    m_dimensions = m_vals[0].dims ();
  else
    {
      // Use dummy array.  FIXME: Need(?) a better solution.
      Array<char> dummy (m_dimensions);
      dummy.delete_elements (ia);
      m_dimensions = dummy.dims ();
    }

  optimize_dimensions ();
}

void
octave_map::delete_elements (const octave_value_list& idx)
{
  octave_idx_type n_idx = idx.length ();

  Array<octave::idx_vector> ia (dim_vector (n_idx, 1));

  for (octave_idx_type i = 0; i < n_idx; i++)
    {
      try
        {
          ia(i) = idx(i).index_vector ();
        }
      catch (octave::index_exception& ie)
        {
          // Rethrow to allow more info to be reported later.
          ie.set_pos_if_unset (n_idx, i+1);
          throw;
        }
    }

  delete_elements (ia);
}

/*
## test preservation of key order by indexing
%!test
%! x(1, 1).d = 10;  x(4, 6).a = "b";  x(2, 4).f = 27;
%! assert (fieldnames (x([1, 2], [2:5])), {"d"; "a"; "f"});
*/

octave_map
octave_map::concat (const octave_map& rb, const Array<octave_idx_type>& ra_idx)
{
  if (nfields () == rb.nfields ())
    {
      for (auto pa = cbegin (); pa != cend (); pa++)
        {
          auto pb = rb.seek (key (pa));

          if (pb == rb.cend ())
            error ("field name mismatch in structure concatenation");

          contents(pa).insert (rb.contents (pb), ra_idx);
        }
    }
  else
    {
      dim_vector dv = dims ();

      if (dv.all_zero ())
        *this = rb;
      else if (! rb.dims ().all_zero ())
        error ("invalid structure concatenation");
    }

  return *this;
}

void
octave_map::optimize_dimensions (void)
{
  octave_idx_type nf = nfields ();

  for (octave_idx_type i = 0; i < nf; i++)
    {
      if (! m_vals[i].optimize_dimensions (m_dimensions))
        error ("internal error: dimension mismatch across fields in struct");
    }

}
