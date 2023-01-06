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

#if ! defined (octave_idx_vector_h)
#define octave_idx_vector_h 1

#include "octave-config.h"

#include <cassert>
#include <cstring>

#include <algorithm>
#include <iosfwd>
#include <memory>

#include "Array-fwd.h"
#include "dim-vector.h"
#include "oct-inttypes.h"
#include "oct-refcount.h"
#include "Sparse-fwd.h"
#include "range-fwd.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Design rationale:
//
// idx_vector is a reference-counting, polymorphic pointer, that can
// contain 4 types of index objects: a magic colon, a range, a scalar,
// or an index vector.
//
// Polymorphic methods for single element access are provided, as well
// as templates implementing "early dispatch", i.e., hoisting the checks
// for index type out of loops.

class
OCTAVE_API
idx_vector
{
public:

  enum idx_class_type
  {
    class_invalid = -1,
    class_colon = 0,
    class_range,
    class_scalar,
    class_vector,
    class_mask
  };

  template <typename T, typename D> friend class std::unique_ptr;

private:

  class OCTAVE_API idx_base_rep
  {
  public:

    idx_base_rep (void) : m_count (1) { }

    // No copying!

    idx_base_rep (const idx_base_rep&) = delete;

    idx_base_rep& operator = (const idx_base_rep&) = delete;

    virtual ~idx_base_rep (void) = default;

    // Non-range-checking element query.
    virtual octave_idx_type xelem (octave_idx_type i) const = 0;

    // Range-checking element query.
    virtual octave_idx_type checkelem (octave_idx_type i) const = 0;

    // Length of the index vector.
    virtual octave_idx_type length (octave_idx_type n) const = 0;

    // The maximum index + 1.  The actual dimension is passed in.
    virtual octave_idx_type extent (octave_idx_type n) const = 0;

    // Index class.
    virtual idx_class_type idx_class (void) const { return class_invalid; }

    // Sorts, maybe uniqifies, and returns a clone object pointer.
    virtual idx_base_rep * sort_uniq_clone (bool uniq = false) = 0;
    // Sorts, and returns a sorting permutation (aka Array::sort).
    virtual idx_base_rep * sort_idx (Array<octave_idx_type>&) = 0;

    // Checks whether the index is colon or a range equivalent to colon.
    virtual bool is_colon_equiv (octave_idx_type) const { return false; }

    // The original dimensions of object (used when subscribing by matrices).
    virtual dim_vector orig_dimensions (void) const { return dim_vector (); }

    // i/o
    virtual std::ostream& print (std::ostream& os) const = 0;

    virtual Array<octave_idx_type> as_array (void);

    refcount<octave_idx_type> m_count;
  };

  // The magic colon index.
  class OCTAVE_API idx_colon_rep : public idx_base_rep
  {
  public:

    idx_colon_rep (void) = default;

    OCTAVE_API idx_colon_rep (char c);

    // No copying!

    idx_colon_rep (const idx_colon_rep& idx) = delete;

    idx_colon_rep& operator = (const idx_colon_rep& idx) = delete;

    octave_idx_type xelem (octave_idx_type i) const { return i; }

    OCTAVE_API octave_idx_type checkelem (octave_idx_type i) const;

    octave_idx_type length (octave_idx_type n) const { return n; }

    octave_idx_type extent (octave_idx_type n) const { return n; }

    idx_class_type idx_class (void) const { return class_colon; }

    idx_base_rep * sort_uniq_clone (bool = false)
    { m_count++; return this; }

    OCTAVE_NORETURN idx_base_rep * sort_idx (Array<octave_idx_type>&);

    bool is_colon_equiv (octave_idx_type) const { return true; }

    OCTAVE_API std::ostream& print (std::ostream& os) const;
  };

  // To distinguish the "direct" constructors that blindly trust the data.
  enum direct { DIRECT };

  // The integer range index.
  class OCTAVE_API idx_range_rep : public idx_base_rep
  {
  public:

    idx_range_rep (void) = delete;

    idx_range_rep (octave_idx_type start, octave_idx_type len,
                   octave_idx_type step, direct)
      : idx_base_rep (), m_start (start), m_len (len), m_step (step) { }

    // Zero-based constructor for index range starting at `start` (inclusive)
    // and ending at `limit` (exclusive) in steps of `step`.
    idx_range_rep (octave_idx_type start, octave_idx_type limit,
                   octave_idx_type step);

    OCTAVE_API idx_range_rep (const range<double>&);

    // No copying!

    idx_range_rep (const idx_range_rep& idx) = delete;

    idx_range_rep& operator = (const idx_range_rep& idx) = delete;

    octave_idx_type xelem (octave_idx_type i) const
    { return m_start + i * m_step; }

    octave_idx_type checkelem (octave_idx_type i) const;

    octave_idx_type length (octave_idx_type) const { return m_len; }

    octave_idx_type extent (octave_idx_type n) const
    {
      return m_len ? std::max (n, m_start + 1 + (m_step < 0 ? 0 : m_step * (m_len - 1))) : n;
    }

    idx_class_type idx_class (void) const { return class_range; }

    OCTAVE_API idx_base_rep * sort_uniq_clone (bool uniq = false);

    OCTAVE_API idx_base_rep * sort_idx (Array<octave_idx_type>&);

    bool is_colon_equiv (octave_idx_type n) const
    { return m_start == 0 && m_step == 1 && m_len == n; }

    dim_vector orig_dimensions (void) const
    { return dim_vector (1, m_len); }

    octave_idx_type get_start (void) const { return m_start; }

    octave_idx_type get_step (void) const { return m_step; }

    OCTAVE_API std::ostream& print (std::ostream& os) const;

    OCTAVE_API range<double> unconvert (void) const;

    OCTAVE_API Array<octave_idx_type> as_array (void);

  private:

    octave_idx_type m_start, m_len, m_step;
  };

  // The integer scalar index.
  class OCTAVE_API idx_scalar_rep : public idx_base_rep
  {
  public:

    idx_scalar_rep (void) = delete;

    idx_scalar_rep (octave_idx_type i, direct) : idx_base_rep (), m_data (i) { }

    // No copying!

    idx_scalar_rep (const idx_scalar_rep& idx) = delete;

    idx_scalar_rep& operator = (const idx_scalar_rep& idx) = delete;

    // Zero-based constructor.
    OCTAVE_API idx_scalar_rep (octave_idx_type i);

    template <typename T>
    idx_scalar_rep (T x);

    octave_idx_type xelem (octave_idx_type) const { return m_data; }

    OCTAVE_API octave_idx_type checkelem (octave_idx_type i) const;

    octave_idx_type length (octave_idx_type) const { return 1; }

    octave_idx_type extent (octave_idx_type n) const
    { return std::max (n, m_data + 1); }

    idx_class_type idx_class (void) const { return class_scalar; }

    idx_base_rep * sort_uniq_clone (bool = false)
    { m_count++; return this; }

    OCTAVE_API idx_base_rep * sort_idx (Array<octave_idx_type>&);

    bool is_colon_equiv (octave_idx_type n) const
    { return n == 1 && m_data == 0; }

    dim_vector orig_dimensions (void) const { return dim_vector (1, 1); }

    octave_idx_type get_data (void) const { return m_data; }

    OCTAVE_API std::ostream& print (std::ostream& os) const;

    OCTAVE_API double unconvert (void) const;

    OCTAVE_API Array<octave_idx_type> as_array (void);

  private:

    octave_idx_type m_data;
  };

  // The integer vector index.
  class OCTAVE_API idx_vector_rep : public idx_base_rep
  {
  public:

    idx_vector_rep (void)
      : m_data (nullptr), m_len (0), m_ext (0), m_aowner (nullptr), m_orig_dims () { }

    // Direct constructor.
    idx_vector_rep (octave_idx_type *data, octave_idx_type len,
                    octave_idx_type ext, const dim_vector& od, direct)
      : idx_base_rep (), m_data (data), m_len (len), m_ext (ext),
        m_aowner (nullptr), m_orig_dims (od)
    { }

    // Zero-based constructor.
    OCTAVE_API idx_vector_rep (const Array<octave_idx_type>& inda);

    OCTAVE_API idx_vector_rep (const Array<octave_idx_type>& inda,
                               octave_idx_type ext, direct);

    template <typename T>
    idx_vector_rep (const Array<T>&);

    OCTAVE_API idx_vector_rep (bool);

    OCTAVE_API idx_vector_rep (const Array<bool>&, octave_idx_type = -1);

    OCTAVE_API idx_vector_rep (const Sparse<bool>&);

    // No copying!

    idx_vector_rep (const idx_vector_rep& idx) = delete;

    idx_vector_rep& operator = (const idx_vector_rep& idx) = delete;

    ~idx_vector_rep (void);

    octave_idx_type xelem (octave_idx_type i) const { return m_data[i]; }

    OCTAVE_API octave_idx_type checkelem (octave_idx_type i) const;

    octave_idx_type length (octave_idx_type) const { return m_len; }

    octave_idx_type extent (octave_idx_type n) const
    { return std::max (n, m_ext); }

    idx_class_type idx_class (void) const { return class_vector; }

    idx_base_rep * sort_uniq_clone (bool uniq = false);

    OCTAVE_API idx_base_rep * sort_idx (Array<octave_idx_type>&);

    dim_vector orig_dimensions (void) const { return m_orig_dims; }

    const octave_idx_type * get_data (void) const { return m_data; }

    OCTAVE_API std::ostream& print (std::ostream& os) const;

    OCTAVE_API Array<double> unconvert (void) const;

    OCTAVE_API Array<octave_idx_type> as_array (void);

  private:

    const octave_idx_type *m_data;
    octave_idx_type m_len;
    octave_idx_type m_ext;

    // This is a trick to allow user-given zero-based arrays to be used
    // as indices without copying.  If the following pointer is nonzero,
    // we do not own the data, but rather have an Array<octave_idx_type>
    // object that provides us the data.  Note that we need a pointer
    // because we deferred the Array<T> declaration and we do not want
    // it yet to be defined.

    Array<octave_idx_type> *m_aowner;

    dim_vector m_orig_dims;
  };

  // The logical mask index.
  class OCTAVE_API idx_mask_rep : public idx_base_rep
  {
  public:

    idx_mask_rep (void) = delete;

    // Direct constructor.
    idx_mask_rep (bool *data, octave_idx_type len,
                  octave_idx_type ext, const dim_vector& od, direct)
      : idx_base_rep (), m_data (data), m_len (len), m_ext (ext),
        m_lsti (-1), m_lste (-1), m_aowner (nullptr), m_orig_dims (od)
    { }

    OCTAVE_API idx_mask_rep (bool);

    OCTAVE_API idx_mask_rep (const Array<bool>&, octave_idx_type = -1);

    // No copying!

    idx_mask_rep (const idx_mask_rep& idx) = delete;

    idx_mask_rep& operator = (const idx_mask_rep& idx) = delete;

    OCTAVE_API ~idx_mask_rep (void);

    octave_idx_type xelem (octave_idx_type i) const;

    octave_idx_type checkelem (octave_idx_type i) const;

    octave_idx_type length (octave_idx_type) const { return m_len; }

    octave_idx_type extent (octave_idx_type n) const
    { return std::max (n, m_ext); }

    idx_class_type idx_class (void) const { return class_mask; }

    idx_base_rep * sort_uniq_clone (bool = false)
    { m_count++; return this; }

    OCTAVE_API idx_base_rep * sort_idx (Array<octave_idx_type>&);

    dim_vector orig_dimensions (void) const { return m_orig_dims; }

    bool is_colon_equiv (octave_idx_type n) const
    { return m_len == n && m_ext == n; }

    const bool * get_data (void) const { return m_data; }

    OCTAVE_API std::ostream& print (std::ostream& os) const;

    OCTAVE_API Array<bool> unconvert (void) const;

    OCTAVE_API Array<octave_idx_type> as_array (void);

  private:

    const bool *m_data;
    octave_idx_type m_len;
    octave_idx_type m_ext;

    // FIXME: I'm not sure if this is a good design.  Maybe it would be
    // better to employ some sort of generalized iteration scheme.
    mutable octave_idx_type m_lsti;
    mutable octave_idx_type m_lste;

    // This is a trick to allow user-given mask arrays to be used as
    // indices without copying.  If the following pointer is nonzero, we
    // do not own the data, but rather have an Array<bool> object that
    // provides us the data.  Note that we need a pointer because we
    // deferred the Array<T> declaration and we do not want it yet to be
    // defined.

    Array<bool> *m_aowner;

    dim_vector m_orig_dims;
  };

  idx_vector (idx_base_rep *r) : m_rep (r) { }

  // The shared empty vector representation (for fast default
  // constructor).
  static OCTAVE_API idx_vector_rep * nil_rep (void);

public:

  // Fast empty constructor.
  idx_vector (void) : m_rep (nil_rep ()) { m_rep->m_count++; }

  // Zero-based constructors (for use from C++).
  idx_vector (octave_idx_type i) : m_rep (new idx_scalar_rep (i)) { }

#if OCTAVE_SIZEOF_INT != OCTAVE_SIZEOF_IDX_TYPE
  idx_vector (int i)
    : m_rep (new idx_scalar_rep (static_cast<octave_idx_type> (i))) { }
#endif

#if (OCTAVE_SIZEOF_F77_INT_TYPE != OCTAVE_SIZEOF_IDX_TYPE \
     && OCTAVE_SIZEOF_F77_INT_TYPE != OCTAVE_SIZEOF_INT)
  idx_vector (octave_f77_int_type i)
    : m_rep (new idx_scalar_rep (static_cast<octave_idx_type> (i))) { }
#endif

  idx_vector (octave_idx_type start, octave_idx_type limit,
              octave_idx_type step = 1)
    : m_rep (new idx_range_rep (start, limit, step)) { }

  static idx_vector
  make_range (octave_idx_type start, octave_idx_type step,
              octave_idx_type len)
  {
    return idx_vector (new idx_range_rep (start, len, step, DIRECT));
  }

  idx_vector (const Array<octave_idx_type>& inda)
    : m_rep (new idx_vector_rep (inda)) { }

  // Directly pass extent, no checking.
  idx_vector (const Array<octave_idx_type>& inda, octave_idx_type ext)
    : m_rep (new idx_vector_rep (inda, ext, DIRECT)) { }

  // Colon is best constructed by simply copying (or referencing) this member.
  static const idx_vector colon;

  // or passing ':' here
  idx_vector (char c) : m_rep (new idx_colon_rep (c)) { }

  // Conversion constructors (used by interpreter).

  template <typename T>
  idx_vector (octave_int<T> x) : m_rep (new idx_scalar_rep (x)) { }

  idx_vector (double x) : m_rep (new idx_scalar_rep (x)) { }

  idx_vector (float x) : m_rep (new idx_scalar_rep (x)) { }

  // A scalar bool does not necessarily map to scalar index.
  idx_vector (bool x) : m_rep (new idx_mask_rep (x)) { }

  template <typename T>
  idx_vector (const Array<octave_int<T>>& nda)
    : m_rep (new idx_vector_rep (nda)) { }

  idx_vector (const Array<double>& nda) : m_rep (new idx_vector_rep (nda)) { }

  idx_vector (const Array<float>& nda) : m_rep (new idx_vector_rep (nda)) { }

  OCTAVE_API idx_vector (const Array<bool>& nda);

  idx_vector (const range<double>& r) : m_rep (new idx_range_rep (r)) { }

  idx_vector (const Sparse<bool>& nda) : m_rep (new idx_vector_rep (nda)) { }

  idx_vector (const idx_vector& a) : m_rep (a.m_rep) { m_rep->m_count++; }

  ~idx_vector (void)
  {
    if (--m_rep->m_count == 0 && m_rep != nil_rep ())
      delete m_rep;
  }

  idx_vector& operator = (const idx_vector& a)
  {
    if (this != &a)
      {
        if (--m_rep->m_count == 0 && m_rep != nil_rep ())
          delete m_rep;

        m_rep = a.m_rep;
        m_rep->m_count++;
      }
    return *this;
  }

  idx_class_type idx_class (void) const { return m_rep->idx_class (); }

  octave_idx_type length (octave_idx_type n = 0) const
  { return m_rep->length (n); }

  octave_idx_type extent (octave_idx_type n) const
  { return m_rep->extent (n); }

  octave_idx_type xelem (octave_idx_type n) const
  { return m_rep->xelem (n); }

  octave_idx_type checkelem (octave_idx_type n) const
  { return m_rep->xelem (n); }

  octave_idx_type operator () (octave_idx_type n) const
  { return m_rep->xelem (n); }

  // FIXME: idx_vector objects are either created successfully or an
  // error is thrown, so this method no longer makes sense.
  operator bool (void) const { return true; }

  bool is_colon (void) const
  { return m_rep->idx_class () == class_colon; }

  bool is_scalar (void) const
  { return m_rep->idx_class () == class_scalar; }

  bool is_range (void) const
  { return m_rep->idx_class () == class_range; }

  bool is_colon_equiv (octave_idx_type n) const
  { return m_rep->is_colon_equiv (n); }

  idx_vector sorted (bool uniq = false) const
  { return idx_vector (m_rep->sort_uniq_clone (uniq)); }

  idx_vector sorted (Array<octave_idx_type>& sidx) const
  { return idx_vector (m_rep->sort_idx (sidx)); }

  dim_vector orig_dimensions (void) const { return m_rep->orig_dimensions (); }

  octave_idx_type orig_rows (void) const
  { return orig_dimensions () (0); }

  octave_idx_type orig_columns (void) const
  { return orig_dimensions () (1); }

  int orig_empty (void) const
  { return (! is_colon () && orig_dimensions ().any_zero ()); }

  // i/o

  std::ostream& print (std::ostream& os) const { return m_rep->print (os); }

  friend std::ostream& operator << (std::ostream& os, const idx_vector& a)
  { return a.print (os); }

  // Slice with specializations.  No checking of bounds!
  //
  // This is equivalent to the following loop (but much faster):
  //
  // for (octave_idx_type i = 0; i < idx->length (n); i++)
  //   dest[i] = src[idx(i)];
  // return i;
  //
  template <typename T>
  octave_idx_type
  index (const T *src, octave_idx_type n, T *dest) const
  {
    octave_idx_type len = m_rep->length (n);

    switch (m_rep->idx_class ())
      {
      case class_colon:
        std::copy_n (src, len, dest);
        break;

      case class_range:
        {
          idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
          octave_idx_type start = r->get_start ();
          octave_idx_type step = r->get_step ();
          const T *ssrc = src + start;
          if (step == 1)
            std::copy_n (ssrc, len, dest);
          else if (step == -1)
            std::reverse_copy (ssrc - len + 1, ssrc + 1, dest);
          else if (step == 0)
            std::fill_n (dest, len, *ssrc);
          else
            {
              for (octave_idx_type i = 0, j = 0; i < len; i++, j += step)
                dest[i] = ssrc[j];
            }
        }
        break;

      case class_scalar:
        {
          idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
          dest[0] = src[r->get_data ()];
        }
        break;

      case class_vector:
        {
          idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (m_rep);
          const octave_idx_type *data = r->get_data ();
          for (octave_idx_type i = 0; i < len; i++)
            dest[i] = src[data[i]];
        }
        break;

      case class_mask:
        {
          idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (m_rep);
          const bool *data = r->get_data ();
          octave_idx_type ext = r->extent (0);
          for (octave_idx_type i = 0; i < ext; i++)
            if (data[i]) *dest++ = src[i];
        }
        break;

      default:
        assert (false);
        break;
      }

    return len;
  }

  // Slice assignment with specializations.  No checking of bounds!
  //
  // This is equivalent to the following loop (but much faster):
  //
  // for (octave_idx_type i = 0; i < idx->length (n); i++)
  //   dest[idx(i)] = src[i];
  // return i;
  //
  template <typename T>
  octave_idx_type
  assign (const T *src, octave_idx_type n, T *dest) const
  {
    octave_idx_type len = m_rep->length (n);

    switch (m_rep->idx_class ())
      {
      case class_colon:
        std::copy_n (src, len, dest);
        break;

      case class_range:
        {
          idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
          octave_idx_type start = r->get_start ();
          octave_idx_type step = r->get_step ();
          T *sdest = dest + start;
          if (step == 1)
            std::copy_n (src, len, sdest);
          else if (step == -1)
            std::reverse_copy (src, src + len, sdest - len + 1);
          else
            {
              for (octave_idx_type i = 0, j = 0; i < len; i++, j += step)
                sdest[j] = src[i];
            }
        }
        break;

      case class_scalar:
        {
          idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
          dest[r->get_data ()] = src[0];
        }
        break;

      case class_vector:
        {
          idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (m_rep);
          const octave_idx_type *data = r->get_data ();
          for (octave_idx_type i = 0; i < len; i++)
            dest[data[i]] = src[i];
        }
        break;

      case class_mask:
        {
          idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (m_rep);
          const bool *data = r->get_data ();
          octave_idx_type ext = r->extent (0);
          for (octave_idx_type i = 0; i < ext; i++)
            if (data[i]) dest[i] = *src++;
        }
        break;

      default:
        assert (false);
        break;
      }

    return len;
  }

  // Slice fill with specializations.  No checking of bounds!
  //
  // This is equivalent to the following loop (but much faster):
  //
  // for (octave_idx_type i = 0; i < idx->length (n); i++)
  //   dest[idx(i)] = val;
  // return i;
  //
  template <typename T>
  octave_idx_type
  fill (const T& val, octave_idx_type n, T *dest) const
  {
    octave_idx_type len = m_rep->length (n);

    switch (m_rep->idx_class ())
      {
      case class_colon:
        std::fill_n (dest, len, val);
        break;

      case class_range:
        {
          idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
          octave_idx_type start = r->get_start ();
          octave_idx_type step = r->get_step ();
          T *sdest = dest + start;
          if (step == 1)
            std::fill_n (sdest, len, val);
          else if (step == -1)
            std::fill (sdest - len + 1, sdest + 1, val);
          else
            {
              for (octave_idx_type i = 0, j = 0; i < len; i++, j += step)
                sdest[j] = val;
            }
        }
        break;

      case class_scalar:
        {
          idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
          dest[r->get_data ()] = val;
        }
        break;

      case class_vector:
        {
          idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (m_rep);
          const octave_idx_type *data = r->get_data ();
          for (octave_idx_type i = 0; i < len; i++)
            dest[data[i]] = val;
        }
        break;

      case class_mask:
        {
          idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (m_rep);
          const bool *data = r->get_data ();
          octave_idx_type ext = r->extent (0);
          for (octave_idx_type i = 0; i < ext; i++)
            if (data[i]) dest[i] = val;
        }
        break;

      default:
        assert (false);
        break;
      }

    return len;
  }

  // Generic non-breakable indexed loop.  The loop body should be
  // encapsulated in a single functor body.  This is equivalent to the
  // following loop (but faster, at least for simple inlined bodies):
  //
  // for (octave_idx_type i = 0; i < idx->length (n); i++) body (idx(i));

  template <typename Functor>
  void
  loop (octave_idx_type n, Functor body) const
  {
    octave_idx_type len = m_rep->length (n);

    switch (m_rep->idx_class ())
      {
      case class_colon:
        for (octave_idx_type i = 0; i < len; i++) body (i);
        break;

      case class_range:
        {
          idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
          octave_idx_type start = r->get_start ();
          octave_idx_type step = r->get_step ();
          octave_idx_type i, j;
          if (step == 1)
            for (i = start, j = start + len; i < j; i++) body (i);
          else if (step == -1)
            for (i = start, j = start - len; i > j; i--) body (i);
          else
            for (i = 0, j = start; i < len; i++, j += step) body (j);
        }
        break;

      case class_scalar:
        {
          idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
          body (r->get_data ());
        }
        break;

      case class_vector:
        {
          idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (m_rep);
          const octave_idx_type *data = r->get_data ();
          for (octave_idx_type i = 0; i < len; i++) body (data[i]);
        }
        break;

      case class_mask:
        {
          idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (m_rep);
          const bool *data = r->get_data ();
          octave_idx_type ext = r->extent (0);
          for (octave_idx_type i = 0; i < ext; i++)
            if (data[i]) body (i);
        }
        break;

      default:
        assert (false);
        break;
      }

  }

  // Generic breakable indexed loop.  The loop body should be
  // encapsulated in a single functor body.  This is equivalent to the
  // following loop (but faster, at least for simple inlined bodies):
  //
  // for (octave_idx_type i = 0; i < idx->length (n); i++)
  //   if (body (idx(i))) break;
  // return i;
  //

  template <typename Functor>
  octave_idx_type
  bloop (octave_idx_type n, Functor body) const
  {
    octave_idx_type len = m_rep->length (n), ret;

    switch (m_rep->idx_class ())
      {
      case class_colon:
        {
          octave_idx_type i;
          for (i = 0; i < len && body (i); i++) ;
          ret = i;
        }
        break;

      case class_range:
        {
          idx_range_rep *r = dynamic_cast<idx_range_rep *> (m_rep);
          octave_idx_type start = r->get_start ();
          octave_idx_type step = r->get_step ();
          octave_idx_type i, j;
          if (step == 1)
            for (i = start, j = start + len; i < j && body (i); i++) ;
          else if (step == -1)
            for (i = start, j = start - len; i > j && body (i); i--) ;
          else
            for (i = 0, j = start; i < len && body (j); i++, j += step) ;
          ret = i;
        }
        break;

      case class_scalar:
        {
          idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (m_rep);
          ret = (body (r->get_data ()) ? 1 : 0);
        }
        break;

      case class_vector:
        {
          idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (m_rep);
          const octave_idx_type *data = r->get_data ();
          octave_idx_type i;
          for (i = 0; i < len && body (data[i]); i++) ;
          ret = i;
        }
        break;

      case class_mask:
        {
          idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (m_rep);
          const bool *data = r->get_data ();
          octave_idx_type ext = r->extent (0);
          octave_idx_type j = 0;
          for (octave_idx_type i = 0; i < ext; i++)
            {
              if (data[i])
                {
                  if (body (i))
                    break;
                  else
                    j++;
                }
            }

          ret = j;
        }
        break;

      default:
        assert (false);
        break;
      }

    return ret;
  }

  // Rationale:
  // This method is the key to "smart indexing".  When indexing cartesian
  // arrays, sometimes consecutive index vectors can be reduced into a
  // single index.  If rows (A) = k and i.maybe_reduce (j) gives k, then
  // A(i,j)(:) is equal to A(k)(:).

  // If the next index can be reduced, returns true and updates this.
  OCTAVE_API bool
  maybe_reduce (octave_idx_type n, const idx_vector& j, octave_idx_type nj);

  OCTAVE_API bool
  is_cont_range (octave_idx_type n, octave_idx_type& l,
                 octave_idx_type& u) const;

  // Returns the increment for ranges and colon, 0 for scalars and empty
  // vectors, 1st difference otherwise.
  OCTAVE_API octave_idx_type increment (void) const;

  OCTAVE_API idx_vector
  complement (octave_idx_type n) const;

  OCTAVE_API bool is_permutation (octave_idx_type n) const;

  // Returns the inverse permutation.  If this is not a permutation on 1:n, the
  // result is undefined (but no error unless extent () != n).
  OCTAVE_API idx_vector inverse_permutation (octave_idx_type n) const;

  // Copies all the indices to a given array.  Not allowed for colons.
  OCTAVE_API void copy_data (octave_idx_type *data) const;

  // If the index is a mask, convert it to index vector.
  OCTAVE_API idx_vector unmask (void) const;

  // Unconverts the index to a scalar, Range, double array or a mask.
  OCTAVE_API void
  unconvert (idx_class_type& iclass, double& scalar, range<double>& range,
             Array<double>& array, Array<bool>& mask) const;

  OCTAVE_API Array<octave_idx_type> as_array (void) const;

  // Raw pointer to index array.  This is non-const because it may be
  // necessary to mutate the index.
  const OCTAVE_API octave_idx_type * raw (void);

  OCTAVE_API bool isvector (void) const;

  // FIXME: these are here for compatibility.  They should be removed
  // when no longer in use.

  octave_idx_type elem (octave_idx_type n) const
  { return (*this) (n); }

  bool is_colon_equiv (octave_idx_type n, int) const
  { return is_colon_equiv (n); }

  OCTAVE_API octave_idx_type
  freeze (octave_idx_type z_len, const char *tag, bool resize_ok = false);

  void sort (bool uniq = false)
  { *this = sorted (uniq); }

  OCTAVE_API octave_idx_type ones_count (void) const;

  octave_idx_type max (void) const { return extent (1) - 1; }

private:

  idx_base_rep *m_rep;

};

OCTAVE_END_NAMESPACE(octave)

// Provide the following typedef for backward compatibility.  Don't
// deprecate (yet) because it is used extensively.

typedef octave::idx_vector idx_vector;

#endif
