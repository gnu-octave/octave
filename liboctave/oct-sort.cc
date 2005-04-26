/*
Copyright (C) 2003 David Bateman

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

Code stolen in large part from Python's, listobject.c, which itself had
no license header. However, thanks to Tim Peters for the parts of the
code I ripped-off.

As required in the Python license the short description of the changes
made are

* convert the sorting code in listobject.cc into a generic class, 
  replacing PyObject* with the type of the class T.

The Python license is

  PSF LICENSE AGREEMENT FOR PYTHON 2.3
  --------------------------------------

  1. This LICENSE AGREEMENT is between the Python Software Foundation
  ("PSF"), and the Individual or Organization ("Licensee") accessing and
  otherwise using Python 2.3 software in source or binary form and its
  associated documentation.

  2. Subject to the terms and conditions of this License Agreement, PSF
  hereby grants Licensee a nonexclusive, royalty-free, world-wide
  license to reproduce, analyze, test, perform and/or display publicly,
  prepare derivative works, distribute, and otherwise use Python 2.3
  alone or in any derivative version, provided, however, that PSF's
  License Agreement and PSF's notice of copyright, i.e., "Copyright (c)
  2001, 2002, 2003 Python Software Foundation; All Rights Reserved" are
  retained in Python 2.3 alone or in any derivative version prepared by
  Licensee.

  3. In the event Licensee prepares a derivative work that is based on
  or incorporates Python 2.3 or any part thereof, and wants to make
  the derivative work available to others as provided herein, then
  Licensee hereby agrees to include in any such work a brief summary of
  the changes made to Python 2.3.

  4. PSF is making Python 2.3 available to Licensee on an "AS IS"
  basis.  PSF MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR
  IMPLIED.  BY WAY OF EXAMPLE, BUT NOT LIMITATION, PSF MAKES NO AND
  DISCLAIMS ANY REPRESENTATION OR WARRANTY OF MERCHANTABILITY OR FITNESS
  FOR ANY PARTICULAR PURPOSE OR THAT THE USE OF PYTHON 2.3 WILL NOT
  INFRINGE ANY THIRD PARTY RIGHTS.

  5. PSF SHALL NOT BE LIABLE TO LICENSEE OR ANY OTHER USERS OF PYTHON
  2.3 FOR ANY INCIDENTAL, SPECIAL, OR CONSEQUENTIAL DAMAGES OR LOSS AS
  A RESULT OF MODIFYING, DISTRIBUTING, OR OTHERWISE USING PYTHON 2.3,
  OR ANY DERIVATIVE THEREOF, EVEN IF ADVISED OF THE POSSIBILITY THEREOF.

  6. This License Agreement will automatically terminate upon a material
  breach of its terms and conditions.

  7. Nothing in this License Agreement shall be deemed to create any
  relationship of agency, partnership, or joint venture between PSF and
  Licensee.  This License Agreement does not grant permission to use PSF
  trademarks or trade name in a trademark sense to endorse or promote
  products or services of Licensee, or any third party.

  8. By copying, installing or otherwise using Python 2.3, Licensee
  agrees to be bound by the terms and conditions of this License
  Agreement.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "lo-mappers.h"
#include "quit.h"
#include "oct-sort.h"

#define IFLT(a,b)  if (compare == NULL ? ((a) < (b)) : compare ((a), (b)))

template <class T>
octave_sort<T>::octave_sort (void) : compare (NULL) 
{ 
  merge_init (); 
  merge_getmem (1024);
}

template <class T>
octave_sort<T>::octave_sort (bool (*comp) (T, T)) : compare (comp) 
{ 
  merge_init (); 
  merge_getmem (1024);
}
  
/* Reverse a slice of a list in place, from lo up to (exclusive) hi. */
template <class T>
void
octave_sort<T>::reverse_slice(T *lo, T *hi)
{
  --hi;
  while (lo < hi) 
    {
      T t = *lo;
      *lo = *hi;
      *hi = t;
      ++lo;
      --hi;
    }
}

template <class T>
void
octave_sort<T>::binarysort (T *lo, T *hi, T *start)
{
  register T *l, *p, *r;
  register T pivot;

  if (lo == start)
    ++start;

  for (; start < hi; ++start) 
    {
      /* set l to where *start belongs */
      l = lo;
      r = start;
      pivot = *r;
      /* Invariants:
       * pivot >= all in [lo, l).
       * pivot  < all in [r, start).
       * The second is vacuously true at the start.
       */
      do 
	{
	  p = l + ((r - l) >> 1);
	  IFLT (pivot, *p)
	    r = p;
	  else
	    l = p+1;
	} 
      while (l < r);
      /* The invariants still hold, so pivot >= all in [lo, l) and
	 pivot < all in [l, start), so pivot belongs at l.  Note
	 that if there are elements equal to pivot, l points to the
	 first slot after them -- that's why this sort is stable.
	 Slide over to make room.
	 Caution: using memmove is much slower under MSVC 5;
	 we're not usually moving many slots. */
      for (p = start; p > l; --p)
	*p = *(p-1);
      *l = pivot;
    }

  return;
}

/*
Return the length of the run beginning at lo, in the slice [lo, hi).  lo < hi
is required on entry.  "A run" is the longest ascending sequence, with

    lo[0] <= lo[1] <= lo[2] <= ...

or the longest descending sequence, with

    lo[0] > lo[1] > lo[2] > ...

Boolean *descending is set to 0 in the former case, or to 1 in the latter.
For its intended use in a stable mergesort, the strictness of the defn of
"descending" is needed so that the caller can safely reverse a descending
sequence without violating stability (strict > ensures there are no equal
elements to get out of order).

Returns -1 in case of error.
*/
template <class T>
int
octave_sort<T>::count_run(T *lo, T *hi, int *descending)
{
  int n;

  *descending = 0;
  ++lo;
  if (lo == hi)
    return 1;

  n = 2;

  IFLT (*lo, *(lo-1))
    {
      *descending = 1;
      for (lo = lo+1; lo < hi; ++lo, ++n) 
	{
	  IFLT (*lo, *(lo-1))
	    ;
	  else
	    break;
	}
    }
  else 
    {
      for (lo = lo+1; lo < hi; ++lo, ++n) 
	{
	  IFLT (*lo, *(lo-1))
	    break;
	}
    }

  return n;
}

/*
Locate the proper position of key in a sorted vector; if the vector contains
an element equal to key, return the position immediately to the left of
the leftmost equal element.  [gallop_right() does the same except returns
the position to the right of the rightmost equal element (if any).]

"a" is a sorted vector with n elements, starting at a[0].  n must be > 0.

"hint" is an index at which to begin the search, 0 <= hint < n.  The closer
hint is to the final result, the faster this runs.

The return value is the int k in 0..n such that

    a[k-1] < key <= a[k]

pretending that *(a-1) is minus infinity and a[n] is plus infinity.  IOW,
key belongs at index k; or, IOW, the first k elements of a should precede
key, and the last n-k should follow key.

Returns -1 on error.  See listsort.txt for info on the method.
*/
template <class T>
int
octave_sort<T>::gallop_left(T key, T *a, int n, int hint)
{
  int ofs;
  int lastofs;
  int k;

  a += hint;
  lastofs = 0;
  ofs = 1;
  IFLT (*a, key)
    {
      /* a[hint] < key -- gallop right, until
       * a[hint + lastofs] < key <= a[hint + ofs]
       */
      const int maxofs = n - hint;	/* &a[n-1] is highest */
      while (ofs < maxofs) 
	{
	  IFLT (a[ofs], key)
	    {
	      lastofs = ofs;
	      ofs = (ofs << 1) + 1;
	      if (ofs <= 0)	/* int overflow */
		ofs = maxofs;
	    }
	  else	/* key <= a[hint + ofs] */
	    break;
	}
      if (ofs > maxofs)
	ofs = maxofs;
      /* Translate back to offsets relative to &a[0]. */
      lastofs += hint;
      ofs += hint;
    }
  else 
    {
      /* key <= a[hint] -- gallop left, until
       * a[hint - ofs] < key <= a[hint - lastofs]
       */
      const int maxofs = hint + 1;	/* &a[0] is lowest */
      while (ofs < maxofs) 
	{
	  IFLT (*(a-ofs), key)
	    break;
	  /* key <= a[hint - ofs] */
	  lastofs = ofs;
	  ofs = (ofs << 1) + 1;
	  if (ofs <= 0)	/* int overflow */
	    ofs = maxofs;
	}
      if (ofs > maxofs)
	ofs = maxofs;
      /* Translate back to positive offsets relative to &a[0]. */
      k = lastofs;
      lastofs = hint - ofs;
      ofs = hint - k;
    }
  a -= hint;

  /* Now a[lastofs] < key <= a[ofs], so key belongs somewhere to the
   * right of lastofs but no farther right than ofs.  Do a binary
   * search, with invariant a[lastofs-1] < key <= a[ofs].
   */
  ++lastofs;
  while (lastofs < ofs) 
    {
      int m = lastofs + ((ofs - lastofs) >> 1);

      IFLT (a[m], key)
	lastofs = m+1;	/* a[m] < key */
      else
	ofs = m;	/* key <= a[m] */
    }
  return ofs;
}

/*
Exactly like gallop_left(), except that if key already exists in a[0:n],
finds the position immediately to the right of the rightmost equal value.

The return value is the int k in 0..n such that

    a[k-1] <= key < a[k]

or -1 if error.

The code duplication is massive, but this is enough different given that
we're sticking to "<" comparisons that it's much harder to follow if
written as one routine with yet another "left or right?" flag.
*/
template <class T>
int
octave_sort<T>::gallop_right(T key, T *a, int n, int hint)
{
  int ofs;
  int lastofs;
  int k;

  a += hint;
  lastofs = 0;
  ofs = 1;
  IFLT (key, *a)
    {
      /* key < a[hint] -- gallop left, until
       * a[hint - ofs] <= key < a[hint - lastofs]
       */
      const int maxofs = hint + 1;	/* &a[0] is lowest */
      while (ofs < maxofs) 
	{
	  IFLT (key, *(a-ofs))
	    {
	      lastofs = ofs;
	      ofs = (ofs << 1) + 1;
	      if (ofs <= 0)	/* int overflow */
		ofs = maxofs;
	    }
	  else	/* a[hint - ofs] <= key */
	    break;
	}
      if (ofs > maxofs)
	ofs = maxofs;
      /* Translate back to positive offsets relative to &a[0]. */
      k = lastofs;
      lastofs = hint - ofs;
      ofs = hint - k;
    }
  else 
    {
      /* a[hint] <= key -- gallop right, until
       * a[hint + lastofs] <= key < a[hint + ofs]
       */
      const int maxofs = n - hint;	/* &a[n-1] is highest */
      while (ofs < maxofs) 
	{
	  IFLT (key, a[ofs])
	    break;
	  /* a[hint + ofs] <= key */
	  lastofs = ofs;
	  ofs = (ofs << 1) + 1;
	  if (ofs <= 0)	/* int overflow */
	    ofs = maxofs;
	}
      if (ofs > maxofs)
	ofs = maxofs;
      /* Translate back to offsets relative to &a[0]. */
      lastofs += hint;
      ofs += hint;
    }
  a -= hint;

  /* Now a[lastofs] <= key < a[ofs], so key belongs somewhere to the
   * right of lastofs but no farther right than ofs.  Do a binary
   * search, with invariant a[lastofs-1] <= key < a[ofs].
   */
  ++lastofs;
  while (lastofs < ofs) 
    {
      int m = lastofs + ((ofs - lastofs) >> 1);

      IFLT (key, a[m])
	ofs = m;	/* key < a[m] */
      else
	lastofs = m+1;	/* a[m] <= key */
    }
  return ofs;
}

/* Conceptually a MergeState's constructor. */
template <class T>
void
octave_sort<T>::merge_init(void)
{
  ms.a = NULL;
  ms.alloced = 0;
  ms.n = 0;
  ms.min_gallop = MIN_GALLOP;
}

/* Free all the temp memory owned by the MergeState.  This must be called
 * when you're done with a MergeState, and may be called before then if
 * you want to free the temp memory early.
 */
template <class T>
void
octave_sort<T>::merge_freemem(void)
{
  if (ms.a)
    free (ms.a);
  ms.alloced = 0;
  ms.a = NULL;
}

static inline int
roundupsize(int n)
{
  unsigned int nbits = 3;
  unsigned int n2 = (unsigned int)n >> 8;

  /* Round up:
   * If n <       256, to a multiple of        8.
   * If n <      2048, to a multiple of       64.
   * If n <     16384, to a multiple of      512.
   * If n <    131072, to a multiple of     4096.
   * If n <   1048576, to a multiple of    32768.
   * If n <   8388608, to a multiple of   262144.
   * If n <  67108864, to a multiple of  2097152.
   * If n < 536870912, to a multiple of 16777216.
   * ...
   * If n < 2**(5+3*i), to a multiple of 2**(3*i).
   *
   * This over-allocates proportional to the list size, making room
   * for additional growth.  The over-allocation is mild, but is
   * enough to give linear-time amortized behavior over a long
   * sequence of appends() in the presence of a poorly-performing
   * system realloc() (which is a reality, e.g., across all flavors
   * of Windows, with Win9x behavior being particularly bad -- and
   * we've still got address space fragmentation problems on Win9x
   * even with this scheme, although it requires much longer lists to
   * provoke them than it used to).
   */
  while (n2) {
    n2 >>= 3;
    nbits += 3;
  }
  return ((n >> nbits) + 1) << nbits;
}

/* Ensure enough temp memory for 'need' array slots is available.
 * Returns 0 on success and -1 if the memory can't be gotten.
 */
template <class T>
int
octave_sort<T>::merge_getmem(int need)
{
  if (need <= ms.alloced)
    return 0;

  need = roundupsize(need); 
  /* Don't realloc!  That can cost cycles to copy the old data, but
   * we don't care what's in the block.
   */
  merge_freemem( );
  ms.a = (T *) malloc (need * sizeof (T));
  if (ms.a) {
    ms.alloced = need;
    return 0;
  }
  merge_freemem( );	/* reset to sane state */
  return -1;
}

#define MERGE_GETMEM(NEED) ((NEED) <= ms.alloced ? 0 :	\
				merge_getmem(NEED))

/* Merge the na elements starting at pa with the nb elements starting at pb
 * in a stable way, in-place.  na and nb must be > 0, and pa + na == pb.
 * Must also have that *pb < *pa, that pa[na-1] belongs at the end of the
 * merge, and should have na <= nb.  See listsort.txt for more info.
 * Return 0 if successful, -1 if error.
 */
template <class T>
int
octave_sort<T>::merge_lo(T *pa, int na, T *pb, int nb)
{
  int k;
  T *dest;
  int result = -1;	/* guilty until proved innocent */
  int min_gallop = ms.min_gallop;

  if (MERGE_GETMEM(na) < 0)
    return -1;
  memcpy(ms.a, pa, na * sizeof(T));
  dest = pa;
  pa = ms.a;

  *dest++ = *pb++;
  --nb;
  if (nb == 0)
    goto Succeed;
  if (na == 1)
    goto CopyB;

  for (;;) {
    int acount = 0;	/* # of times A won in a row */
    int bcount = 0;	/* # of times B won in a row */

    /* Do the straightforward thing until (if ever) one run
     * appears to win consistently.
     */
    for (;;) {

      IFLT (*pb, *pa)
	{
	  *dest++ = *pb++;
	  ++bcount;
	  acount = 0;
	  --nb;
	  if (nb == 0)
	    goto Succeed;
	  if (bcount >= min_gallop)
	    break;
	}
      else 
	{
	  *dest++ = *pa++;
	  ++acount;
	  bcount = 0;
	  --na;
	  if (na == 1)
	    goto CopyB;
	  if (acount >= min_gallop)
	    break;
	}
    }

    /* One run is winning so consistently that galloping may
     * be a huge win.  So try that, and continue galloping until
     * (if ever) neither run appears to be winning consistently
     * anymore.
     */
    ++min_gallop;
    do 
      {
	min_gallop -= min_gallop > 1;
	ms.min_gallop = min_gallop;
	k = gallop_right(*pb, pa, na, 0);
	acount = k;
	if (k) 
	  {
	    if (k < 0)
	      goto Fail;
	    memcpy(dest, pa, k * sizeof(T));
	    dest += k;
	    pa += k;
	    na -= k;
	    if (na == 1)
	      goto CopyB;
	    /* na==0 is impossible now if the comparison
	     * function is consistent, but we can't assume
	     * that it is.
	     */
	    if (na == 0)
	      goto Succeed;
	  }
	*dest++ = *pb++;
	--nb;
	if (nb == 0)
	  goto Succeed;

	k = gallop_left(*pa, pb, nb, 0);
	bcount = k;
	if (k) {
	  if (k < 0)
	    goto Fail;
	  memmove(dest, pb, k * sizeof(T));
	  dest += k;
	  pb += k;
	  nb -= k;
	  if (nb == 0)
	    goto Succeed;
	}
	*dest++ = *pa++;
	--na;
	if (na == 1)
	  goto CopyB;
      } while (acount >= MIN_GALLOP || bcount >= MIN_GALLOP);
    ++min_gallop;	/* penalize it for leaving galloping mode */
    ms.min_gallop = min_gallop;
  }
 Succeed:
  result = 0;
 Fail:
  if (na)
    memcpy(dest, pa, na * sizeof(T));
  return result;
 CopyB:
  /* The last element of pa belongs at the end of the merge. */
  memmove(dest, pb, nb * sizeof(T));
  dest[nb] = *pa;
  return 0;
}

/* Merge the na elements starting at pa with the nb elements starting at pb
 * in a stable way, in-place.  na and nb must be > 0, and pa + na == pb.
 * Must also have that *pb < *pa, that pa[na-1] belongs at the end of the
 * merge, and should have na >= nb.  See listsort.txt for more info.
 * Return 0 if successful, -1 if error.
 */
template <class T>
int
octave_sort<T>::merge_hi(T *pa, int na, T *pb, int nb)
{
  int k;
  T *dest;
  int result = -1;	/* guilty until proved innocent */
  T *basea;
  T *baseb;
  int min_gallop = ms.min_gallop;

  if (MERGE_GETMEM(nb) < 0)
    return -1;
  dest = pb + nb - 1;
  memcpy(ms.a, pb, nb * sizeof(T));
  basea = pa;
  baseb = ms.a;
  pb = ms.a + nb - 1;
  pa += na - 1;

  *dest-- = *pa--;
  --na;
  if (na == 0)
    goto Succeed;
  if (nb == 1)
    goto CopyA;

  for (;;) 
    {
      int acount = 0;	/* # of times A won in a row */
      int bcount = 0;	/* # of times B won in a row */

      /* Do the straightforward thing until (if ever) one run
       * appears to win consistently.
       */
      for (;;) 
	{
	  IFLT (*pb, *pa)
	    {
	      *dest-- = *pa--;
	      ++acount;
	      bcount = 0;
	      --na;
	      if (na == 0)
		goto Succeed;
	      if (acount >= min_gallop)
		break;
	    }
	  else 
	    {
	      *dest-- = *pb--;
	      ++bcount;
	      acount = 0;
	      --nb;
	      if (nb == 1)
		goto CopyA;
	      if (bcount >= min_gallop)
		break;
	    }
	}

      /* One run is winning so consistently that galloping may
       * be a huge win.  So try that, and continue galloping until
       * (if ever) neither run appears to be winning consistently
       * anymore.
       */
      ++min_gallop;
      do 
	{
	  min_gallop -= min_gallop > 1;
	  ms.min_gallop = min_gallop;
	  k = gallop_right(*pb, basea, na, na-1);
	  if (k < 0)
	    goto Fail;
	  k = na - k;
	  acount = k;
	  if (k) 
	    {
	      dest -= k;
	      pa -= k;
	      memmove(dest+1, pa+1, k * sizeof(T ));
	      na -= k;
	      if (na == 0)
		goto Succeed;
	    }
	  *dest-- = *pb--;
	  --nb;
	  if (nb == 1)
	    goto CopyA;

	  k = gallop_left(*pa, baseb, nb, nb-1);
	  if (k < 0)
	    goto Fail;
	  k = nb - k;
	  bcount = k;
	  if (k) 
	    {
	      dest -= k;
	      pb -= k;
	      memcpy(dest+1, pb+1, k * sizeof(T));
	      nb -= k;
	      if (nb == 1)
		goto CopyA;
	      /* nb==0 is impossible now if the comparison
	       * function is consistent, but we can't assume
	       * that it is.
	       */
	      if (nb == 0)
		goto Succeed;
	    }
	  *dest-- = *pa--;
	  --na;
	  if (na == 0)
	    goto Succeed;
	} while (acount >= MIN_GALLOP || bcount >= MIN_GALLOP);
      ++min_gallop;	/* penalize it for leaving galloping mode */
      ms.min_gallop = min_gallop;
    }
Succeed:
  result = 0;
Fail:
  if (nb)
    memcpy(dest-(nb-1), baseb, nb * sizeof(T));
  return result;
CopyA:
  /* The first element of pb belongs at the front of the merge. */
  dest -= na;
  pa -= na;
  memmove(dest+1, pa+1, na * sizeof(T));
  *dest = *pb;
  return 0;
}

/* Merge the two runs at stack indices i and i+1.
 * Returns 0 on success, -1 on error.
 */
template <class T>
int
octave_sort<T>::merge_at(int i)
{
  T *pa, *pb;
  int na, nb;
  int k;

  pa = ms.pending[i].base;
  na = ms.pending[i].len;
  pb = ms.pending[i+1].base;
  nb = ms.pending[i+1].len;

  /* Record the length of the combined runs; if i is the 3rd-last
   * run now, also slide over the last run (which isn't involved
   * in this merge).  The current run i+1 goes away in any case.
   */
  ms.pending[i].len = na + nb;
  if (i == ms.n - 3)
    ms.pending[i+1] = ms.pending[i+2];
  --ms.n;

  /* Where does b start in a?  Elements in a before that can be
   * ignored (already in place).
   */
  k = gallop_right(*pb, pa, na, 0);
  if (k < 0)
    return -1;
  pa += k;
  na -= k;
  if (na == 0)
    return 0;

  /* Where does a end in b?  Elements in b after that can be
   * ignored (already in place).
   */
  nb = gallop_left(pa[na-1], pb, nb, nb-1);
  if (nb <= 0)
    return nb;

  /* Merge what remains of the runs, using a temp array with
   * min(na, nb) elements.
   */
  if (na <= nb)
    return merge_lo(pa, na, pb, nb);
  else
    return merge_hi(pa, na, pb, nb);
}

/* Examine the stack of runs waiting to be merged, merging adjacent runs
 * until the stack invariants are re-established:
 *
 * 1. len[-3] > len[-2] + len[-1]
 * 2. len[-2] > len[-1]
 *
 * See listsort.txt for more info.
 *
 * Returns 0 on success, -1 on error.
 */
template <class T>
int
octave_sort<T>::merge_collapse(void)
{
  struct s_slice *p = ms.pending;

  while (ms.n > 1) 
    {
      int n = ms.n - 2;
      if (n > 0 && p[n-1].len <= p[n].len + p[n+1].len) 
	{
	  if (p[n-1].len < p[n+1].len)
	    --n;
	  if (merge_at(n) < 0)
	    return -1;
	}
      else if (p[n].len <= p[n+1].len) 
	{
	  if (merge_at(n) < 0)
	    return -1;
	}
      else
	break;
    }
  return 0;
}

/* Regardless of invariants, merge all runs on the stack until only one
 * remains.  This is used at the end of the mergesort.
 *
 * Returns 0 on success, -1 on error.
 */
template <class T>
int
octave_sort<T>::merge_force_collapse(void)
{
  struct s_slice *p = ms.pending;

  while (ms.n > 1) 
    {
      int n = ms.n - 2;
      if (n > 0 && p[n-1].len < p[n+1].len)
	--n;
      if (merge_at(n) < 0)
	return -1;
    }
  return 0;
}

/* Compute a good value for the minimum run length; natural runs shorter
 * than this are boosted artificially via binary insertion.
 *
 * If n < 64, return n (it's too small to bother with fancy stuff).
 * Else if n is an exact power of 2, return 32.
 * Else return an int k, 32 <= k <= 64, such that n/k is close to, but
 * strictly less than, an exact power of 2.
 *
 * See listsort.txt for more info.
 */
template <class T>
int
octave_sort<T>::merge_compute_minrun(int n)
{
  int r = 0;	/* becomes 1 if any 1 bits are shifted off */

  while (n >= 64) {
    r |= n & 1;
    n >>= 1;
  }
  return n + r;
}

template <class T>
void
octave_sort<T>::sort (T *v, int elements)
{
  /* Re-initialize the Mergestate as this might be the second time called */
  ms.n = 0;
  ms.min_gallop = MIN_GALLOP;

  if (elements > 1)
    {
      int nremaining = elements; 
      T *lo = v;
      T *hi = v + elements;

      /* March over the array once, left to right, finding natural runs,
       * and extending short natural runs to minrun elements.
       */
      int minrun = merge_compute_minrun(nremaining);
      do 
	{
	  int descending;
	  int n;

	  /* Identify next run. */
	  n = count_run(lo, hi, &descending);
	  if (n < 0)
	    goto fail;
	  if (descending)
	    reverse_slice(lo, lo + n);
	  /* If short, extend to min(minrun, nremaining). */
	  if (n < minrun) 
	    {
	      const int force = nremaining <= minrun ? nremaining : minrun;
	      binarysort(lo, lo + force, lo + n);
	      n = force;
	    }
	  /* Push run onto pending-runs stack, and maybe merge. */
	  assert(ms.n < MAX_MERGE_PENDING);
	  ms.pending[ms.n].base = lo;
	  ms.pending[ms.n].len = n;
	  ++ms.n;
	  if (merge_collapse( ) < 0)
	    goto fail;
	  /* Advance to find next run. */
	  lo += n;
	  nremaining -= n;
	} while (nremaining);

      merge_force_collapse( );
    }

fail:
  return;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
