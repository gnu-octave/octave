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

#if !defined (octave_sort_h)
#define octave_sort_h 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "lo-mappers.h"
#include "quit.h"

/* The maximum number of entries in a MergeState's pending-runs stack.
 * This is enough to sort arrays of size up to about
 *     32 * phi ** MAX_MERGE_PENDING
 * where phi ~= 1.618.  85 is ridiculously large enough, good for an array
 * with 2**64 elements.
 */
#define MAX_MERGE_PENDING 85

/* When we get into galloping mode, we stay there until both runs win less
 * often than MIN_GALLOP consecutive times.  See listsort.txt for more info.
 */
#define MIN_GALLOP 7

/* Avoid malloc for small temp arrays. */
#define MERGESTATE_TEMP_SIZE 1024

template <class T>
class
octave_sort
{
public:

  octave_sort (void);

  octave_sort (bool (*comp) (T, T));
  
  ~octave_sort (void) { merge_freemem (); }

  void set_compare (bool (*comp) (T, T)) { compare = comp; }

  void sort (T *v, int elements);

private:

  /* One MergeState exists on the stack per invocation of mergesort.  It's just
   * a convenient way to pass state around among the helper functions.
   *
   * DGB: This isn't needed with mergesort in a class, but it doesn't slow
   *      things up, and it is likely to make my life easier for any potential
   *      backporting of changes in the Python code.
   */
  
  struct s_slice 
  {
    T *base;
    int len;
  };
  
  typedef struct s_MergeState 
  {
    /* This controls when we get *into* galloping mode.  It's initialized
     * to MIN_GALLOP.  merge_lo and merge_hi tend to nudge it higher for
     * random data, and lower for highly structured data.
     */
    int min_gallop;

    /* 'a' is temp storage to help with merges.  It contains room for
     * alloced entries.
     */
    T *a;	/* may point to temparray below */
    int alloced;
    
    /* A stack of n pending runs yet to be merged.  Run #i starts at
     * address base[i] and extends for len[i] elements.  It's always
     * true (so long as the indices are in bounds) that
     *
     *     pending[i].base + pending[i].len == pending[i+1].base
     *
     * so we could cut the storage for this, but it's a minor amount,
     * and keeping all the info explicit simplifies the code.
     */
    int n;
    struct s_slice pending[MAX_MERGE_PENDING];
  } MergeState;

  bool (*compare)(T, T);
  
  MergeState ms;
  
  void reverse_slice (T *lo, T *hi);
  
  void binarysort (T *lo, T *hi, T *start);
    
  int count_run(T *lo, T *hi, int *descending);

  int gallop_left(T key, T *a, int n, int hint);

  int gallop_right(T key, T *a, int n, int hint);

  void merge_init(void);

  void merge_freemem(void);

  int merge_getmem(int need);

  int merge_lo(T *pa, int na, T *pb, int nb);

  int merge_hi(T *pa, int na, T *pb, int nb);

  int merge_at(int i);

  int merge_collapse(void);

  int merge_force_collapse(void);

  int merge_compute_minrun(int n);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
