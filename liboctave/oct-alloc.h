/*

Copyright (C) 1996, 1997, 1998, 2002, 2003, 2005, 2006, 2007
              John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_oct_alloc_h)
#define octave_oct_alloc_h 1

class
OCTAVE_API
octave_allocator
{
public:

  octave_allocator (size_t item_sz, int grow_sz = 256)
    : head (0), grow_size (grow_sz),
      item_size (item_sz > sizeof (link *) ? item_sz : sizeof (link *))
  { }

  // Get an object from the free list, possibly increasing the size of
  // the free list.
  void *alloc (size_t size);

  // Put objects back on the free list.
  void free (void *p, size_t size);

private:

  // Structure for internal free list management.
  struct link { link *next; };

  // Front of the free list.
  link *head;

  // How many objects to get each time we call the global operator new.
  int grow_size;

  // The size of each item on the list (or, if that is smaller than
  // the size of list*, the size of list*.
  size_t item_size;

  // How to grow the free list.
  bool grow (void);
};

#if defined (HAVE_PLACEMENT_DELETE)
#define DECLARE_OCTAVE_ALLOCATOR_PLACEMENT_DELETE \
    void operator delete (void *p, void *) \
      { ::operator delete (p, static_cast<void*> (0)); }
#else
#define DECLARE_OCTAVE_ALLOCATOR_PLACEMENT_DELETE \
    void operator delete (void *p, void *) \
      { ::operator delete (p); }
#endif

#define DECLARE_OCTAVE_ALLOCATOR \
  public: \
    void *operator new (size_t size, void *p) \
      { return ::operator new (size, p); } \
    DECLARE_OCTAVE_ALLOCATOR_PLACEMENT_DELETE \
    void *operator new (size_t size) { return allocator.alloc (size); } \
    void operator delete (void *p, size_t size) { allocator.free (p, size); } \
  private: \
    static octave_allocator allocator;

#define DEFINE_OCTAVE_ALLOCATOR(t) \
  octave_allocator t::allocator (sizeof (t))

#define DEFINE_OCTAVE_ALLOCATOR2(t, s) \
  octave_allocator t::allocator (sizeof (t), s)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
