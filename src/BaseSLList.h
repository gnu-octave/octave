// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988, 1992 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef _BaseSLList_h
#define _BaseSLList_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#undef OK

#include <Pix.h>

struct BaseSLNode
{
   union {
     struct BaseSLNode *tl;
     double dummy;  /* To force correct alignment */
   };
   void *item() {return (void*)(this+1);} // Return ((SLNode<T>*)this)->hd
};

class
BaseSLList
{
  protected:
    BaseSLNode *last;
    virtual void delete_node(BaseSLNode*node) = 0;
    virtual BaseSLNode* copy_node(const void* datum) = 0;
    virtual void copy_item(void *dst, void *src) = 0;
    virtual ~BaseSLList() { }
    BaseSLList() { last = 0; }
    void copy(const BaseSLList&);
    BaseSLList& operator = (const BaseSLList& a);
    Pix ins_after(Pix p, const void *datum);
    Pix prepend(const void *datum);
    Pix append(const void *datum);
    int remove_front(void *dst, int signal_error = 0);
    void join(BaseSLList&);
  public:
    int length() const;
    int empty() const { return last == 0; }
    void clear();
    Pix                   prepend(BaseSLNode*);
    Pix                   append(BaseSLNode*);
    int                   OK() const;
    void                  error(const char* msg) const;
    void                  del_after(Pix p);
    int                   owns(Pix p) const;
    void                  del_front();
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
