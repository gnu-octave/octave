/*

  Copyright (C) 2012-2018 Max Brister

  This file is part of Octave.

  Octave is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Octave is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Octave; see the file COPYING.  If not, see
  <https://www.gnu.org/licenses/>.

*/

// Author: Max Brister <max@2bass.com>

// Some utility classes and functions used throughout jit

#if ! defined (octave_jit_util_h)
#define octave_jit_util_h 1

#include "octave-config.h"

#if defined (HAVE_LLVM)

#include <stdexcept>

#if defined (HAVE_LLVM_IR_DATALAYOUT_H) || defined (HAVE_LLVM_DATALAYOUT_H)
#  define HAVE_LLVM_DATALAYOUT
#endif

// we don't want to include llvm headers here, as they require
// __STDC_LIMIT_MACROS and __STDC_CONSTANT_MACROS be defined in the entire
// compilation unit
namespace llvm
{
  class Value;
  class Module;
#if defined (LEGACY_PASSMANAGER)
  namespace legacy
  {
    class FunctionPassManager;
    class PassManager;
  }
#else
  class FunctionPassManager;
  class PassManager;
#endif
  class ExecutionEngine;
  class Function;
  class BasicBlock;
  class LLVMContext;
  class Type;
  class StructType;
  class FunctionType;
  class Twine;
  class GlobalValue;
  class GlobalVariable;
  class TerminatorInst;
  class PHINode;
  class TargetMachine;

  class ConstantFolder;

  template <bool preserveNames>
  class IRBuilderDefaultInserter;

  template <bool preserveNames, typename T, typename Inserter>
  class IRBuilder;

  typedef IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true>>
    IRBuilderD;
}

// some octave classes that are not (yet) in the octave namespace
class octave_base_value;
class octave_builtin;
class octave_value;
class tree;
class tree_expression;

namespace octave
{
  // thrown when we should give up on JIT and interpret
  class jit_fail_exception : public std::runtime_error
  {
  public:

    jit_fail_exception (void)
      : std::runtime_error ("unknown"), m_known (false)
    { }

    jit_fail_exception (const std::string& reason)
      : std::runtime_error (reason), m_known (true)
    { }

    bool known (void) const { return m_known; }

  private:

    bool m_known;
  };

  // llvm doesn't provide this, and it's really useful for debugging
  std::ostream& operator<< (std::ostream& os, const llvm::Value& v);

  template <typename HOLDER_T, typename SUB_T>
  class jit_internal_node;

  // jit_internal_list and jit_internal_node implement generic embedded doubly
  // linked lists.  List items extend from jit_internal_list, and can be placed
  // in nodes of type jit_internal_node.  We use CRTP twice.

  template <typename LIST_T, typename NODE_T>
  class
  jit_internal_list
  {
    friend class jit_internal_node<LIST_T, NODE_T>;

  public:

    jit_internal_list (void)
      : m_use_head (0), m_use_tail (0), m_use_count (0)
    { }

    virtual ~jit_internal_list (void)
    {
      while (m_use_head)
        m_use_head->stash_value (0);
    }

    NODE_T * first_use (void) const { return m_use_head; }

    size_t use_count (void) const { return m_use_count; }

  private:

    NODE_T *m_use_head;
    NODE_T *m_use_tail;
    size_t m_use_count;
  };

  // a node for internal linked lists
  template <typename LIST_T, typename NODE_T>
  class
  jit_internal_node
  {
  public:

    typedef jit_internal_list<LIST_T, NODE_T> jit_ilist;

    jit_internal_node (void)
      : m_value (nullptr), m_next (nullptr), m_prev (nullptr)
    { }

    ~jit_internal_node (void) { remove (); }

    LIST_T * value (void) const { return m_value; }

    void stash_value (LIST_T *avalue)
    {
      remove ();

      m_value = avalue;

      if (m_value)
        {
          jit_ilist *ilist = m_value;
          NODE_T *sthis = static_cast<NODE_T *> (this);
          if (ilist->m_use_head)
            {
              ilist->m_use_tail->m_next = sthis;
              m_prev = ilist->m_use_tail;
            }
          else
            ilist->m_use_head = sthis;

          ilist->m_use_tail = sthis;
          ++ilist->m_use_count;
        }
    }

    NODE_T * next (void) const { return m_next; }

    NODE_T * prev (void) const { return m_prev; }

  private:

    void remove (void)
    {
      if (m_value)
        {
          jit_ilist *ilist = m_value;
          if (m_prev)
            m_prev->m_next = m_next;
          else
            // we are the use_head
            ilist->m_use_head = m_next;

          if (m_next)
            m_next->m_prev = m_prev;
          else
            // we are the use tail
            ilist->m_use_tail = m_prev;

          m_next = m_prev = 0;
          --ilist->m_use_count;
          m_value = 0;
        }
    }

    LIST_T *m_value;
    NODE_T *m_next;
    NODE_T *m_prev;
  };

  // Use like: isa<jit_phi> (value)
  // basically just a short cut type typing dyanmic_cast.
  template <typename T, typename U>
  bool isa (U *value)
  {
    return dynamic_cast<T *> (value);
  }

}

#endif
#endif
