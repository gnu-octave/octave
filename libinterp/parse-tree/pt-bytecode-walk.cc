////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023 The Octave Project Developers
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

#include "pt-all.h"
#include "pt-bytecode-walk.h"
#include "symrec.h"
#include "pt-walk.h"
#include "ov-scalar.h"

//#pragma GCC optimize("Og")

using namespace octave;

void octave::compile_user_function (octave_user_function &ufn, bool do_print)
{
  try
    {
      if (ufn.is_classdef_constructor ())
        error ("Classdef constructors are not supported by the VM yet"); // Needs special handling
      if (ufn.is_inline_function () || ufn.is_nested_function ())
        error ("Inlined or scoped functions are not supported by the VM yet");

      // Begin with clearing the old bytecode, if any
      ufn.clear_bytecode ();

      bytecode_walker bw;

      ufn.accept (bw);

      if (do_print)
        print_bytecode (bw.m_code);

      ufn.set_bytecode (bw.m_code);

      // Compile the subfunctions
      auto subs = ufn.subfunctions ();
      for (auto kv : subs)
        {
          octave_user_function *sub = kv.second.user_function_value ();
          compile_user_function (*sub, do_print);
          sub->get_bytecode ().m_unwind_data.m_file = ufn.fcn_file_name ();
        }
    }
  catch(...)
  {
    ufn.clear_bytecode ();
    throw;
  }
}

// Class to walk the tree and see if a index expression has
// an end in it.
//
// Does not walk nested index expressions.
class find_end_walker : tree_walker
{
public:
  static bool has_end (tree &e)
  {
    find_end_walker walker;
    e.accept (walker);

    return walker.m_has_end;
  }

  bool m_has_end = false;

  void visit_identifier (tree_identifier &id)
  {
    std::string name = id.name ();
    if (name == "end")
      m_has_end = true;
  }
};

class is_foldable_walker : tree_walker
{
public:
  static bool is_foldable (tree_binary_expression &e)
  {
    return is_foldable_internal (e);
  }

  static bool is_foldable (tree_prefix_expression &e)
  {
    return is_foldable_internal (e);
  }

  static bool is_foldable (tree_postfix_expression &e)
  {
    return is_foldable_internal (e);
  }

private:
  static bool is_foldable_internal (tree &e)
  {
    is_foldable_walker walker;

    e.accept (walker);

    return walker.m_is_foldable;
  }

  bool is_foldable_expr (tree_expression *e)
  {
    return e->is_binary_expression () || e->is_unary_expression () || e->is_constant ();
  }

  void visit_postfix_expression (tree_postfix_expression& e)
  {
    if (!m_is_foldable)
      return;

    tree_expression *op = e.operand ();

    if (!is_foldable_expr (op))
      {
        m_is_foldable = false;
        return;
      }

    op->accept (*this);
  }

  void visit_prefix_expression (tree_prefix_expression& e)
  {
    if (!m_is_foldable)
      return;

    tree_expression *op = e.operand ();

    if (!is_foldable_expr (op))
      {
        m_is_foldable = false;
        return;
      }

    op->accept (*this);
  }

  void visit_binary_expression (tree_binary_expression &e)
  {
    if (!m_is_foldable)
      return;

    tree_expression *rhs = e.rhs ();
    tree_expression *lhs = e.lhs ();
    if (!is_foldable_expr (rhs) || !is_foldable_expr (lhs))
      {
        m_is_foldable = false;
        return;
      }

    lhs->accept (*this);
    if (m_is_foldable)
      rhs->accept (*this);
  }

  bool m_is_foldable = true;
};

class collect_idnames_walker : tree_walker
{
public:
  static std::vector<std::pair<std::string, int>> collect_id_names (tree_statement_list &l)
  {
    collect_idnames_walker walker;

    for (auto it = l.begin (); it != l.end (); it++)
      {
        if (*it)
          (*it)->accept (walker);
      }

    return walker.m_id_names_and_offset;
  }

  static std::vector<std::pair<std::string, int>> collect_id_names (tree_expression &e)
  {
    collect_idnames_walker walker;
    e.accept (walker);

    return walker.m_id_names_and_offset;
  }

  std::vector<std::pair<std::string, int>> m_id_names_and_offset;

  void visit_identifier (tree_identifier &id)
  {
    std::string name = id.name ();
    if (name == "~") // We dont want this magic id
      return;

    m_id_names_and_offset.push_back ({name, id.symbol ().data_offset ()});
  }

  void visit_anon_fcn_handle (tree_anon_fcn_handle &)
  {
    // We dont collect any id:s in the handle, since the original scope
    // don't.
  }
};

template <class T>
typename T::value_type vector_pop (T &v)
{
  typename T::value_type tmp = v.back ();
  v.pop_back ();
  return tmp;
}

#define ERR(msg) error("VM error %d: " msg, __LINE__)

#define TODO(msg) error("VM error, Not done yet %d: " msg, __LINE__)

#define CHECK(cond)                                                            \
  do {                                                                         \
    if (!(cond))                                                               \
      ERR("Internal VM compiler consistency check failed, " #cond);             \
  } while ((0))

#define PUSH_CODE(code_) do {\
    int code_check_s_ = static_cast<int> (code_); \
    unsigned char code_s_ = static_cast<unsigned char> (code_check_s_);     \
    CHECK (code_check_s_ < 256 && code_check_s_ >= -128); \
    m_code.m_code.push_back(code_s_);        \
  } while ((0))

#define PUSH_CODE_LOAD_CST(offset) do {\
  unsigned offset_ = offset; \
  if (offset_ < 65536)\
    {\
      if (offset_ >= 256) \
        PUSH_CODE (INSTR::WIDE); \
      emit_alt (m_cnt_alts_cst, {INSTR::LOAD_CST, INSTR::LOAD_CST_ALT2, \
        INSTR::LOAD_CST_ALT3, INSTR::LOAD_CST_ALT4});\
      if (offset_ >= 256) \
        PUSH_CODE_SHORT (offset_);\
      else\
        PUSH_CODE (offset_);\
    }\
  else\
    {\
      PUSH_CODE (INSTR::LOAD_FAR_CST);\
      PUSH_CODE_INT (offset_);\
    }\
} while (0)

#define PUSH_SSLOT(sslot) PUSH_CODE(sslot)
#define PUSH_WSLOT(wslot) PUSH_CODE_SHORT(wslot)
#define NEED_WIDE_SLOTS() (m_map_locals_to_slot.size () >= 256)

#define MAYBE_PUSH_WIDE_OPEXT(slot) \
do {\
  if (slot >= 256)\
    PUSH_CODE (INSTR::WIDE);\
} while ((0))

#define PUSH_SLOT(slot) \
do {\
  if (slot >= 256)\
    PUSH_WSLOT (slot);\
  else\
    PUSH_SSLOT (slot);\
} while ((0))

#define CODE_SIZE() m_code.m_code.size()
#define CODE(x) m_code.m_code[x]
#define PUSH_CODE_SHORT(code_) do {   \
  unsigned u = code_;                 \
  unsigned char b0 = u & 0xFF;        \
  unsigned char b1 = (u >> 8) & 0xFF; \
  int code_check_ss_ = static_cast<int> (u);                  \
  CHECK (code_check_ss_ < 65536 && code_check_ss_ >= -32768); \
  PUSH_CODE (b0);                     \
  PUSH_CODE (b1);                     \
  } while ((0))
#define PUSH_CODE_INT(code_) do {   \
  unsigned u = code_;                 \
  unsigned char b0 = u & 0xFF;        \
  unsigned char b1 = (u >> 8) & 0xFF; \
  unsigned char b2 = (u >> 16) & 0xFF;\
  unsigned char b3 = (u >> 24) & 0xFF;\
  PUSH_CODE (b0);                     \
  PUSH_CODE (b1);                     \
  PUSH_CODE (b2);                     \
  PUSH_CODE (b3);                     \
  } while ((0))

#define SET_CODE_SHORT(offset, value) do {  \
  int tmp = offset;                         \
  unsigned u = value;                       \
  unsigned char b0 = u & 0xFF;              \
  unsigned char b1 = (u >> 8) & 0xFF;       \
  int code_check_s_ = static_cast<int> (u); \
  CHECK (code_check_s_ < 65536 && code_check_s_ >= -32768); \
  CODE (tmp) = b0;                          \
  CODE (tmp + 1) = b1;                      \
  } while ((0))

#define PUSH_DATA(cst) m_code.m_data.push_back(cst)
#define DATA_SIZE() m_code.m_data.size()

// TODO: This optimization is nice and we should get it working again.
#define PUSH_ALL_PATHS_TERMINATED() m_all_paths_terminated.push_back (false)
#define POP_ALL_PATHS_TERMINATED() vector_pop (m_all_paths_terminated)
#define PEEK_ALL_PATHS_TERMINATED() m_all_paths_terminated.back ()
#define SET_ALL_PATHS_TERMINATED() m_all_paths_terminated.back () = true

#define PUSH_BREAKS() m_need_break_target.push_back ({})
#define POP_BREAKS() vector_pop (m_need_break_target)
#define PUSH_NEED_BREAK(offset) m_need_break_target.back ().push_back (offset)
#define N_BREAKS() m_need_break_target.size ()

#define PUSH_CONTINUE_TARGET(target) m_continue_target.push_back ({})
#define POP_CONTINUE_TARGET() vector_pop (m_continue_target)
#define PUSH_NEED_CONTINUE_TARGET(offset) \
  m_continue_target.back ().push_back (offset)

#define SLOT(name) get_slot (name)

#define CHECK_NONNULL(ptr) if (!ptr) error ("Unexpected null %d", __LINE__)

#define PUSH_ID_BEGIN_INDEXED(slot, idx, narg, is_obj) \
  m_indexed_id.push_back ({slot, idx, narg, is_obj})
#define POP_ID_BEING_INDEXED() m_indexed_id.pop_back ()
#define ID_IS_BEING_INDEXED() (m_indexed_id.size () != 0)
#define N_IDS_BEING_INDEXED() (m_indexed_id.size ())
#define PEEK_ID_BEING_INDEXED() m_indexed_id.back ()
#define IDS_BEING_INDEXED(idx) m_indexed_id[idx]

#define PUSH_NESTING_STATEMENT(type) m_nesting_statement.push_back (type)
#define POP_NESTING_STATEMENT() m_nesting_statement.pop_back ()
#define NESTING_STATEMENTS() m_nesting_statement

// Track how many expression deep we are in the walk.
// I.e. identifiers need to know if they are:
//   foo; %depth 1
// or
//   foo * 2; %depth 2 for id foo
//
// so that nargout for a command call at root is zero.
// E.g.:
//   tic;
#define INC_DEPTH() ++m_depth
#define DEC_DEPTH() --m_depth
#define DEPTH() m_depth

// We need to track the expected amount of output variables
// for each expression. E.g.:
// foo(); %0
// a = foo (); %1
// [a b] = foo (); %2
// [a b] = foo (foo () + foo ()); %2 for outer, 1 for inner foo

#define NARGOUT() m_nargout.back ()
#define PUSH_NARGOUT(nargout) m_nargout.push_back (nargout)
#define POP_NARGOUT() vector_pop (m_nargout)

#define PUSH_ARGNAMES_ENTRY(arg_nm_e) m_code.m_unwind_data.m_argname_entries.push_back (arg_nm_e)

#define PUSH_UNWIND_RETURN_TARGETS() m_need_unwind_target.push_back ({})
#define POP_UNWIND_RETURN_TARGET() vector_pop (m_need_unwind_target)
#define N_UNWIND_RETURN_TARGETS() m_need_unwind_target.size ()
#define PUSH_A_UNWIND_RETURN_TARGET(offset) \
  m_need_unwind_target.back ().push_back (offset)

#define PUSH_LOC() m_code.m_unwind_data.m_loc_entry.push_back ({})
#define LOC(i) m_code.m_unwind_data.m_loc_entry[i]
#define N_LOC() m_code.m_unwind_data.m_loc_entry.size ()

#define PUSH_UNWIND() m_code.m_unwind_data.m_unwind_entries.push_back ({})
#define UNWIND(i) m_code.m_unwind_data.m_unwind_entries[i]
#define N_UNWIND() m_code.m_unwind_data.m_unwind_entries.size ()

#define PUSH_GLOBAL(name) do {m_map_id_is_global[name] = 1;} while ((0))
#define IS_GLOBAL(name) (m_map_id_is_global.find (name) !=\
                                                  m_map_id_is_global.end ())

#define PUSH_PERSISTENT(name) do {m_map_id_is_persistent[name] = 1;} while ((0))
#define IS_PERSISTENT(name) (m_map_id_is_persistent.find (name) !=\
                             m_map_id_is_persistent.end ())

// Note that the placement of PUSH_TREE_FOR_DBG() need to mirror the walk in pt-bp.cc
#define PUSH_TREE_FOR_DBG(ptree) do { m_code.m_unwind_data.m_ip_to_tree[CODE_SIZE ()] = ptree; } while(0)
#define PUSH_TREE_FOR_EVAL(ptree) do { m_code.m_unwind_data.m_ip_to_tree[-CODE_SIZE ()] = ptree; } while(0)

void
bytecode_walker::
visit_statement_list (tree_statement_list& lst)
{
  for (tree_statement *elt : lst)
    {
      CHECK_NONNULL (elt);

      PUSH_NARGOUT (0);

      elt->accept (*this);
      POP_NARGOUT ();
    }
}

void
bytecode_walker::
visit_statement (tree_statement& stmt)
{
  if (stmt.is_expression ())
    {
      int loc_id = N_LOC ();
      PUSH_LOC ();
      LOC (loc_id).m_ip_start = CODE_SIZE ();

      tree_expression *expr = stmt.expression ();
      PUSH_TREE_FOR_DBG(expr);
      CHECK_NONNULL (expr);
      expr->accept (*this);

      LOC (loc_id).m_ip_end = CODE_SIZE ();
      LOC (loc_id).m_col = stmt.column ();
      LOC (loc_id).m_line = stmt.line ();
    }
  else if (stmt.is_command ())
    {
      tree_command *cmd = stmt.command ();
      CHECK_NONNULL (cmd);
      cmd->accept (*this);
    }
  else
    TODO ();
}

void
bytecode_walker::emit_alt (int &cntr, std::vector<INSTR> alts)
{
  unsigned n = alts.size ();
  unsigned offset = cntr++ % n;
  PUSH_CODE (alts [offset]);
}

bytecode_walker::emit_unwind_protect_data
bytecode_walker::emit_unwind_protect_code_start ()
{
  emit_unwind_protect_data D; // Keeps track of state for emit_unwind_protect_code_before_cleanup() and emit_unwind_protect_code_end()

    // Unwind protect has a body and cleanup part that always
  // is executed.
  //
  // If the VM is unwinding it enters the cleanup code with an
  // error object on the stack. The body puts a nil object on the
  // stack.
  //
  // If there is an error object on the stack at the end of the cleanup
  // code it rethrows it.
  //
  // Returns in the body jumps to the cleanup code before actually returning.
  // If a return is reached in the body, a true object is pushed to the stack,
  // which is checked in the cleanup code to know if we are falling through or
  // are supposed to return.
  //
  // The same applies to breaks, so the code underneath gets abit messy.
  //
  // The body_expr, cleanup_expr and cleanup_instructions parameters are for
  // when we need to emit some internal cleanup code that has no corrensponding
  // unwind_protect in the user code.

  int unwind_idx = N_UNWIND ();
  PUSH_UNWIND();

  UNWIND (unwind_idx).m_ip_start = CODE_SIZE ();

  UNWIND (unwind_idx).m_unwind_entry_type =
    unwind_entry_type::UNWIND_PROTECT;

  UNWIND (unwind_idx).m_stack_depth = n_on_stack_due_to_stmt();

  // Returns need to execute the unwind cleanup code before
  // returning, so we need to keep track of offsets that need
  // to jump to the cleanup code.
  PUSH_UNWIND_RETURN_TARGETS ();

  // We need to store away the pending "need breaks" since any break in the
  // unwind protect body need to jump to the cleanup code.
  std::vector<int> v_need_breaks_initial;
  bool break_stack_populated = N_BREAKS ();
  if (break_stack_populated)
    {
      v_need_breaks_initial = POP_BREAKS ();
      PUSH_BREAKS ();
    }

  D.m_break_stack_populated = break_stack_populated;
  D.m_idx_unwind = unwind_idx;
  D.m_v_need_breaks_initial = v_need_breaks_initial;

  return D;
}

void
bytecode_walker::emit_unwind_protect_code_before_cleanup (emit_unwind_protect_data &D)
{
  // If the vm is unwinding it will push an error object to
  // the stack. If we are just done executing the body we
  // push a nil ov to the stack.
  //
  // At the end of the cleanup code we check the ov on the stack
  // and continue to unwind if it is an error object, otherwise
  // just execute the next instruction.
  PUSH_CODE (INSTR::PUSH_NIL);
  // For unwinding we need to keep track of the ov we pushed.
  PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);

  auto v_need_cleanup = POP_UNWIND_RETURN_TARGET ();
  int n_need_cleanup = v_need_cleanup.size ();

  std::vector<int> v_need_break;
  if (D.m_break_stack_populated)
    v_need_break = POP_BREAKS();

  int n_need_break = v_need_break.size ();

  // If there is a return statement inside the unwind body it need
  // to jump to the cleanup code before the actual return. The return
  // statement pushed a true ov to the stack, which is checked at the end of the
  // cleanup code, since we use the same code for just falling throught too.
  //
  // The same applies to breaks, and also the combination of a possibility of
  // breaks and returns.
  int n_falses = 0;
  if (n_need_break && n_need_cleanup)
    {
      n_falses = 2;
      // These nils ov is the break and return  marker if we are falling
      // through to the cleanup code from the body.
      // We have an error object on the stack.
      PUSH_CODE (INSTR::PUSH_FALSE); // return marker
      PUSH_CODE (INSTR::PUSH_FALSE); // break marker
      PUSH_CODE (INSTR::JMP);
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // For unwinding we need to keep track of the ovs we pushed.
      PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);
      PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);

      // Set the offset for all the break jumps that need to go to here
      for (int need : v_need_break)
        SET_CODE_SHORT (need, CODE_SIZE ());


      PUSH_CODE (INSTR::PUSH_NIL); // error object
      PUSH_CODE (INSTR::PUSH_FALSE); // return marker
      PUSH_CODE (INSTR::PUSH_TRUE);// break marker
      PUSH_CODE (INSTR::JMP);
      int also_need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // Set the offset for all the return jumps that need to go to here
      for (int need_cleanup : v_need_cleanup)
        SET_CODE_SHORT (need_cleanup, CODE_SIZE ());

      PUSH_CODE (INSTR::PUSH_NIL); // error object
      PUSH_CODE (INSTR::PUSH_TRUE);// return marker
      PUSH_CODE (INSTR::PUSH_FALSE); // break marker

      // If we were falling throught the body to the cleanup we jump to here
      SET_CODE_SHORT(need_after, CODE_SIZE ());
      SET_CODE_SHORT(also_need_after, CODE_SIZE ());
    }
  else if (n_need_break)
    {
      n_falses = 1;
      // This nil ov is the break marker if we are falling through to the
      // cleanup code from the body. We have an error object on the stack.
      PUSH_CODE (INSTR::PUSH_FALSE); // break marker
      PUSH_CODE (INSTR::JMP);
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // For unwinding we need to keep track of the ov we pushed.
      PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);

      // Set the offset for all the break jumps that need to go to here
      for (int need : v_need_break)
        SET_CODE_SHORT (need, CODE_SIZE ());

      PUSH_CODE (INSTR::PUSH_NIL); // error object
      PUSH_CODE (INSTR::PUSH_TRUE);// break marker

      SET_CODE_SHORT(need_after, CODE_SIZE ());
    }
  else if (n_need_cleanup)
    {
      n_falses = 1;
      // This nil ov is the return marker if we are falling through to the
      // cleanup code from the body
      PUSH_CODE (INSTR::PUSH_FALSE); // return marker
      PUSH_CODE (INSTR::JMP); // We need to skip the pushes for the returns
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // For unwinding we need to keep track of the ov we pushed.
      PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);

      // Set the offset for all the jumps that need to go to here
      for (int need_cleanup : v_need_cleanup)
        SET_CODE_SHORT (need_cleanup, CODE_SIZE ());

      PUSH_CODE (INSTR::PUSH_NIL); // error object
      PUSH_CODE (INSTR::PUSH_TRUE);// return marker

      SET_CODE_SHORT(need_after, CODE_SIZE ());
    }

  // This is the end of protected code
  UNWIND (D.m_idx_unwind).m_ip_end = CODE_SIZE ();

  if (n_falses)
    {
      // Fallthrough code do not need false pushes
      PUSH_CODE (INSTR::JMP);
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);
      // An unwind will jump to here and needs some
      // falses pushed to mark return or breaks
      UNWIND (D.m_idx_unwind).m_ip_target = CODE_SIZE ();
      for (int i = 0; i < n_falses; i++)
        PUSH_CODE (INSTR::PUSH_FALSE);

      SET_CODE_SHORT(need_after, CODE_SIZE ());
    }
  else
    UNWIND (D.m_idx_unwind).m_ip_target = CODE_SIZE ();

  // The body will just fall through into the unwind clean up code

  // There might be breaks in the cleanup code too
  if (D.m_break_stack_populated)
    PUSH_BREAKS();

  D.m_n_need_break = n_need_break;
  D.m_n_need_cleanup = n_need_cleanup;
}

void
bytecode_walker::emit_unwind_protect_code_end (emit_unwind_protect_data &D)
{
  std::vector<int> v_need_break_cleanup;
  if (D.m_break_stack_populated)
    v_need_break_cleanup = POP_BREAKS ();

  if (v_need_break_cleanup.size ())
    TODO ("break in cleanup code");

  if (D.m_break_stack_populated)
    {
      // Restore the initial "need breaks"
      PUSH_BREAKS ();
      for (int offset : D.m_v_need_breaks_initial)
        PUSH_NEED_BREAK (offset);
    }

  if (D.m_n_need_break && D.m_n_need_cleanup)
    TODO ("Return and break nested");
  if (D.m_n_need_break)
    {
      // The break ov marker is on the stack.
      // If it is not true, we skip the break jump
      PUSH_CODE (INSTR::JMP_IFN);
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      POP_NESTING_STATEMENT (); // The jump ate the break marker

      PUSH_CODE (INSTR::POP); // Pop the error object

      // So, we break jump from here.
      // The visitor for the loop will write to proper target
      PUSH_CODE (INSTR::JMP);
      int need_break = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);
      PUSH_NEED_BREAK (need_break); // Nesting loop need to know

      // If we are not breaking we jump to here
      SET_CODE_SHORT (need_after, CODE_SIZE ());
    }
  // Check if we are doing a return unwind
  else if (D.m_n_need_cleanup)
    {
      // If we are in another unwind protect we need to jump to its cleanup
      // code if the return ov marker is true
      if (N_UNWIND_RETURN_TARGETS())
        {
          // The return ov marker is on the stack.
          // If it is not true, we skip the "jump bridge"
          PUSH_CODE (INSTR::JMP_IFN);
          int need_after = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);

          POP_NESTING_STATEMENT (); // The jump ate the return marker

          PUSH_CODE (INSTR::POP); // Pop the error object

          PUSH_CODE (INSTR::JMP); // Jump to the nesting unwind protect
          int need_unwind = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);
          PUSH_A_UNWIND_RETURN_TARGET (need_unwind);

          // If we are not returning we jump to here
          SET_CODE_SHORT (need_after, CODE_SIZE ());
        }
      // Return if the return marker on the stack is true
      else
        {
          // The return ov marker is on the stack.
          // If it is not true, we skip the return
          PUSH_CODE (INSTR::JMP_IFN);
          int need_after = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);

          POP_NESTING_STATEMENT (); // The jump ate the return marker

          // Generate code for a return
          emit_return ();

          // If we are not returning we jump to here
          SET_CODE_SHORT (need_after, CODE_SIZE ());
        }
    }
  // If there is an error object on top of the stack we
  // need to continue unwinding.
  PUSH_CODE (INSTR::THROW_IFERROBJ);

  POP_NESTING_STATEMENT ();
}

void
bytecode_walker::
emit_unwind_protect_code (tree_statement_list *body,
                          tree_statement_list *cleanup_code,
                          tree_expression *body_expr,
                          tree_expression *cleanup_expr,
                          std::vector<int> cleanup_instructions)
{
  // Unwind protect has a body and cleanup part that always
  // is executed.
  //
  // If the VM is unwinding it enters the cleanup code with an
  // error object on the stack. The body puts a nil object on the
  // stack.
  //
  // If there is an error object on the stack at the end of the cleanup
  // code it rethrows it.
  //
  // Returns in the body jumps to the cleanup code before actually returning.
  // If a return is reached in the body, a true object is pushed to the stack,
  // which is checked in the cleanup code to know if we are falling through or
  // are supposed to return.
  //
  // The same applies to breaks, so the code underneath gets abit messy.
  //
  // The body_expr, cleanup_expr and cleanup_instructions parameters are for
  // when we need to emit some internal cleanup code that has no corrensponding
  // unwind_protect in the user code.

  int unwind_idx = N_UNWIND ();
  PUSH_UNWIND();

  UNWIND (unwind_idx).m_ip_start = CODE_SIZE ();

  UNWIND (unwind_idx).m_unwind_entry_type =
    unwind_entry_type::UNWIND_PROTECT;

  UNWIND (unwind_idx).m_stack_depth = n_on_stack_due_to_stmt();

  // Returns need to execute the unwind cleanup code before
  // returning, so we need to keep track of offsets that need
  // to jump to the cleanup code.
  PUSH_UNWIND_RETURN_TARGETS ();

  // We need to store away the pending "need breaks" since any break in the
  // unwind protect body need to jump to the cleanup code.
  std::vector<int> v_need_breaks_initial;
  bool break_stack_populated = N_BREAKS ();
  if (break_stack_populated)
    {
      v_need_breaks_initial = POP_BREAKS ();
      PUSH_BREAKS ();
    }

  // Walk the body
  if (body)
    body->accept (*this);
  if (body_expr)
    body_expr->accept (*this);

  // If the vm is unwinding it will push an error object to
  // the stack. If we are just done executing the body we
  // push a nil ov to the stack.
  //
  // At the end of the cleanup code we check the ov on the stack
  // and continue to unwind if it is an error object, otherwise
  // just execute the next instruction.
  PUSH_CODE (INSTR::PUSH_NIL);
  // For unwinding we need to keep track of the ov we pushed.
  PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);

  auto v_need_cleanup = POP_UNWIND_RETURN_TARGET ();
  int n_need_cleanup = v_need_cleanup.size ();

  std::vector<int> v_need_break;
  if (break_stack_populated)
    v_need_break = POP_BREAKS();

  int n_need_break = v_need_break.size ();

  // If there is a return statement inside the unwind body it need
  // to jump to the cleanup code before the actual return. The return
  // statement pushed a true ov to the stack, which is checked at the end of the
  // cleanup code, since we use the same code for just falling throught too.
  //
  // The same applies to breaks, and also the combination of a possibility of
  // breaks and returns.
  int n_falses = 0;
  if (n_need_break && n_need_cleanup)
    {
      n_falses = 2;
      // These nils ov is the break and return  marker if we are falling
      // through to the cleanup code from the body.
      // We have an error object on the stack.
      PUSH_CODE (INSTR::PUSH_FALSE); // return marker
      PUSH_CODE (INSTR::PUSH_FALSE); // break marker
      PUSH_CODE (INSTR::JMP);
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // For unwinding we need to keep track of the ovs we pushed.
      PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);
      PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);

      // Set the offset for all the break jumps that need to go to here
      for (int need : v_need_break)
        SET_CODE_SHORT (need, CODE_SIZE ());


      PUSH_CODE (INSTR::PUSH_NIL); // error object
      PUSH_CODE (INSTR::PUSH_FALSE); // return marker
      PUSH_CODE (INSTR::PUSH_TRUE);// break marker
      PUSH_CODE (INSTR::JMP);
      int also_need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // Set the offset for all the return jumps that need to go to here
      for (int need_cleanup : v_need_cleanup)
        SET_CODE_SHORT (need_cleanup, CODE_SIZE ());

      PUSH_CODE (INSTR::PUSH_NIL); // error object
      PUSH_CODE (INSTR::PUSH_TRUE);// return marker
      PUSH_CODE (INSTR::PUSH_FALSE); // break marker

      // If we were falling throught the body to the cleanup we jump to here
      SET_CODE_SHORT(need_after, CODE_SIZE ());
      SET_CODE_SHORT(also_need_after, CODE_SIZE ());
    }
  else if (n_need_break)
    {
      n_falses = 1;
      // This nil ov is the break marker if we are falling through to the
      // cleanup code from the body. We have an error object on the stack.
      PUSH_CODE (INSTR::PUSH_FALSE); // break marker
      PUSH_CODE (INSTR::JMP);
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // For unwinding we need to keep track of the ov we pushed.
      PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);

      // Set the offset for all the break jumps that need to go to here
      for (int need : v_need_break)
        SET_CODE_SHORT (need, CODE_SIZE ());

      PUSH_CODE (INSTR::PUSH_NIL); // error object
      PUSH_CODE (INSTR::PUSH_TRUE);// break marker

      SET_CODE_SHORT(need_after, CODE_SIZE ());
    }
  else if (n_need_cleanup)
    {
      n_falses = 1;
      // This nil ov is the return marker if we are falling through to the
      // cleanup code from the body
      PUSH_CODE (INSTR::PUSH_FALSE); // return marker
      PUSH_CODE (INSTR::JMP); // We need to skip the pushes for the returns
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // For unwinding we need to keep track of the ov we pushed.
      PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);

      // Set the offset for all the jumps that need to go to here
      for (int need_cleanup : v_need_cleanup)
        SET_CODE_SHORT (need_cleanup, CODE_SIZE ());

      PUSH_CODE (INSTR::PUSH_NIL); // error object
      PUSH_CODE (INSTR::PUSH_TRUE);// return marker

      SET_CODE_SHORT(need_after, CODE_SIZE ());
    }

  // This is the end of protected code
  UNWIND (unwind_idx).m_ip_end = CODE_SIZE ();

  if (n_falses)
    {
      // Fallthrough code do not need false pushes
      PUSH_CODE (INSTR::JMP);
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);
      // An unwind will jump to here and needs some
      // falses pushed to mark return or breaks
      UNWIND (unwind_idx).m_ip_target = CODE_SIZE ();
      for (int i = 0; i < n_falses; i++)
        PUSH_CODE (INSTR::PUSH_FALSE);

      SET_CODE_SHORT(need_after, CODE_SIZE ());
    }
  else
    UNWIND (unwind_idx).m_ip_target = CODE_SIZE ();

  // The body will just fall through into the unwind clean up code

  // There might be breaks in the cleanup code too
  if (break_stack_populated)
    PUSH_BREAKS();

  // Walk the clean up code
  if (cleanup_code)
    cleanup_code->accept (*this);
  if (cleanup_expr)
    cleanup_expr->accept (*this);

  // Used to e.g. always call op CLEAR_IGNORE_OUTPUTS
  for (auto instr : cleanup_instructions)
    PUSH_CODE (instr);

  std::vector<int> v_need_break_cleanup;
  if (break_stack_populated)
    v_need_break_cleanup = POP_BREAKS ();

  if (v_need_break_cleanup.size ())
    TODO ("break in cleanup code");

  if (break_stack_populated)
    {
      // Restore the initial "need breaks"
      PUSH_BREAKS ();
      for (int offset : v_need_breaks_initial)
        PUSH_NEED_BREAK (offset);
    }

  if (n_need_break && n_need_cleanup)
    TODO ("Return and break nested");
  if (n_need_break)
    {
      // The break ov marker is on the stack.
      // If it is not true, we skip the break jump
      PUSH_CODE (INSTR::JMP_IFN);
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      POP_NESTING_STATEMENT (); // The jump ate the break marker

      PUSH_CODE (INSTR::POP); // Pop the error object

      // So, we break jump from here.
      // The visitor for the loop will write to proper target
      PUSH_CODE (INSTR::JMP);
      int need_break = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);
      PUSH_NEED_BREAK (need_break); // Nesting loop need to know

      // If we are not breaking we jump to here
      SET_CODE_SHORT (need_after, CODE_SIZE ());
    }
  // Check if we are doing a return unwind
  else if (n_need_cleanup)
    {
      // If we are in another unwind protect we need to jump to its cleanup
      // code if the return ov marker is true
      if (N_UNWIND_RETURN_TARGETS())
        {
          // The return ov marker is on the stack.
          // If it is not true, we skip the "jump bridge"
          PUSH_CODE (INSTR::JMP_IFN);
          int need_after = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);

          POP_NESTING_STATEMENT (); // The jump ate the return marker

          PUSH_CODE (INSTR::POP); // Pop the error object

          PUSH_CODE (INSTR::JMP); // Jump to the nesting unwind protect
          int need_unwind = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);
          PUSH_A_UNWIND_RETURN_TARGET (need_unwind);

          // If we are not returning we jump to here
          SET_CODE_SHORT (need_after, CODE_SIZE ());
        }
      // Return if the return marker on the stack is true
      else
        {
          // The return ov marker is on the stack.
          // If it is not true, we skip the return
          PUSH_CODE (INSTR::JMP_IFN);
          int need_after = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);

          POP_NESTING_STATEMENT (); // The jump ate the return marker

          // Generate code for a return
          emit_return ();

          // If we are not returning we jump to here
          SET_CODE_SHORT (need_after, CODE_SIZE ());
        }
    }
  // If there is an error object on top of the stack we
  // need to continue unwinding.
  PUSH_CODE (INSTR::THROW_IFERROBJ);

  POP_NESTING_STATEMENT ();
}

void
bytecode_walker::
visit_unwind_protect_command (tree_unwind_protect_command& cmd)
{
  emit_unwind_protect_code (cmd.body (), cmd.cleanup ());
}

void
bytecode_walker::
visit_try_catch_command (tree_try_catch_command& cmd)
{
  // So we are in a try catch.
  //
  int unwind_idx = N_UNWIND ();
  PUSH_UNWIND();

  UNWIND (unwind_idx).m_ip_start = CODE_SIZE ();

  UNWIND (unwind_idx).m_unwind_entry_type =
    unwind_entry_type::TRY_CATCH;

  tree_statement_list *try_code = cmd.body ();

  // Walk the body for the code
  if (try_code)
    try_code->accept (*this);
  // We need to jump past the catch code that will come after
  PUSH_CODE (INSTR::JMP);
  int need_after = CODE_SIZE ();
  PUSH_CODE_SHORT (-1);

  // Mark an end to the "try zone"
  UNWIND (unwind_idx).m_ip_end = CODE_SIZE ();

  // We put the catch code right after the try body
  UNWIND (unwind_idx).m_ip_target = CODE_SIZE ();

  // For loops add two native ints and one ov to the stack,
  // and switches add one ov to the stack, so we need to
  // record how many things we have added to the stack,
  // from for loops and switches.
  UNWIND (unwind_idx).m_stack_depth = n_on_stack_due_to_stmt ();

  // The optional identifier "catch id"
  tree_identifier *expr_id = cmd.identifier ();

  // The unwind code in the vm will push an error object ...
  if (expr_id)
    {
      // ... so assign it to the identifiers in its slot.
      std::string name = expr_id->name ();
      int slot = add_id_to_table (name);
      MAYBE_PUSH_WIDE_OPEXT (slot);
      PUSH_CODE (INSTR::ASSIGN);
      PUSH_SLOT (slot);
    }
  else
    {
      // ... just pop the error object unceremoniously.
      PUSH_CODE (INSTR::POP);
    }

  // Walk the catch code
  tree_statement_list *catch_code = cmd.cleanup ();
  if (catch_code)
    catch_code->accept (*this);

  // The body jumps to here
  SET_CODE_SHORT (need_after, CODE_SIZE ());

  return;
}

// For loops add two native ints and one ov to the stack,
// and switches etc add one ov to the stack, so we need to
// how how many things we have added to the stack
int
bytecode_walker::
n_on_stack_due_to_stmt ()
{
  auto v = NESTING_STATEMENTS();
  int n_things_on_stack = 0;
  for (auto it = v.rbegin () ;it !=  v.rend (); it++)
    {
      nesting_statement t = *it;
      switch (t)
        {
        case nesting_statement::FOR_LOOP:
          n_things_on_stack += 3;
          break;
        case nesting_statement::ONE_OV_ON_STACK:
          n_things_on_stack += 1;
          break;
        default:
          ERR("Invalid state");
        }
    }

  return n_things_on_stack;
}

void
bytecode_walker::
visit_decl_command (tree_decl_command& cmd)
{
  tree_decl_init_list *lst = cmd.initializer_list ();

  CHECK_NONNULL (lst);

  // A decl list might containt multiple declarations.
  // E.g. "global a b = 3 c"
  for (auto it = lst->begin (); it != lst->end (); it++)
    {
      tree_decl_elt *el = *it;
      CHECK_NONNULL (el);

      std::string name = el->name ();

      int slot = add_id_to_table (name);

      if (el->is_global () || el->is_persistent())
        {
          if (el->is_global ())
            PUSH_GLOBAL (name);
          if (el->is_persistent())
            PUSH_PERSISTENT (name);

          // Slot for variable to keep track off if the variable is actually
          // a global. Prepended with "#" to not collide. "+" for persistent.
          //
          // We need this since the same identifier in a function can be both
          // a local or a global depending on whether the global declare
          // statement is reached or not.
          //
          // Since the name of the identifier that is declared global might
          // allready be used as a local, we also need to store the slot number
          // of the #-marker in the code too. If this feature is removed, we
          // can save some space in the OP-codes making the slot number implicit
          // +1.
          std::string prefix;
          if (el->is_global ())
            prefix = "#";
          else
            prefix = "+";

          int prefix_slot = add_id_to_table (prefix + name);

          PUSH_CODE (INSTR::GLOBAL_INIT);
          if (el->is_global ())
            PUSH_CODE (global_type::GLOBAL);
          else if (el->is_persistent ())
            {
              PUSH_CODE (global_type::PERSISTENT);
              // We need a "offset" for the persistent variable that
              // matches the exact offset the treewalker would use
              tree_identifier *id = el->ident ();
              CHECK_NONNULL (id);
              int offset = id->symbol ().data_offset ();

              CHECK (offset < 256); // TODO: Support more slots

              // The VM need to know the special persistent variable offset
              // so we store it in the unwind data
              m_code.m_unwind_data.
                m_slot_to_persistent_slot[slot] = offset;
            }
          PUSH_WSLOT (slot);
          PUSH_WSLOT (prefix_slot);

          tree_expression *expr = el->expression ();
          bool has_init = expr;

          PUSH_CODE (has_init); // has initialization code

          // The global has an initialization expression
          if (has_init)
            {
              // Placeholder for address to after init code.
              // GLOBAL_INIT jumps to there if the global is
              // already initialized.
              int need_after = CODE_SIZE ();
              PUSH_CODE_SHORT (-1);

              // We want the value of the initialization on
              // the operand stack.

              INC_DEPTH();
              PUSH_NARGOUT(1);

              // Walk for the initialization code
              expr->accept (*this);
              // The value of rhs is on the operand stack now.
              // So we need to write it to its local slot and then
              // write that to its global value.
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::ASSIGN); // Write operand stack top ...
              PUSH_SLOT (slot);   // to the local slot of the global

              // I think only this makes sense
              CHECK (DEPTH () == 1);

              POP_NARGOUT ();
              DEC_DEPTH ();

              // Write the instruction address to the placeholder
              SET_CODE_SHORT (need_after, CODE_SIZE ());
            }
        }
      else
        ERR ("Strange state");
    }
}

void
bytecode_walker::
visit_postfix_expression (tree_postfix_expression& expr)
{
  INC_DEPTH();

  tree_expression *e = expr.operand ();
  CHECK_NONNULL (e);

  octave_value::unary_op op = expr.op_type ();

  int folded_need_after = -1;
  int fold_slot = -1;
  // Check if we should to a constant fold. It only makes sense in loops since the expression is folded at runtime.
  // Essentially there is a PUSH_FOLDED_CST opcode that is tied to a cache. If the cache is valid, push it and jump
  // past the initialization code, otherwise run the initialization code and set the cache with SET_FOLDED_CST
  if (m_n_nested_loops && !m_is_folding && is_foldable_walker::is_foldable (expr))
    {
      m_is_folding = true;

      std::string fold_name = "#cst_fold_" + std::to_string (m_n_folds++);
      fold_slot = add_id_to_table (fold_name);

      MAYBE_PUSH_WIDE_OPEXT (fold_slot);
      PUSH_CODE (INSTR::PUSH_FOLDED_CST);
      PUSH_SLOT (fold_slot);
      folded_need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);
    }

  int slot = -1;
  // For ++ and -- we don't want a local pushed to the stack, but operate
  // directly in the slot, and then pushing the slot.
  if (e->is_identifier() && (op == octave_value::unary_op::op_decr ||
                             op == octave_value::unary_op::op_incr))
    {
      // Just add the symbol to the table
      // TODO: Could there be command function calls messing this up?
      //       I.e. foo++ could be a foo()++?
      slot = add_id_to_table (e->name ());
    }
  // We handle e.g. m("qwe")++ with eval
  else if (op != octave_value::unary_op::op_incr && op != octave_value::unary_op::op_decr)
    {
      PUSH_NARGOUT (1);
      e->accept (*this);
      POP_NARGOUT ();
    }

  switch (op)
    {
    case octave_value::unary_op::op_not:
      PUSH_CODE (INSTR::NOT);
      break;
    case octave_value::unary_op::op_uplus:
      PUSH_CODE (INSTR::UADD);
      break;
    case octave_value::unary_op::op_uminus:
      PUSH_CODE (INSTR::USUB);
      break;
    case octave_value::unary_op::op_transpose:
      PUSH_CODE (INSTR::TRANS);
      break;
    case octave_value::unary_op::op_hermitian:
      PUSH_CODE (INSTR::HERM);
      break;
    case octave_value::unary_op::op_incr:
      {
        if (! e->is_identifier ())
          {
            // TODO: Cheating with eval
            PUSH_TREE_FOR_EVAL (&expr);
            int tree_idx = -CODE_SIZE (); // NB: Negative to not collide with debug data

            PUSH_CODE (INSTR::EVAL);
            PUSH_CODE (NARGOUT ());
            PUSH_CODE_INT (tree_idx);
          }
        else
          {
            MAYBE_PUSH_WIDE_OPEXT (slot);
            PUSH_CODE (INSTR::INCR_ID_POSTFIX);
            PUSH_SLOT (slot);
          }
      }
      break;
    case octave_value::unary_op::op_decr:
      {
        if (! e->is_identifier ())
          {
            // TODO: Cheating with eval
            PUSH_TREE_FOR_EVAL (&expr);
            int tree_idx = -CODE_SIZE (); // NB: Negative to not collide with debug data

            PUSH_CODE (INSTR::EVAL);
            PUSH_CODE (NARGOUT ());
            PUSH_CODE_INT (tree_idx);
          }
        else
          {
            MAYBE_PUSH_WIDE_OPEXT (slot);
            PUSH_CODE (INSTR::DECR_ID_POSTFIX);
            PUSH_SLOT (slot);
          }
      }
      break;
    default:
      TODO ("not covered");
    }

  if (fold_slot != -1)
    {
      m_is_folding = false;

      PUSH_CODE (INSTR::DUP);
      MAYBE_PUSH_WIDE_OPEXT (fold_slot);
      PUSH_CODE (INSTR::SET_FOLDED_CST);
      PUSH_SLOT (fold_slot);

      SET_CODE_SHORT (folded_need_after, CODE_SIZE ());
    }

  maybe_emit_bind_ans_and_disp (expr);

  DEC_DEPTH();
}

void
bytecode_walker::
visit_prefix_expression (tree_prefix_expression& expr)
{
  INC_DEPTH();

  tree_expression *e = expr.operand ();
  CHECK_NONNULL (e);

  octave_value::unary_op op = expr.op_type ();

  int folded_need_after = -1;
  int fold_slot = -1;
  // Check if we should to a constant fold. It only makes sense in loops since the expression is folded at runtime.
  // Essentially there is a PUSH_FOLDED_CST opcode that is tied to a cache. If the cache is valid, push it and jump
  // past the initialization code, otherwise run the initialization code and set the cache with SET_FOLDED_CST
  if (m_n_nested_loops && !m_is_folding && is_foldable_walker::is_foldable (expr))
    {
      m_is_folding = true;

      std::string fold_name = "#cst_fold_" + std::to_string (m_n_folds++);
      fold_slot = add_id_to_table (fold_name);

      MAYBE_PUSH_WIDE_OPEXT (fold_slot);
      PUSH_CODE (INSTR::PUSH_FOLDED_CST);
      PUSH_SLOT (fold_slot);
      folded_need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);
    }

  int slot = -1;
  // For ++ and -- we don't want a local pushed to the stack, but operate
  // directly in the slot, and then pushing the slot.
  if (e->is_identifier() && (op == octave_value::unary_op::op_decr ||
                             op == octave_value::unary_op::op_incr))
    {
      // Just add the symbol to the table
      // TODO: Could there be command function calls messing this up?
      //       I.e. foo++ could be a foo()++?
      slot = add_id_to_table (e->name ());
    }
  // We handle e.g. m("qwe")++ with eval
  else if (op != octave_value::unary_op::op_incr && op != octave_value::unary_op::op_decr)
    {
      PUSH_NARGOUT (1);
      e->accept (*this);
      POP_NARGOUT ();
    }

  switch (op)
    {
    case octave_value::unary_op::op_not:
      PUSH_CODE (INSTR::NOT);
      break;
    case octave_value::unary_op::op_uplus:
      PUSH_CODE (INSTR::UADD);
      break;
    case octave_value::unary_op::op_uminus:
      PUSH_CODE (INSTR::USUB);
      break;
    case octave_value::unary_op::op_transpose:
      PUSH_CODE (INSTR::TRANS);
      break;
    case octave_value::unary_op::op_hermitian:
      PUSH_CODE (INSTR::HERM);
      break;
    case octave_value::unary_op::op_incr:
      {
        if (! e->is_identifier ())
          {
            // TODO: Cheating with eval
            PUSH_TREE_FOR_EVAL (&expr);
            int tree_idx = -CODE_SIZE (); // NB: Negative to not collide with debug data

            PUSH_CODE (INSTR::EVAL);
            PUSH_CODE (NARGOUT ());
            PUSH_CODE_INT (tree_idx);
          }
        else
          {
            MAYBE_PUSH_WIDE_OPEXT (slot);
            PUSH_CODE (INSTR::INCR_ID_PREFIX);
            PUSH_SLOT (slot);
          }
      }
      break;
    case octave_value::unary_op::op_decr:
      {
        if (! e->is_identifier ())
          {
            // TODO: Cheating with eval
            PUSH_TREE_FOR_EVAL (&expr);
            int tree_idx = -CODE_SIZE (); // NB: Negative to not collide with debug data

            PUSH_CODE (INSTR::EVAL);
            PUSH_CODE (NARGOUT ());
            PUSH_CODE_INT (tree_idx);
          }
        else
          {
            MAYBE_PUSH_WIDE_OPEXT (slot);
            PUSH_CODE (INSTR::DECR_ID_PREFIX);
            PUSH_SLOT (slot);
          }
      }
      break;
    default:
      TODO ("not covered");
    }

  if (fold_slot != -1)
    {
      m_is_folding = false;

      PUSH_CODE (INSTR::DUP);
      MAYBE_PUSH_WIDE_OPEXT (fold_slot);
      PUSH_CODE (INSTR::SET_FOLDED_CST);
      PUSH_SLOT (fold_slot);

      SET_CODE_SHORT (folded_need_after, CODE_SIZE ());
    }

  maybe_emit_bind_ans_and_disp (expr);

  DEC_DEPTH();
}

void
bytecode_walker::
visit_boolean_expression(tree_boolean_expression& expr)
{
  INC_DEPTH ();
  PUSH_NARGOUT (1);

  // Since the || and && has short circuit behavoir
  // we need to built up the expression from multiple opcodes.
  //
  // Note that UNARY_TRUE accepts operands that are not
  // "is_defined ()" where as IF or IF_N would error on those,
  // so we need UNARY_TRUE before the IFs.
  if (expr.op_type() == tree_boolean_expression::bool_and)
    {
      // We want lhs on the operand stack
      tree_expression *op1 = expr.lhs ();
      CHECK_NONNULL (op1);
      op1->accept (*this);

      // If false, jump to push false
      PUSH_CODE (INSTR::UNARY_TRUE);
      PUSH_CODE (INSTR::JMP_IFN);
      int need_false0 = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // If lhs was true, we want rhs on the
      // operand stack too.
      tree_expression *op2 = expr.rhs ();
      CHECK_NONNULL (op2);
      op2->accept (*this);

      // If false, jump to push false
      PUSH_CODE (INSTR::UNARY_TRUE);
      PUSH_CODE (INSTR::JMP_IFN);
      int need_false1 = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // If both lhs and rhs was true,
      // we fallthrough to push true
      PUSH_CODE (INSTR::PUSH_TRUE);
      PUSH_CODE (INSTR::JMP); // Jump past PUSH_FALSE
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // The 2 JMP_IFN goes here, to PUSH_FALSE
      int offset_false = CODE_SIZE ();
      PUSH_CODE (INSTR::PUSH_FALSE);
      // The JMP after PUSH_TRUE goes here

      // Set the addresses for the false jumps
      SET_CODE_SHORT (need_false0, offset_false);
      SET_CODE_SHORT (need_false1, offset_false);
      // The true push jumps to after
      SET_CODE_SHORT (need_after, CODE_SIZE ());
    }
  else
    {
      // We want lhs on the operand stack
      tree_expression *op1 = expr.lhs ();
      CHECK_NONNULL (op1);
      op1->accept (*this);

      // If true, jump to push true
      PUSH_CODE (INSTR::UNARY_TRUE);
      PUSH_CODE (INSTR::JMP_IF);
      int need_true0 = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // If lhs was false, we want rhs on the
      // operand stack too.
      tree_expression *op2 = expr.rhs ();
      CHECK_NONNULL (op2);
      op2->accept (*this);

      // If true, jump to push true
      PUSH_CODE (INSTR::UNARY_TRUE);
      PUSH_CODE (INSTR::JMP_IF);
      int need_true1 = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // If both lhs and rhs were false,
      // we fallthrough to here, push false
      PUSH_CODE (INSTR::PUSH_FALSE);
      PUSH_CODE (INSTR::JMP); // Jump past PUSH_TRUE
      int need_after = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);

      // The 2 JMP_IF goes here, to PUSH_TRUE
      int offset_true = CODE_SIZE ();
      PUSH_CODE (INSTR::PUSH_TRUE);
      // The JMP after PUSH_FALSE goes here

      // Set the addresses for the true jumps
      SET_CODE_SHORT (need_true0, offset_true);
      SET_CODE_SHORT (need_true1, offset_true);
      // The false push jumps to after
      SET_CODE_SHORT (need_after, CODE_SIZE ());
    }

  maybe_emit_bind_ans_and_disp (expr);

  DEC_DEPTH ();
  POP_NARGOUT ();
}

void
bytecode_walker::
visit_compound_binary_expression (tree_compound_binary_expression &expr)
{
  // Compound expression are expression that are more effeicient to
  // do fused for a matrix, like M'*A etc.
  INC_DEPTH();
  PUSH_NARGOUT (1);

  tree_expression *op1 = expr.clhs ();

  CHECK_NONNULL (op1);
  op1->accept (*this);

  tree_expression *op2 = expr.crhs ();

  CHECK_NONNULL (op2);
  op2->accept (*this);

  switch (expr.cop_type ())
    {
    case octave_value::compound_binary_op::op_trans_mul:
      PUSH_CODE (INSTR::TRANS_MUL);
      break;
    case octave_value::compound_binary_op::op_mul_trans:
      PUSH_CODE (INSTR::MUL_TRANS);
      break;
    case octave_value::compound_binary_op::op_herm_mul:
      PUSH_CODE (INSTR::HERM_MUL);
      break;
    case octave_value::compound_binary_op::op_mul_herm:
      PUSH_CODE (INSTR::MUL_HERM);
      break;
    case octave_value::compound_binary_op::op_trans_ldiv:
      PUSH_CODE (INSTR::TRANS_LDIV);
      break;
    case octave_value::compound_binary_op::op_herm_ldiv:
      PUSH_CODE (INSTR::HERM_LDIV);
      break;
    default:
      TODO ("not covered");
    }

  maybe_emit_bind_ans_and_disp (expr);

  POP_NARGOUT ();
  DEC_DEPTH();
}

void
bytecode_walker::
visit_binary_expression (tree_binary_expression& expr)
{
  INC_DEPTH ();
  PUSH_NARGOUT (1);

  std::vector<int> need_after;
  int fold_slot = -1;

  // "&" and "|" have a braindead short circuit behavoiur when
  // in if or while conditions, so we need special handling of those.
  if (expr.is_braindead ())
    {
      if (expr.op_type() == octave_value::binary_op::op_el_and)
        {
          // We use a slot to store whether a warning has been issued
          // or not
          std::string id_warning = "%braindead_warning_" +
            std::to_string(CODE_SIZE ());
          int slot = add_id_to_table(id_warning);

          // The left most expression is always evaled
          tree_expression *op1 = expr.lhs ();

          CHECK_NONNULL (op1);
          op1->accept (*this);

          // We need to check if lhs value is scalar
          PUSH_CODE (INSTR::DUP);
          PUSH_CODE (INSTR::BRAINDEAD_PRECONDITION);

          // If the precondition is not true, we do a
          // normal binop. Note that lhs is evaled twice
          // since that is what the treewalker does.
          PUSH_CODE (INSTR::JMP_IFN);
          int need_target_not_braindead = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);

          // Now we do the braindead short circuit

          // If the lhs expression is true we check the rhs
          PUSH_CODE (INSTR::UNARY_TRUE);
          PUSH_CODE (INSTR::JMP_IF);
          int need_target_true = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);

          // The lhs was false which means we need to issue a warning
          // and push a false
          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::BRAINDEAD_WARNING);
          PUSH_SLOT (slot);
          PUSH_CODE ('&'); // The operand type to print in the warning
          PUSH_CODE (INSTR::PUSH_FALSE);
          PUSH_CODE (INSTR::JMP);
          need_after.push_back (CODE_SIZE ());
          PUSH_CODE_SHORT (-1);

          // If lhs was true we jump to here
          SET_CODE_SHORT (need_target_true, CODE_SIZE ());
          // Walk rhs
          tree_expression *op2 = expr.rhs ();

          CHECK_NONNULL (op2);
          op2->accept (*this);

          // With rhs on the stack, check if it is true and jump to
          // a true push, otherwise push false and jump to after
          PUSH_CODE (INSTR::UNARY_TRUE);
          PUSH_CODE (INSTR::JMP_IF);
          need_target_true = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);

          // Push false jump to after
          PUSH_CODE (INSTR::PUSH_FALSE);
          PUSH_CODE (INSTR::JMP);
          need_after.push_back (CODE_SIZE ());
          PUSH_CODE_SHORT (-1);

          // Push true jump to after
          SET_CODE_SHORT (need_target_true, CODE_SIZE ());
          PUSH_CODE (INSTR::PUSH_TRUE);
          PUSH_CODE (INSTR::JMP);
          need_after.push_back (CODE_SIZE ());
          PUSH_CODE_SHORT (-1);

          // If the precondition was false we need to do the ordinary binary op
          SET_CODE_SHORT (need_target_not_braindead, CODE_SIZE ());
          PUSH_CODE (INSTR::POP); // Pop the evaled lhs value
        }
      else if (expr.op_type() == octave_value::binary_op::op_el_or)
        {
          // We use a slot to store whether a warning has been issued
          // or not
          std::string id_warning = "%braindead_warning_" +
            std::to_string(CODE_SIZE ());
          int slot = add_id_to_table(id_warning);

          // The left most expression is always evaled
          tree_expression *op1 = expr.lhs ();

          CHECK_NONNULL (op1);
          op1->accept (*this);

          // We need to check if lhs value is scalar
          PUSH_CODE (INSTR::DUP);
          PUSH_CODE (INSTR::BRAINDEAD_PRECONDITION);

          // If the precondition is not true, we do a
          // normal binop. Note that lhs is evaled twice
          // since that is what the treewalker does.
          PUSH_CODE (INSTR::JMP_IFN);
          int need_target_not_braindead = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);

          // Now we do the braindead short circuit for "or"

          // If the lhs expression is true we issue a
          // warning, push a true and jump to after.
          // If lhs is false we instead need to check rhs too.
          PUSH_CODE (INSTR::UNARY_TRUE);
          PUSH_CODE (INSTR::JMP_IFN);
          int need_target_check_rhs = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);

          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::BRAINDEAD_WARNING);
          PUSH_SLOT (slot);
          PUSH_CODE ('|'); // The operand type to print in the warning
          PUSH_CODE (INSTR::PUSH_TRUE);
          PUSH_CODE (INSTR::JMP);
          need_after.push_back (CODE_SIZE ());
          PUSH_CODE_SHORT (-1);

          // If lhs was false we jump to here
          SET_CODE_SHORT (need_target_check_rhs, CODE_SIZE ());
          // Walk rhs
          tree_expression *op2 = expr.rhs ();

          CHECK_NONNULL (op2);
          op2->accept (*this);

          // With rhs on the stack, check if it is true and jump to
          // a true push, otherwise push false and jump to after
          PUSH_CODE (INSTR::UNARY_TRUE);
          PUSH_CODE (INSTR::JMP_IF);
          int need_target_true = CODE_SIZE ();
          PUSH_CODE_SHORT (-1);

          // Push false jump to after
          PUSH_CODE (INSTR::PUSH_FALSE);
          PUSH_CODE (INSTR::JMP);
          need_after.push_back (CODE_SIZE ());
          PUSH_CODE_SHORT (-1);

          // Push true jump to after
          SET_CODE_SHORT (need_target_true, CODE_SIZE ());
          PUSH_CODE (INSTR::PUSH_TRUE);
          PUSH_CODE (INSTR::JMP);
          need_after.push_back (CODE_SIZE ());
          PUSH_CODE_SHORT (-1);

          // If the precondition was false we need to do the ordinary binary op
          SET_CODE_SHORT (need_target_not_braindead, CODE_SIZE ());
          PUSH_CODE (INSTR::POP); // Pop the evaled lhs value
        }
      else
        panic_impossible ();
    }
  // Check if we should to a constant fold. It only makes sense in loops since the expression is folded at runtime.
  // Essentially there is a PUSH_FOLDED_CST opcode that is tied to a cache. If the cache is valid, push it and jump
  // past the initialization code, otherwise run the initialization code and set the cache with SET_FOLDED_CST
  else if (m_n_nested_loops && !m_is_folding && is_foldable_walker::is_foldable (expr))
    {
      m_is_folding = true;

      std::string fold_name = "#cst_fold_" + std::to_string (m_n_folds++);
      fold_slot = add_id_to_table (fold_name);

      MAYBE_PUSH_WIDE_OPEXT (fold_slot);
      PUSH_CODE (INSTR::PUSH_FOLDED_CST);
      PUSH_SLOT (fold_slot);
      need_after.push_back (CODE_SIZE ());
      PUSH_CODE_SHORT (-1);
    }

  tree_expression *op1 = expr.lhs ();
  tree_expression *op2 = expr.rhs ();
  CHECK_NONNULL (op1);
  CHECK_NONNULL (op2);

  if (op1->is_constant () && op2->is_constant () && DATA_SIZE () < 255)
    {
      // If both rhs and lhs are constants we want to emit a super op-code
      // aslong as the WIDE op is not going to be used (<255)
      emit_load_2_cst (op1, op2);
    }
  else
    {
      op1->accept (*this);
      op2->accept (*this);
    }

  switch (expr.op_type ())
    {
    case octave_value::binary_op::op_mul:
      PUSH_CODE (INSTR::MUL);
      break;
    case octave_value::binary_op::op_div:
      PUSH_CODE (INSTR::DIV);
      break;
    case octave_value::binary_op::op_add:
      PUSH_CODE (INSTR::ADD);
      break;
    case octave_value::binary_op::op_sub:
      PUSH_CODE (INSTR::SUB);
      break;
    case octave_value::binary_op::op_lt:
      PUSH_CODE (INSTR::LE);
      break;
    case octave_value::binary_op::op_le:
      PUSH_CODE (INSTR::LE_EQ);
      break;
    case octave_value::binary_op::op_gt:
      PUSH_CODE (INSTR::GR);
      break;
    case octave_value::binary_op::op_ge:
      PUSH_CODE (INSTR::GR_EQ);
      break;
    case octave_value::binary_op::op_eq:
      PUSH_CODE (INSTR::EQ);
      break;
    case octave_value::binary_op::op_ne:
      PUSH_CODE (INSTR::NEQ);
      break;
    case octave_value::binary_op::op_pow:
      PUSH_CODE (INSTR::POW);
      break;
    case octave_value::binary_op::op_ldiv:
      PUSH_CODE (INSTR::LDIV);
      break;
    case octave_value::binary_op::op_el_mul:
      PUSH_CODE (INSTR::EL_MUL);
      break;
    case octave_value::binary_op::op_el_div:
      PUSH_CODE (INSTR::EL_DIV);
      break;
    case octave_value::binary_op::op_el_pow:
      PUSH_CODE (INSTR::EL_POW);
      break;
    case octave_value::binary_op::op_el_and:
      PUSH_CODE (INSTR::EL_AND);
      break;
    case octave_value::binary_op::op_el_or:
      PUSH_CODE (INSTR::EL_OR);
      break;
    case octave_value::binary_op::op_el_ldiv:
      PUSH_CODE (INSTR::EL_LDIV);
      break;

    default:
      TODO ("not covered");
    }

  if (fold_slot != -1)
    {
      m_is_folding = false;

      PUSH_CODE (INSTR::DUP);
      MAYBE_PUSH_WIDE_OPEXT (fold_slot);
      PUSH_CODE (INSTR::SET_FOLDED_CST);
      PUSH_SLOT (fold_slot);
    }

  for (int offset : need_after)
    SET_CODE_SHORT (offset, CODE_SIZE ());

  maybe_emit_bind_ans_and_disp (expr);

  POP_NARGOUT ();

  DEC_DEPTH ();
}

void

bytecode_walker::
emit_load_2_cst (tree_expression *lhs, tree_expression *rhs)
{
  INC_DEPTH();

  CHECK (DEPTH () > 1);

  CHECK (lhs); CHECK (rhs);
  CHECK (lhs->is_constant ());
  CHECK (rhs->is_constant ());

  tree_constant *lhs_cst = static_cast<tree_constant *> (lhs);
  tree_constant *rhs_cst = static_cast<tree_constant *> (rhs);

  octave_value ov_lhs = lhs_cst->value ();
  octave_value ov_rhs = rhs_cst->value ();

  PUSH_DATA (ov_lhs);
  PUSH_DATA (ov_rhs);

  unsigned cst_offset = DATA_SIZE () - 1;
  CHECK (cst_offset < 256);

  PUSH_CODE (INSTR::LOAD_2_CST);
  PUSH_CODE (cst_offset - 1); // Offset of lhs

  DEC_DEPTH();
}

void
bytecode_walker::
visit_constant (tree_constant& cst)
{
  INC_DEPTH();

  octave_value ov_cst = cst.value ();

  bool specialized = false;
  if (ov_cst.type_id () == octave_scalar::static_type_id ())
    {
      double val = ov_cst.double_value ();
      if (val == 0)
      {
          specialized = true;
          PUSH_CODE (INSTR::PUSH_DBL_0);
      }
      else if (val == 1)
      {
          specialized = true;
          PUSH_CODE (INSTR::PUSH_DBL_1);
      }
      else if (val == 2)
      {
          specialized = true;
          PUSH_CODE (INSTR::PUSH_DBL_2);
      }
    }

  if (!specialized)
    {
      PUSH_DATA (ov_cst);
      PUSH_CODE_LOAD_CST (DATA_SIZE () - 1); // Offset of the constant
    }

  maybe_emit_bind_ans_and_disp (cst);

  DEC_DEPTH();
}

void
bytecode_walker::
visit_octave_user_function (octave_user_function& fcn)
{
  m_code.m_unwind_data.m_name = fcn.name ();
  m_code.m_unwind_data.m_file = fcn.fcn_file_name ();
  PUSH_DATA (fcn.name ());
  PUSH_DATA (std::string {"user-function"});
  PUSH_DATA (fcn.profiler_name ());

  tree_statement_list *cmd_list = fcn.body ();
  tree_parameter_list *returns = fcn.return_list();
  tree_parameter_list *paras = fcn.parameter_list ();

  std::vector<std::string> v_paras;
  if (paras) // paras is 0 if function args are missing, e.g. "function foo\nend"
    {
      for (auto it = paras->begin (); it != paras->end (); it++)
        {
          CHECK_NONNULL (*it);
          CHECK ((*it)->ident ());
          v_paras.push_back ((*it)->name ());
        }
    }

  // TODO: The return_list is a nullptr for anonymous functions.
  if (!returns)
    error ("Compiling anonymous functions is not supported by the VM yet");

  // Does the function output varargout?
  m_varargout = returns->takes_varargs ();
  // "varargout" is not in the 'returns' list (if in the proper last position)
  // so add one to size if 'm_varargout' is true
  int n_returns = returns ? returns->size () + m_varargout: 0;

  // The first instruction is the amount of return variables. Negative for varargout.
  // +1 for native '%nargout' on the stack
  PUSH_CODE (m_varargout ? -(n_returns + 1) : (n_returns + 1));

  // Check if the last parameter is "varargin"
  // If that is the case, we need to mess with the stacks
  // in the vm, so mark the function as having negative
  // amount of parameters.
  bool is_varargin = paras ? paras->takes_varargs () : false;

  // varargin is not among the parameter_lists elements, so
  // add it to the vector of parameter names
  if (is_varargin)
    v_paras.push_back("varargin");

  // The second instruction is the amount of arguments
  int n_paras = v_paras.size ();
  PUSH_CODE (is_varargin ? -n_paras : n_paras);

  // The third is the amount of locals, which need to be set
  // after compiling the function. So we need to store the offset
  // to it for later
  m_offset_n_locals = CODE_SIZE ();
  PUSH_CODE (-1); // Placeholder
  PUSH_CODE (-1);

  // The first slot is a native int represenation nargout
  // so we add a dummy slot object for it
  add_id_to_table("%nargout");

  // Then the return values
  for (auto it = returns->begin (); it != returns->end (); it++)
    {
      std::string name = (*it)->name();
      tree_identifier *id = (*it)->ident ();
      CHECK_NONNULL (id);
      add_id_to_table (id->name ());
    }
  if (m_varargout)
    add_id_to_table ("varargout"); // Not in the returns list. Need to be last

  // The function itself is put after the arg outs
  /* add_id_to_table (fcn.name ()); */

  // Then the arguments
  for (std::string name : v_paras)
    {
      if (m_map_locals_to_slot.find (name) !=
          m_map_locals_to_slot.end ())
        {
          // So the parameter is also a return value
          // so we need to push it and assign
          // it to the return value, since the caller
          // will write the argument to the argument slot.
          //
          // We give the parameter a dummy name so it
          // still occupies a slot, and assigns a dummy
          // object to it after we copied it to the return
          // slot.

          std::string dummy_name = "!" + name;
          int slot_dummy = add_id_to_table (dummy_name);

          // PUSH_SLOT_INDEXED just pushes and does not check
          // for doing a cmd function call.
          MAYBE_PUSH_WIDE_OPEXT (slot_dummy);
          PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
          PUSH_SLOT (slot_dummy);
          int slot = SLOT (name);
          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::FORCE_ASSIGN); // Accepts undefined rhs
          PUSH_SLOT (slot);
          PUSH_CODE (INSTR::PUSH_FALSE); // False will do
          MAYBE_PUSH_WIDE_OPEXT (slot_dummy);
          PUSH_CODE (INSTR::ASSIGN);
          PUSH_SLOT (slot_dummy);

          continue;
        }

      add_id_to_table (name);
    }

  // We always need the magic id "ans"
  add_id_to_table ("ans");

  // We add all identifiers in the body to the id-table. We also
  // make a map mapping the interpreters frame offset of a id
  // to the frame offset in the bytecode VM frame.
  if (cmd_list)
    {
      auto v_names_offsets = collect_idnames_walker::collect_id_names (*cmd_list);

      for (auto name_offset : v_names_offsets)
        {
          std::string name = name_offset.first;
          int frame_offset = name_offset.second;
          add_id_to_table (name_offset.first);
          int slot = SLOT (name);

          m_code.m_unwind_data.m_external_frame_offset_to_internal[frame_offset] = slot;
        }
    }
  // We need the arguments and return id:s in the map too.
  if (paras)
    {
      for (auto it = paras->begin (); it != paras->end (); it++)
        {
          CHECK_NONNULL (*it);
          tree_identifier *id = (*it)->ident ();
          int frame_offset = id->symbol ().data_offset ();
          int slot = SLOT (id->name ());
          m_code.m_unwind_data.m_external_frame_offset_to_internal[frame_offset] = slot;

          // If the parameter has an init expression e.g.
          // "function foo (a = sin (pi))"
          // , we need to search it for id:s too.
          tree_expression *init_expr = (*it)->expression ();
          if (init_expr)
            {
              auto v_names_offsets = collect_idnames_walker::collect_id_names (*init_expr);
              for (auto name_offset : v_names_offsets)
                {
                  std::string name = name_offset.first;
                  int frame_offset_i = name_offset.second;
                  add_id_to_table (name_offset.first);
                  int slot_i = SLOT (name);

                  m_code.m_unwind_data.m_external_frame_offset_to_internal[frame_offset_i] = slot_i;
                }
            }
        }
    }
  for (auto it = returns->begin (); it != returns->end (); it++)
    {
      std::string name = (*it)->name();
      tree_identifier *id = (*it)->ident ();
      int frame_offset = id->symbol ().data_offset ();
      int slot = SLOT (name);
      m_code.m_unwind_data.m_external_frame_offset_to_internal[frame_offset] = slot;
    }

  // The function name should be in the frame as an id too aswell
  // as 'varargin', 'varargout' and 'ans'.
  //
  // 'ans' is allready added to the id table and 'varargin' and 'varargout' too
  // if they are used, but we don't have their external offset.
  //
  // The function name is not added to the id table yet.
  //
  // Note that there might be symbols added to the original scope by
  // eg. eval ("foo = 3"). We just ignore those.
  std::string function_name = fcn.name ();
  auto dot_idx = function_name.find_last_of ('.'); // Names might be e.g. "get.Count" but we only want "Count"
  if (dot_idx != std::string::npos)
    function_name = function_name.substr (dot_idx + 1);

  // We need to keep track of which id is the function name so that
  // we can add the id to the id-table and get it's external offset.
  //
  // Note that the file 'bar.m' can have one function with the id name 'foo'
  // which will be added to the scope by the parser, but the function name
  // and thus call-name is 'bar'.
  std::size_t idx_fn_name = n_returns + 1; // "+1" since 'ans' is always added first

  for (auto p : fcn.scope ().symbols ())
    {
      std::string name = p.first;
      symbol_record sym = p.second;
      std::size_t offset = sym.data_offset ();

      bool is_fn_id = offset == idx_fn_name; // Are we at the function name id?

      auto it = m_map_locals_to_slot.find (name);
      if (it == m_map_locals_to_slot.end ())
        {
          if (is_fn_id)
            {
              // Add the function name id to the table and add the correct external offset.
              // (The name might not be the call-name of the function.)
              int slot = add_id_to_table (name);
              m_code.m_unwind_data.m_external_frame_offset_to_internal[offset] = slot;
            }
          else
            continue;
        }

      if (name == "varargin")
        m_code.m_unwind_data.m_external_frame_offset_to_internal[offset] = SLOT ("varargin");
      else if (name == "varargout")
        m_code.m_unwind_data.m_external_frame_offset_to_internal[offset] = SLOT ("varargout");
      else if (name == "ans")
        m_code.m_unwind_data.m_external_frame_offset_to_internal[offset] = SLOT ("ans");
    }

  // Add code to handle default arguments. If an argument is undefined or
  // "magic colon" it is to get its default value.
  if (paras)
    {
      for (auto it = paras->begin (); it != paras->end (); it++)
        {
          tree_expression *init_expr = (*it)->expression ();
          // TODO: Default init for varargin?

          if (init_expr)
            {
              // There is a default arg.

              std::string name = (*it)->name ();
              int slot = SLOT (name);

              // Push the arg to the operand stack from its slot
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
              PUSH_SLOT (slot);
              // If it is undefined or "Magic colon", execute the init code
              // otherwise jump past it.
              PUSH_CODE (INSTR::JMP_IFDEF);
              int need_after = CODE_SIZE ();
              PUSH_CODE_SHORT (-1); // Placeholder

              INC_DEPTH();
              PUSH_NARGOUT(1); // nargout is 1 for simple assignments

              // Walk for the rhs code
              init_expr->accept (*this);

              // The value of rhs is now on the operand stack. Assign it
              // to the arg.
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::ASSIGN);
              PUSH_SLOT (slot);

              POP_NARGOUT ();
              DEC_DEPTH();

              // The jump need to go here, if the argument is defined, so
              // set the placeholder from above.
              SET_CODE_SHORT (need_after, CODE_SIZE ());
            }
        }
    }

  CHECK_NONNULL (cmd_list);
  cmd_list->accept (*this);

  // Set the amount of locals that has a placeholder since earlier
  SET_CODE_SHORT (m_offset_n_locals, m_n_locals);

  // When the last byte of opcode, a 'RET', is to be executed, the VM reads the
  // next byte of code and puts it in 'arg0'.  So, we need to add a dummy
  // opcode afterwards to prevent out-of-bounds reads.
  PUSH_CODE (INSTR::RET);

  // We want to add the locals to the scope in slot order
  // so we push all the locals' names to a vector by their slot
  // number
  unsigned n_slots = m_map_locals_to_slot.size ();
  CHECK (n_slots == static_cast<unsigned> (m_n_locals));
  std::vector<std::string> names (n_slots);

  auto iter = m_map_locals_to_slot.begin ();
  for (unsigned i = 0; i < n_slots; i++)
    {
      auto kv = *iter++;

      const std::string& name = kv.first;
      int slot = kv.second;

      CHECK (slot >= 0 && slot < static_cast<int> (n_slots));
      CHECK (names[slot] == ""); // Check not duplicate slot number used

      names[slot] = name;
    }

  // Check that the mapping between external offsets and internal slots has no holes in it
  int i = 0;
  for (auto it : m_code.m_unwind_data.m_external_frame_offset_to_internal)
    {
      int external_offset = it.first;
      CHECK (external_offset == i);
      i++;
    }

  // The profiler needs to know these sizes when copying from pointers.
  m_code.m_unwind_data.m_code_size = m_code.m_code.size ();
  m_code.m_unwind_data.m_ids_size = m_code.m_ids.size ();
}

void
bytecode_walker::
visit_multi_assignment (tree_multi_assignment& expr)
{
  INC_DEPTH();
  int outer_nargout = NARGOUT ();

  tree_argument_list *lhs = expr.left_hand_side ();

  // Lists are annoying, move lhs elements to a vector
  std::vector<tree_expression *> v_lhs;
  for (auto it = lhs->begin (); it != lhs->end (); it++)
    {
      CHECK_NONNULL(*it);
      v_lhs.push_back (*it);
    }

  CHECK_NONNULL (lhs);
  // Set nargout
  size_t n_args = v_lhs.size ();
  PUSH_NARGOUT (n_args);

  std::vector<std::string> v_arg_names;
  std::vector<bool> v_is_blackhole;

  // Can't nest ignored outputs as the code here is written. Is it even possible to nest those?
  CHECK (m_pending_ignore_outputs == false);

  // TODO:
  //       Something smarter is needed to split of cs-lists among different lhs values
  //       This does not work for e.g. [C{1:2}, D] = {1,2,3}{:}
  //       Probably need some opcode ASSIGNNX or something.
  //       See tree_multi_assignment::evaluate_n and octave_lvalue::eval_for_numel
  //
  //       There probably has to be another tree_walker figuring out how many elements
  //       a lvalue will "ask for". In [C{1:2}] = deal (1,2) deal will have to have nargout 2
  //       which is annoying since we will have to be able to set nargout dynamically.
  //       With a slot maybe?
  //
  //       Maybe make a octave_lvalue and call numel() for simplicity?
  //
  //       Meanwhile, just call eval on it ...

  bool any_lhs_not_id = false;
  for (tree_expression *e : v_lhs)
    if (! e->is_identifier ())
      any_lhs_not_id = true;

  if (any_lhs_not_id)
    {
      // The VM need to access the tree expr.
      // Abuse the dbg info.
      PUSH_TREE_FOR_EVAL (&expr);
      int tree_idx = -CODE_SIZE (); // NB: Negative to not collide with debug data

      PUSH_CODE (INSTR::EVAL);
      PUSH_CODE (outer_nargout);
      PUSH_CODE_INT (tree_idx);

      if (DEPTH () == 1)
        PUSH_CODE (INSTR::POP);

      POP_NARGOUT ();
      DEC_DEPTH ();
      return;
    }

  int n_blackholes = 0;
  int i = 0;
  for (tree_expression *e : v_lhs)
    {
      if (!e->is_identifier ())
        {
          v_arg_names.push_back ("");
          v_is_blackhole.push_back (false);
          continue;
        }

      std::string name = e->name ();
      if (name == "~") // We need to handle the special "ignore id" '~' as in [~, a] = foo ()
        {
          m_v_ignored.push_back (i + 1); // Output parameters are one-indexed
          name = "%~" + std::to_string (n_blackholes++); // We rename it to "%~X"
          v_is_blackhole.push_back (true);
        }
      else
        v_is_blackhole.push_back (false);

      v_arg_names.push_back (name);

      add_id_to_table (name);

      i++;
    }

  CHECK (v_arg_names.size () == n_args);

  /* Handle ignored outputs. Since the called function can ask with isargout()
   * whether an output is ignored or not we need to set a state for this. */
  if (m_v_ignored.size ())
    {
      m_pending_ignore_outputs = 1;
      m_ignored_of_total = n_args;
    }

  tree_expression *rhs = expr.right_hand_side ();
  CHECK_NONNULL(rhs);

  // We want push NARGOUT elements to the operand stack

  emit_unwind_protect_data D;
  if (m_pending_ignore_outputs)
    D = emit_unwind_protect_code_start ();

  rhs->accept (*this); // Walks rhs for NARGOUT elements

  if (m_pending_ignore_outputs)
    {
      // The outer expression in rhs should have set m_ignored_ip_start to its ip offset.
      CHECK (m_ignored_ip_start);
      UNWIND (D.m_idx_unwind).m_ip_start = m_ignored_ip_start;
    }

  if (DEPTH () != 1)
    TODO ("Only root multi assignment supported now");

  PUSH_CODE (INSTR::ASSIGNN);
  // Push the amount of slots
  PUSH_CODE (v_lhs.size ());

  // Push the slots
  for (std::string &name : v_arg_names)
    PUSH_WSLOT (SLOT (name));

  // Emit code to disp if no ;
  for (std::string &name : v_arg_names)
    maybe_emit_push_and_disp_id (expr, name);

  if (m_pending_ignore_outputs)
    {
      emit_unwind_protect_code_before_cleanup (D);

      // As we are ignoring outputs we need to unwind protect to clear the VM state with opcode CLEAR_IGNORE_OUTPUTS
      // We need to supply each black hole slot

      PUSH_CODE (INSTR::CLEAR_IGNORE_OUTPUTS);
      PUSH_CODE (n_blackholes);

      for (unsigned j = 0; j < n_args; j++)
        {
          if (v_is_blackhole.at (j))
            PUSH_WSLOT (SLOT (v_arg_names.at (j)));
        }

      emit_unwind_protect_code_end (D);
    }

  if (m_pending_ignore_outputs)
    {
      m_pending_ignore_outputs = 0;
      m_v_ignored.clear ();
      m_ignored_ip_start = 0;
    }

  POP_NARGOUT ();
  DEC_DEPTH ();
}

std::map<std::string, octave_base_value::unary_mapper_t> bytecode_walker::m_name_to_unary_func =
{
{"abs",       octave_base_value::umap_abs},
{"acos",      octave_base_value::umap_acos},
{"acosh",     octave_base_value::umap_acosh},
{"angle",     octave_base_value::umap_angle},
{"arg",       octave_base_value::umap_arg},
{"asin",      octave_base_value::umap_asin},
{"asinh",     octave_base_value::umap_asinh},
{"atan",      octave_base_value::umap_atan},
{"atanh",     octave_base_value::umap_atanh},
{"cbrt",      octave_base_value::umap_cbrt},
{"ceil",      octave_base_value::umap_ceil},
{"conj",      octave_base_value::umap_conj},
{"cos",       octave_base_value::umap_cos},
{"cosh",      octave_base_value::umap_cosh},
{"erf",       octave_base_value::umap_erf},
{"erfinv",    octave_base_value::umap_erfinv},
{"erfcinv",   octave_base_value::umap_erfcinv},
{"erfc",      octave_base_value::umap_erfc},
{"erfcx",     octave_base_value::umap_erfcx},
{"erfi",      octave_base_value::umap_erfi},
{"dawson",    octave_base_value::umap_dawson},
{"exp",       octave_base_value::umap_exp},
{"expm1",     octave_base_value::umap_expm1},
{"isfinite",  octave_base_value::umap_isfinite},
{"fix",       octave_base_value::umap_fix},
{"floor",     octave_base_value::umap_floor},
{"gamma",     octave_base_value::umap_gamma},
{"imag",      octave_base_value::umap_imag},
{"isinf",     octave_base_value::umap_isinf},
{"isna",      octave_base_value::umap_isna},
{"isnan",     octave_base_value::umap_isnan},
{"lgamma",    octave_base_value::umap_lgamma},
{"log",       octave_base_value::umap_log},
{"log2",      octave_base_value::umap_log2},
{"log10",     octave_base_value::umap_log10},
{"log1p",     octave_base_value::umap_log1p},
{"real",      octave_base_value::umap_real},
{"round",     octave_base_value::umap_round},
{"roundb",    octave_base_value::umap_roundb},
{"signum",    octave_base_value::umap_signum},
{"sin",       octave_base_value::umap_sin},
{"sinh",      octave_base_value::umap_sinh},
{"sqrt",      octave_base_value::umap_sqrt},
{"tan",       octave_base_value::umap_tan},
{"tanh",      octave_base_value::umap_tanh},
{"isalnum",   octave_base_value::umap_xisalnum},
{"isalpha",   octave_base_value::umap_xisalpha},
{"isascii",   octave_base_value::umap_xisascii},
{"iscntrl",   octave_base_value::umap_xiscntrl},
{"isdigit",   octave_base_value::umap_xisdigit},
{"isgraph",   octave_base_value::umap_xisgraph},
{"islower",   octave_base_value::umap_xislower},
{"isprint",   octave_base_value::umap_xisprint},
{"ispunct",   octave_base_value::umap_xispunct},
{"isspace",   octave_base_value::umap_xisspace},
{"isupper",   octave_base_value::umap_xisupper},
{"isxdigit",  octave_base_value::umap_xisxdigit},
{"signbit",   octave_base_value::umap_xsignbit},
{"tolower",   octave_base_value::umap_xtolower},
{"toupper",   octave_base_value::umap_xtoupper},
};

void
bytecode_walker::
emit_disp_obj (tree_expression &expr)
{
  CHECK (expr.print_result ());
  CHECK (DEPTH () == 1);
  PUSH_CODE (INSTR::DISP);
  // Magic slot number 0 (%nargout that is a native int) that
  // will never be printed corrensponds to "" name tag stashing of
  // the ovl before calling display.
  PUSH_SLOT (0);
  PUSH_WSLOT (0); // never a command function call
}

void
bytecode_walker::
maybe_emit_push_and_disp_id (tree_expression &expr, const std::string &name, const std::string maybe_cmd_name)
{
  if (!expr.print_result ())
    return;

  if (name.size () && name[0] == '%') // Don't print internal variables like black holes
    return;

  CHECK (DEPTH () == 1);
  int slot = SLOT (name);
  MAYBE_PUSH_WIDE_OPEXT (slot);
  PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
  PUSH_SLOT (slot);
  maybe_emit_disp_id (expr, name, maybe_cmd_name); // Always, not maybe
}

void
bytecode_walker::
maybe_emit_disp_id (tree_expression &expr, const std::string &name, const std::string maybe_cmd_name)
{
  if (!expr.print_result ())
    return;

  if (name.size () && name[0] == '%') // Don't print internal variables like black holes
    return;

  // The Octave function inputname (i) needs to be able to know the name
  // of the argument to a function, so we need to make an entry of
  // the id printed if the user overloads display()
  arg_name_entry arg_name_entry;
  arg_name_entry.m_arg_names = string_vector {name};

  arg_name_entry.m_ip_start = CODE_SIZE ();

  CHECK (DEPTH () == 1);
  int slot = SLOT (name);
  MAYBE_PUSH_WIDE_OPEXT (slot);
  PUSH_CODE (INSTR::DISP);
  PUSH_SLOT (slot);
  // E.g. "x" might either be a command call x() that should print
  // "ans = ..." or a variable that should print "x = ..." so we
  // store the information on whether a certain symbol
  // was a variable or command call in a slot.
  // Some expressions like "1+1" are never command calls
  // ans have maybe_cmd_name as ""
  if (maybe_cmd_name != "")
    PUSH_WSLOT (SLOT (maybe_cmd_name));
  else
    PUSH_WSLOT (0);

  arg_name_entry.m_ip_end = CODE_SIZE ();
  PUSH_ARGNAMES_ENTRY (arg_name_entry);
}

void
bytecode_walker::
maybe_emit_bind_ans_and_disp (tree_expression &expr, const std::string maybe_cmd_name)
{
  bool print_result = expr.print_result ();

  // If this is an root expression we need to write the return value
  // to ans.
  if (DEPTH () == 1)
    {
      if (print_result)
        PUSH_CODE (INSTR::DUP);
      int slot = SLOT ("ans");
      MAYBE_PUSH_WIDE_OPEXT (slot);
      PUSH_CODE (INSTR::BIND_ANS);
      PUSH_SLOT (slot);
    }

  if (expr.is_identifier ())
    maybe_emit_disp_id (expr, expr.name (), maybe_cmd_name);
  else
    maybe_emit_disp_id (expr, "ans", maybe_cmd_name);
}

void
bytecode_walker::
emit_return ()
{
  // For loops, unwind protect and switches etc have stuff on the stack
  // inside them, so we need to pop those before executing the RET opcode.
  auto v = NESTING_STATEMENTS();
  // Reverse it backwards (top to bottom)
  for (auto it = v.rbegin () ;it !=  v.rend (); it++)
    {
      nesting_statement t = *it;
      switch (t)
        {
        case nesting_statement::FOR_LOOP:
          // We need to pop the counter and n
          PUSH_CODE (INSTR::POP_N_INTS);
          PUSH_CODE (2);
          // Pop the rhs ov (the range)
          PUSH_CODE (INSTR::POP);
          break;
        case nesting_statement::ONE_OV_ON_STACK:
          PUSH_CODE (INSTR::POP);
          break;
        default:
          ERR("Invalid state");
        }
    }

  PUSH_CODE (INSTR::RET);
}

void
bytecode_walker::
visit_return_command (tree_return_command &cmd)
{
  int loc_id = N_LOC ();
  PUSH_LOC ();
  LOC (loc_id).m_ip_start = CODE_SIZE ();

  // If we are in a unwind protect and returning we need to
  // run the cleanup code before returning.
  if (N_UNWIND_RETURN_TARGETS())
    {
      PUSH_CODE (INSTR::JMP);
      int need_unwind = CODE_SIZE ();
      PUSH_CODE_SHORT (-1); // Placeholder
      PUSH_A_UNWIND_RETURN_TARGET (need_unwind);
    }
  else
    emit_return ();

  LOC (loc_id).m_ip_end = CODE_SIZE ();
  LOC (loc_id).m_col = cmd.column ();
  LOC (loc_id).m_line = cmd.line ();
}

void
bytecode_walker::
visit_simple_assignment (tree_simple_assignment& expr)
{
  INC_DEPTH();
  PUSH_NARGOUT(1); // nargout is 1 for simple assignments

  tree_expression *lhs = expr.left_hand_side ();

  CHECK_NONNULL (lhs);

  if (!lhs->is_identifier() && !lhs->is_index_expression())
    TODO ("lhs not identifier or index expression");

  octave_value::assign_op op = expr.op_type ();

  // There is a general op-code SUBASSIGN_CHAINED for "complex" index assignments
  // and some specialized for "simple" ones
  bool complex_index_assignment = false;
  bool idx_has_ends = false;

  if (lhs->is_index_expression ())
    {
      tree_index_expression *idx = dynamic_cast<tree_index_expression*> (lhs);
      complex_index_assignment = idx->type_tags ().size () != 1;

      // We want to know if there is any magic end index in the arguments
      std::list<tree_argument_list *> args_lists = idx->arg_lists ();
      for (auto it = args_lists.begin (); it != args_lists.end (); it++)
        {
          if (!*it)
            continue;
          tree_argument_list *args = *it;
          for (auto it_args = args->begin (); it_args != args->end (); it_args++)
            {
              if (!*it_args)
                continue;
              if (find_end_walker::has_end (**it_args))
                idx_has_ends = true;
            }
        }

      if (op != octave_value::assign_op::op_asn_eq)
        complex_index_assignment = true;
    }

  if (complex_index_assignment)
    {
      if (idx_has_ends)
        {
          // TODO: Need lvalue walk to figure out how big subexpression are for end.
          //       Eval as workaround.

          // The VM need to access the tree expr.
          // Abuse the dbg info.
          PUSH_TREE_FOR_EVAL (&expr);
          int tree_idx = -CODE_SIZE (); // NB: Negative to not collide with debug data

          PUSH_CODE (INSTR::EVAL);
          PUSH_CODE (0); // nargout
          PUSH_CODE_INT (tree_idx);

          if (DEPTH () == 1)
            PUSH_CODE (INSTR::POP);

          POP_NARGOUT ();
          DEC_DEPTH ();
          return;
        }

      tree_index_expression *idx = dynamic_cast<tree_index_expression*> (lhs);

      tree_expression *e = idx->expression ();
      std::list<tree_argument_list *> args_lists = idx->arg_lists ();
      std::list<tree_expression *> dyns_fields = idx->dyn_fields ();
      std::list<string_vector> fields_names = idx->arg_names();
      std::string type_tags = idx->type_tags ();

      size_t n_chained = type_tags.size ();

      // Begin with rhs
      tree_expression *rhs = expr.right_hand_side ();
      CHECK_NONNULL (rhs);
      rhs->accept (*this);

      // rhs is on the stack now. If the assignment is nested we need to DUP rhs.
      // E.g. "a = b.c.d = 3" => a = 3 or "a = length (b.c.d = 3)" => a = 1
      if (DEPTH () != 1)
        PUSH_CODE (INSTR::DUP);

      if (e->is_identifier ())
        {
          // Name of the identifier
          std::string name = e->name ();

          int slot = add_id_to_table (name);

          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
          PUSH_SLOT (slot);
        }
      else
        {
          // Visit the lhs expression. This should put whatever
          // we are assigning to, on the stack.
          e->accept (*this);
        }

      // Subassigns are abit awkward on a stack VM, since we can't
      // do this piecewise. We need to construct a list of lists of
      // arguments to all the chained subassigns and feed them to
      // ov.assign (). TODO: make a "ref_subsasgn()" call or what
      // ever in ov.cc for the middle in the chain.
      //
      // Also, any inclusion of 'end' get quite annoying since we
      // need to save subsrefs to each chained subexpression to be
      // able to figure out the subexpressions sizes.

      auto it_args_lists = args_lists.begin ();
      auto it_dyns_fields = dyns_fields.begin ();
      auto it_fields_names = fields_names.begin ();

      int active_idx_slot = -1;
      if (idx_has_ends)
        {
          // We need to store the active subexpression in a slot for end
          // to be able to access it.
          std::string name = "%active_idx_" + std::to_string (CODE_SIZE ());
          add_id_to_table (name);
          active_idx_slot = SLOT (name);
          // Write the root value to the slot
          // i.e. root(2:end)(3,end)
          PUSH_CODE (INSTR::DUP);
          MAYBE_PUSH_WIDE_OPEXT (active_idx_slot);
          PUSH_CODE (INSTR::FORCE_ASSIGN);
          PUSH_SLOT (active_idx_slot);
        }

      std::vector<int> n_args_per_part;

      for (size_t i = 0; i < n_chained; i++)
        {
          // Amount of args in the subexpresseion
          // E.g. foo(1,2).bar(1) = ... => 2 and 1
          int n_args_in_part = 0;
          char type = type_tags[i];

          tree_argument_list *args = *it_args_lists++;
          tree_expression *dyn_fields = *it_dyns_fields++;
          string_vector field_names = *it_fields_names++;

          if (type == '.' && dyn_fields)
            {
              INC_DEPTH ();
              dyn_fields->accept (*this);
              DEC_DEPTH ();
              n_args_in_part++; // Dynamic struct fields are always one arg
            }
          else if (type == '.')
            {
              // We want to push the field name as a ovtave_string to the stack
              std::string field_name = field_names.elem (0);
              octave_value ov_field_name{field_name};
              PUSH_DATA (ov_field_name); // Make a constant
              // Load the constant
              PUSH_CODE_LOAD_CST (DATA_SIZE () - 1); // Offset of the constant

              n_args_in_part++;
            }
          else
            {
              // Push all the args to the stack

              n_args_in_part = args->size ();
              int j = 0;
              // We want to push the args to the stack
              // The order of eval is left to right
              for (auto it = args->begin (); it != args->end (); it++, j++)
                {
                  INC_DEPTH ();
                  // Any end will work on the active idx slot's object
                  PUSH_ID_BEGIN_INDEXED (active_idx_slot, j, n_args_in_part, false);
                  (*it)->accept (*this);
                  POP_ID_BEING_INDEXED ();
                  DEC_DEPTH ();
                }
            }

          // If we have an end in the assignment we need to write the active subexpression
          // to the designated slot for end to be able to access it.
          // Unecessary for the last in the chain.
          if (idx_has_ends && i + 1 != n_chained)
            {
              // Push the prior active index subexpression
              MAYBE_PUSH_WIDE_OPEXT (active_idx_slot);
              PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
              PUSH_SLOT (active_idx_slot);
              // Duplicate the args
              PUSH_CODE (INSTR::DUPN);
              PUSH_CODE (1); // offset, under the object being indexed
              PUSH_CODE (n_args_in_part); // amount of objects to duplicate
              // Index the prior active index subexpression
              PUSH_CODE (INSTR::INDEX_OBJ);
              PUSH_CODE (1); // nargout
              PUSH_CODE (0); // "has slot"
              PUSH_WSLOT (0); // The w/e slot
              PUSH_CODE (n_args_in_part);
              PUSH_CODE (type);
              // Write the new active subexpression back to the slot
              MAYBE_PUSH_WIDE_OPEXT (active_idx_slot);
              PUSH_CODE (INSTR::FORCE_ASSIGN);
              PUSH_SLOT (active_idx_slot);
            }

          n_args_per_part.push_back (n_args_in_part);
        }

      // So we have alot of arguments to different subexpression evaluated on the
      // stack now.
      //
      // We want to put them in lists and feed them to a subsassgn() call. We use
      // a special op-code for this.

      PUSH_CODE (INSTR::SUBASSIGN_CHAINED);
      PUSH_CODE (op); // =, += etc.
      PUSH_CODE (n_chained);
      for (unsigned i = 0; i < n_chained; i++)
        {
          PUSH_CODE (n_args_per_part[i]); // Amount of args, left to right
           // The type, i.e. '.' or '(' or '{'
          PUSH_CODE (type_tags[i]);
        }

      // Now we got the value that is subassigned to, on the stack
      if (e->is_identifier ())
        {
          // Write the subassigned value back to the slot
          int slot = SLOT (e->name ());
          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::FORCE_ASSIGN);
          PUSH_SLOT (slot);

          maybe_emit_push_and_disp_id (expr, e->name ());
        }
      else
        {
          if (expr.print_result ())
            {
              PUSH_CODE (INSTR::DUP);
              emit_disp_obj (expr);
            }
          if (DEPTH () == 1)
            PUSH_CODE (INSTR::POP);
        }
    }
  else if (lhs->is_index_expression()) // eg "foo(2) = bar" or "foo.a = bar"?
    {
      /* We have differen op codes for struct, cell, () index assignement
       * of ids and another for assignments where the rhs of the index is not
       * an id, e.g. foo.("bar") = 2 */

      CHECK (op == octave_value::assign_op::op_asn_eq);

      // We want the arguments to the index expression on the
      // operand stack. They are evaluated before the rhs expression.
      tree_index_expression *idx = dynamic_cast<tree_index_expression*> (lhs);

      tree_expression *ee = idx->expression ();
      std::list<tree_argument_list *> arg_lists = idx->arg_lists ();
      std::list<tree_expression *> dyn_fields = idx->dyn_fields ();
      std::list<string_vector> field_names = idx->arg_names();

      std::string type_tags = idx->type_tags ();
      CHECK (type_tags.size () == 1);
      CHECK (dyn_fields.size () == 1);
      CHECK (arg_lists.size () == 1);
      CHECK (field_names.size () == 1);

      char type = type_tags[0];


      bool is_id = ee->is_identifier ();
      CHECK_NONNULL(ee);

      bool is_dynamic_field = false;
      if (type == '.')
        {
          tree_expression *dyn_field = dyn_fields.front ();
          if (dyn_field)
            is_dynamic_field = true;
        }

      if (!is_id && type != '.')
        {
          tree_argument_list *arg = *arg_lists.begin ();

          // TODO: The other branches evaluate rhs after the arguments.
          //       Has to be wrong?
          tree_expression *rhs = expr.right_hand_side ();

          CHECK_NONNULL (rhs);
          rhs->accept (*this);
          // The value of rhs is on the stack now

          // Visit the lhs expression
          ee->accept (*this);
          // Pushed the left most lhs expression to the stack

          int nargs = 0;

          if (arg)
            {
              // If we are indexing an object, and have a magic end index
              // we need to save the stack depth in a slot
              bool obj_has_end = false;
              for (auto it = arg->begin (); it != arg->end (); it++)
                {
                  CHECK_NONNULL (*it);
                  tree_expression &t = **it;
                  obj_has_end = find_end_walker::has_end (t);
                  if (obj_has_end)
                    break;
                }

              int obj_stack_depth_slot = -1;
              if (obj_has_end)
                {
                  std::string obj_stack_depth_name = "%objsd_" + std::to_string (CODE_SIZE ());
                  obj_stack_depth_slot = add_id_to_table (obj_stack_depth_name);

                  MAYBE_PUSH_WIDE_OPEXT (obj_stack_depth_slot);
                  PUSH_CODE (INSTR::SET_SLOT_TO_STACK_DEPTH);
                  PUSH_SLOT (obj_stack_depth_slot);
                }

              nargs = arg->size ();
              int i = 0;
              // We want to push the args to the stack
              for (auto it = arg->begin (); it != arg->end (); it++, i++)
                {
                  INC_DEPTH ();
                  PUSH_ID_BEGIN_INDEXED (obj_stack_depth_slot, i, nargs, true);
                  (*it)->accept (*this);
                  POP_ID_BEING_INDEXED ();
                  DEC_DEPTH ();
                }
            }
          // rhs, lhs root expression, lhs's args on the stack now

          PUSH_CODE (INSTR::SUBASSIGN_OBJ);
          PUSH_CODE (nargs);
          PUSH_CODE (type);

          if (expr.print_result ())
            {
              PUSH_CODE (INSTR::DUP);
              emit_disp_obj (expr);
            }

          // SUBASSIGN_OBJ puts the lhs back on the stack
          // but since lhs is not an id from a slot we just
          // pop it unless it is used by a chained assign.
          if (DEPTH () == 1)
            PUSH_CODE (INSTR::POP);
        }
      else if (type == '(')
        {
          // Name of the identifier
          std::string name = ee->name ();

          add_id_to_table (name);

          tree_argument_list *arg = *arg_lists.begin ();

          int nargs = 0;
          if (arg)
            {
              nargs = arg->size ();
              int i = 0;
              // We want to push the args to the stack
              for (auto it = arg->begin (); it != arg->end (); it++, i++)
                {
                  INC_DEPTH ();
                  PUSH_ID_BEGIN_INDEXED (SLOT (name), i, nargs, false);
                  (*it)->accept (*this);
                  POP_ID_BEING_INDEXED ();
                  DEC_DEPTH ();
                }
            }

          tree_expression *rhs = expr.right_hand_side ();

          CHECK_NONNULL (rhs);
          rhs->accept (*this);
          // The value of rhs is on the operand stack now

          // If the assignment is not at root we want to keep the
          // value on the stack, e.g.
          //   a = b(1) = 3;
          //   Gives: a == 3
          // We use a slot to store the rhs in.
          std::string rhs_copy_nm = "%rhs_" + std::to_string (CODE_SIZE ());
          int slot_cpy = -1;
          if (DEPTH () != 1)
            {
              slot_cpy = add_id_to_table (rhs_copy_nm);
              PUSH_CODE (INSTR::DUP);
              MAYBE_PUSH_WIDE_OPEXT (slot_cpy);
              PUSH_CODE (INSTR::FORCE_ASSIGN);
              PUSH_SLOT (slot_cpy);
            }

          int slot = SLOT (name);
          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::SUBASSIGN_ID);
          PUSH_SLOT (slot);
          PUSH_CODE (nargs);

          if (DEPTH () != 1)
            {
              MAYBE_PUSH_WIDE_OPEXT (slot_cpy);
              PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
              PUSH_SLOT (slot_cpy);
            }

          maybe_emit_push_and_disp_id (expr, name);
        }
      else if (type == '.')
        {
          tree_expression *e = idx->expression ();
          CHECK_NONNULL(e);

          if (is_id && !is_dynamic_field)
            {
              // Name of the identifier
              std::string name = e->name ();

              add_id_to_table (name);

              std::list<string_vector>  l_pv_nms = idx->arg_names ();
              CHECK (l_pv_nms.size () == 1);
              auto pv_nms = l_pv_nms.begin ();
              CHECK (pv_nms->numel () == 1);

              std::string field_name = pv_nms->elem (0);

              // We just need the field's name in the VM
              int slot_field = add_id_to_table (field_name);

              tree_expression *rhs = expr.right_hand_side ();

              CHECK_NONNULL (rhs);
              rhs->accept (*this);
              // The value of rhs is on the operand stack now

              std::string rhs_copy_nm = "%rhs_" + std::to_string (CODE_SIZE ());
              int slot_cpy = -1;
              if (DEPTH () != 1) // Chained assignments?
                {
                  slot_cpy = add_id_to_table (rhs_copy_nm);
                  PUSH_CODE (INSTR::DUP);
                  MAYBE_PUSH_WIDE_OPEXT (slot_cpy);
                  PUSH_CODE (INSTR::FORCE_ASSIGN);
                  PUSH_SLOT (slot_cpy);
                }

              int slot = SLOT (name);
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::SUBASSIGN_STRUCT);
              PUSH_SLOT (slot);
              PUSH_WSLOT (slot_field);

              if (DEPTH () != 1)
                {
                  MAYBE_PUSH_WIDE_OPEXT (slot_cpy);
                  PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
                  PUSH_SLOT (slot_cpy);
                }

              maybe_emit_push_and_disp_id (expr, name);
            }
          else if (is_dynamic_field && is_id)
            {
              // Name of the identifier
              std::string name = e->name ();

              add_id_to_table (name);

              tree_expression *rhs = expr.right_hand_side ();
              CHECK_NONNULL (rhs);
              rhs->accept (*this);
              // The value of rhs is on the stack now

              // We want lhs on the stack
              int slot = SLOT (name);
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
              PUSH_SLOT (slot);

              // The argument, foo.(arg) = bar
              tree_expression *dyn_expr = dyn_fields.front ();
              CHECK_NONNULL (dyn_expr);

              INC_DEPTH ();
              PUSH_NARGOUT (1);
              dyn_expr->accept (*this);
              // The value of the arg on the stack, i.e. foo.(arg) = baz
              POP_NARGOUT ();
              DEC_DEPTH ();

              PUSH_CODE (INSTR::SUBASSIGN_OBJ);
              PUSH_CODE (1); // nargout
              PUSH_CODE (type);

              if (DEPTH () != 1) // Chained assignments?
                PUSH_CODE (INSTR::DUP);

              // Assign the assigned to value back to the slot
              // TODO: Neccessary?
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::FORCE_ASSIGN);
              PUSH_SLOT (slot);

              maybe_emit_push_and_disp_id (expr, name);
            }
          else if (!is_dynamic_field && !is_id)
            {
              tree_expression *rhs = expr.right_hand_side ();
              CHECK_NONNULL (rhs);
              rhs->accept (*this);
              // The value of rhs is on the operand stack now

              // Visit the lhs expression
              e->accept (*this);
              // Pushed the left most lhs expression to the stack

              string_vector ptr = field_names.front ();
              CHECK (ptr.numel() == 1);
              std::string field_name = ptr.elem (0);

              /* Make a ov string with the field name in it that
                * we store as a constant. */
              octave_value ov_field_name{field_name};
              PUSH_DATA (ov_field_name);

              PUSH_CODE_LOAD_CST (DATA_SIZE () - 1); // Offset of the constant

              PUSH_CODE (INSTR::SUBASSIGN_OBJ);
              PUSH_CODE (1); // nargout
              PUSH_CODE (type);

              if (expr.print_result ())
                {
                  PUSH_CODE (INSTR::DUP);
                  emit_disp_obj (expr);
                }

              // SUBASSIGN_OBJ puts the lhs back on the stack
              // but since lhs is not an id from a slot we just
              // pop it, unless there are chained assignments.
              if (DEPTH () == 1)
                PUSH_CODE (INSTR::POP);
            }
          else //(is_dynamic_field && !is_id)
            {
              tree_expression *rhs = expr.right_hand_side ();
              CHECK_NONNULL (rhs);
              rhs->accept (*this);
              // The value of rhs is on the operand stack now

              // Visit the lhs expression
              e->accept (*this);
              // Pushed the left most lhs expression to the stack

              // The argument, foo.(arg) = bar
              tree_expression *dyn_expr = dyn_fields.front ();
              CHECK_NONNULL (dyn_expr);

              INC_DEPTH ();
              PUSH_NARGOUT (1);
              dyn_expr->accept (*this);
              // The value of the arg on the stack, i.e. foo.(arg) = baz
              POP_NARGOUT ();
              DEC_DEPTH ();

              PUSH_CODE (INSTR::SUBASSIGN_OBJ);
              PUSH_CODE (1); // nargout
              PUSH_CODE (type);

              if (expr.print_result ())
                {
                  PUSH_CODE (INSTR::DUP);
                  emit_disp_obj (expr);
                }

              // SUBASSIGN_OBJ puts the lhs back on the stack
              // but since lhs is not an id from a slot we just
              // pop it, unless there are chained assignments.
              if (DEPTH () == 1)
                PUSH_CODE (INSTR::POP);
            }
        }
      else if (type == '{')
        {
          tree_expression *e = idx->expression ();
          CHECK_NONNULL(e);
          CHECK (e->is_identifier ());

          // Name of the identifier
          std::string name = e->name ();

          add_id_to_table (name);

          CHECK (arg_lists.size ());
          tree_argument_list *arg = *arg_lists.begin ();

          int nargs = 0;
          if (arg)
            {
              nargs = arg->size ();
              int i = 0;
              // We want to push the args to the stack
              for (auto it = arg->begin (); it != arg->end (); it++, i++)
                {
                  INC_DEPTH ();
                  PUSH_ID_BEGIN_INDEXED (SLOT (name), i, nargs, false);
                  (*it)->accept (*this);
                  POP_ID_BEING_INDEXED ();
                  DEC_DEPTH ();
                }
            }

          tree_expression *rhs = expr.right_hand_side ();

          CHECK_NONNULL (rhs);
          rhs->accept (*this);
          // The value of rhs is on the operand stack now

          // If the assignment is not at root we want to keep the
          // value on the stack, e.g.
          // a = b(1) = 3;
          // Gives: a == 3
          // We use a slot to store the rhs in.
          std::string rhs_copy_nm = "%rhs_" + std::to_string (CODE_SIZE ());
          int slot_cpy = -1;
          if (DEPTH () != 1)
            {
              slot_cpy = add_id_to_table (rhs_copy_nm);
              PUSH_CODE (INSTR::DUP);
              MAYBE_PUSH_WIDE_OPEXT (slot_cpy);
              PUSH_CODE (INSTR::FORCE_ASSIGN);
              PUSH_SLOT (slot_cpy);
            }

          int slot = SLOT (name);
          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::SUBASSIGN_CELL_ID);
          PUSH_SLOT (slot);
          PUSH_CODE (nargs);

          if (DEPTH () != 1)
            {
              MAYBE_PUSH_WIDE_OPEXT (slot_cpy);
              PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
              PUSH_SLOT (slot_cpy);
            }

          maybe_emit_push_and_disp_id (expr, name);
        }
      else
        TODO ("Type of subassignment not done yet");
    }
  else if (lhs->is_identifier ())
    {
      std::string name = lhs->name ();

      int slot = add_id_to_table (name);

      tree_expression *rhs = expr.right_hand_side ();

      CHECK_NONNULL (rhs);
      rhs->accept (*this);
      // The value of rhs is on the operand stack now

      if (op != octave_value::assign_op::op_asn_eq)
        {
          // Compound assignment have the type of operation in the code
          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::ASSIGN_COMPOUND);
          PUSH_SLOT (slot);
          PUSH_CODE (op);
        }
      else
        {
          // Ordinary assignment has its own opcode.
          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::ASSIGN);
          PUSH_SLOT (slot);
        }

      // If the assignment is not at root we want to keep the
      // value on the stack, e.g.
      // a = (b = 3);
      if (DEPTH () != 1)
        {
          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
          PUSH_SLOT (slot);
        }

      maybe_emit_push_and_disp_id (expr, name);
    }

  POP_NARGOUT ();
  DEC_DEPTH();
}

void
bytecode_walker::
visit_matrix (tree_matrix &m)
{
  INC_DEPTH ();

  bool is_rectangle = true;
  std::vector<int> row_lengths;

  /* We want to know if the matrix is rectangular. I.e.
   * all rows are of equal length. */
  size_t first_row_size = static_cast<size_t> (-1);
  for (auto it = m.begin (); it != m.end (); it++)
  {
    // This is a row
    tree_argument_list *row = *it;
    size_t row_size = row->size ();

    if (first_row_size == static_cast<size_t> (-1))
      first_row_size = row_size;
    else if (first_row_size != row_size)
      is_rectangle = false;

    row_lengths.push_back (row_size);
  }


  auto p = m.begin ();
  int n_rows = 0;
  int n_cols = 0;

  // Push each row element to operand stack
  while (p != m.end ())
    {
      // This is a row
      tree_argument_list *elt = *p++;

      n_cols = 0;
      CHECK_NONNULL (elt);
      for (auto it = elt->begin (); it != elt->end (); it++)
        {
          // This is an element
          tree_expression *e = *it;
          CHECK_NONNULL (e);

          INC_DEPTH ();
          e->accept (*this);
          DEC_DEPTH ();
          n_cols++;
        }
      n_rows++;
    }

  CHECK (n_cols > 0);
  CHECK (n_rows > 0);

  if (is_rectangle && n_cols < 256 && n_rows < 256) // Small rectangle matrix
    {
      PUSH_CODE (INSTR::MATRIX);
      PUSH_CODE (n_rows);
      PUSH_CODE (n_cols);
    }
  else if (is_rectangle) // Big rectangle matrix
    {
      PUSH_CODE (INSTR::MATRIX_UNEVEN);
      PUSH_CODE (1); // Type 1, Big rectangle matrix
      PUSH_CODE_INT (n_rows);
      PUSH_CODE_INT (n_cols);
    }
  else // Uneven matrix
    {
      PUSH_CODE (INSTR::MATRIX_UNEVEN);
      PUSH_CODE (0); // Type 0, Uneven matrix
      PUSH_CODE_INT (n_rows);
      for (int i : row_lengths)
        PUSH_CODE_INT (i);
    }

  maybe_emit_bind_ans_and_disp (m);

  DEC_DEPTH ();
}

void
bytecode_walker::
visit_cell (tree_cell &m)
{
  INC_DEPTH ();

  auto p = m.begin ();
  int n_rows = 0;
  int n_cols = -1;

  PUSH_CODE (INSTR::PUSH_OV_U64); // number of rows

  // Push each row element to operand stack
  while (p != m.end ())
    {
      // This is a row
      tree_argument_list *elt = *p++;

      PUSH_CODE (INSTR::PUSH_OV_U64); //number of columns

      int n_cols_old = n_cols;
      n_cols = 0;
      CHECK_NONNULL (elt);
      for (auto it = elt->begin (); it != elt->end (); it++)
        {
          // This is an element
          tree_expression *e = *it;
          CHECK_NONNULL (e);

          INC_DEPTH ();
          e->accept (*this);
          DEC_DEPTH ();
          n_cols++;

          // We now need to expand the value (if it is an cs list)
          // and rotate the counters to the top of the stack.
          //
          // Expand cslist does that in one opcode.
          PUSH_CODE (INSTR::EXPAND_CS_LIST);
        }

      if (n_cols > n_cols_old)
        n_cols_old = n_cols;

      // The amount of rows is on the second position of the stack,
      // rotate it with the amount of columns and increment the rows.
      PUSH_CODE (INSTR::ROT);
      PUSH_CODE (INSTR::INCR_PREFIX);

      n_rows++;
    }

  PUSH_CODE (INSTR::PUSH_CELL);

  maybe_emit_bind_ans_and_disp (m);

  DEC_DEPTH ();
}

void
bytecode_walker::
visit_identifier (tree_identifier& id)
{
  INC_DEPTH();

  std::string name = id.name ();
  if (name == "__VM_DBG")
  {
    PUSH_CODE (INSTR::PUSH_FALSE); // An id need to put something on the stack
    PUSH_CODE (INSTR::DEBUG);
  }
  // The magic end id need special handling
  else if (name == "end")
    {
      CHECK (ID_IS_BEING_INDEXED ());

      // Since in e.g. "M = [1 2 3]; M (min (10, end))" the 'end' will
      // refer to the end of M, not the function min, we need a special
      // op-code for nested indexings that can refer to any outer object
      int n_ids = N_IDS_BEING_INDEXED ();

      if (n_ids == 1) // Simple case
        {
          id_being_indexed obj = PEEK_ID_BEING_INDEXED ();
          if (obj.type == 0)
            {
              /* TODO: Is this op-code with slots really needed? */
              MAYBE_PUSH_WIDE_OPEXT (obj.slot);
              PUSH_CODE (INSTR::END_ID);
              PUSH_SLOT (obj.slot); // The slot variable being indexed
              PUSH_CODE (obj.nargs); // The amount of dimensions being indexed
              PUSH_CODE (obj.idx); // The offset of the index being indexed right now
            }
          else if (obj.type == 1)
            {
              MAYBE_PUSH_WIDE_OPEXT (obj.slot);
              PUSH_CODE (INSTR::END_OBJ);
              // Slot for keeping the stack depth of the object being indexed
              PUSH_SLOT (obj.slot);
              PUSH_CODE (obj.nargs); // The amount of dimensions being indexed
              PUSH_CODE (obj.idx); // The offset of the index being indexed right now
            }
          else
            panic_impossible ();
        }
      else // Nested indexing
        {
          PUSH_CODE (INSTR::END_X_N);
          PUSH_CODE (n_ids);

          // Note: Pushing inner to outer.
          // foo (bar (baz (1, end))) => 1: baz, 2: bar, 3: foo
          for (int i = n_ids - 1; i >= 0; i--)
            {
              id_being_indexed obj = IDS_BEING_INDEXED (i);
              PUSH_CODE (obj.nargs);
              PUSH_CODE (obj.idx);
              PUSH_CODE (obj.type);
              PUSH_WSLOT (obj.slot);
            }
        }
    }
  else
    {
      int slot = add_id_to_table (name);

      int loc_id = N_LOC ();
      PUSH_LOC ();
      LOC (loc_id).m_ip_start = CODE_SIZE ();

      if (m_pending_ignore_outputs && DEPTH () == 2)
        {
          PUSH_CODE (INSTR::SET_IGNORE_OUTPUTS);
          PUSH_CODE (m_v_ignored.size ());
          PUSH_CODE (m_ignored_of_total);
          for (int i : m_v_ignored)
            PUSH_CODE (i);
          m_ignored_ip_start = CODE_SIZE (); // visit_multi_assignment () need the code offset to set the proper range for the unwind protect
        }

      if (id.is_postfix_indexed ())
        {
          // "foo.a" and "foo{1}" might be command function calls
          // which is checked for in PUSH_SLOT_NARGOUT1_SPECIAL
          // Also foo might be a classdef meta object.
          MAYBE_PUSH_WIDE_OPEXT (slot);
          if (id.postfix_index () != '(')
            PUSH_CODE (INSTR::PUSH_SLOT_NARGOUT1_SPECIAL);
          else
            PUSH_CODE (INSTR::PUSH_SLOT_INDEXED);
          PUSH_SLOT (slot);
        }
      else if (DEPTH () == 1)
        {
          CHECK (NARGOUT () == 0);

          if (id.print_result ())
            {
              // Need to keep track of if this is a command call
              // or not for display since "x" will print "x = 3"
              // for e.g. variables but "ans = 3" for command calls.
              std::string maybe_cmd_name = "%maybe_command";
              int slot_cmd = add_id_to_table (maybe_cmd_name);
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::PUSH_SLOT_DISP);
              PUSH_SLOT (slot);
              PUSH_WSLOT (slot_cmd);

              maybe_emit_bind_ans_and_disp (id, maybe_cmd_name);
            }
          else
            {
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::PUSH_SLOT_NARGOUT0);
              PUSH_SLOT (slot);

              // Write the return value to ans. It is either the variables
              // value straight off, or e.g. a cmd function call return value.
              maybe_emit_bind_ans_and_disp (id);
            }
        }
      else if (NARGOUT () == 1)
        {
          // Push the local at its slot number to the stack
          MAYBE_PUSH_WIDE_OPEXT (slot);
          if (name == "pi")
            PUSH_CODE (INSTR::PUSH_PI);
          else
            PUSH_CODE (INSTR::PUSH_SLOT_NARGOUT1);
          PUSH_SLOT (slot);
        }
      else if (NARGOUT() > 1)
        {
          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::PUSH_SLOT_NARGOUTN);
          PUSH_SLOT (slot);
          PUSH_CODE (NARGOUT ());
        }
      else
        {
          // Push the local at its slot number to the stack
          MAYBE_PUSH_WIDE_OPEXT (slot);
          PUSH_CODE (INSTR::PUSH_SLOT_NARGOUT0);
          PUSH_SLOT (slot);
        }

      LOC (loc_id).m_ip_end = CODE_SIZE ();
      LOC (loc_id).m_col = id.column ();
      LOC (loc_id).m_line = id.line ();
    }
  DEC_DEPTH();
}

int
bytecode_walker::
add_id_to_table (std::string name)
{
  // Is the id already added to the local table?
  auto it = m_map_locals_to_slot.find (name);

  if (it == m_map_locals_to_slot.end ())
    {
      // Push local
      m_code.m_ids.push_back(name);
      m_map_locals_to_slot[name] = m_n_locals++;

      return m_n_locals - 1;
    }

  return it->second;
}

void
bytecode_walker::
visit_no_op_command (tree_no_op_command& cmd)
{
  if (cmd.is_end_of_fcn_or_script())
    {
      // Put a return in the end so that we don't fall of the edge
      // of the world

      int loc_id = N_LOC ();
      PUSH_LOC ();
      LOC (loc_id).m_ip_start = CODE_SIZE ();

      PUSH_TREE_FOR_DBG (&cmd);
      emit_return ();

      LOC (loc_id).m_ip_end = CODE_SIZE ();
      LOC (loc_id).m_col = cmd.column ();
      LOC (loc_id).m_line = cmd.line ();
    }
}

void
bytecode_walker::
visit_do_until_command (tree_do_until_command& cmd)
{
  tree_expression *expr = cmd.condition ();
  int code_start = CODE_SIZE ();

  tree_statement_list *list = cmd.body ();

  PUSH_CONTINUE_TARGET ();
  PUSH_BREAKS ();

  // Push an opcode that checks for signals, e.g. ctrl-c
  PUSH_CODE (INSTR::HANDLE_SIGNALS);

  // A empty body will yield a null list pointer
  m_n_nested_loops++;
  if (list)
    list->accept (*this);
  m_n_nested_loops--;

  // Any continue jumps to here (before the condition)
  for (int offset : POP_CONTINUE_TARGET())
    SET_CODE_SHORT (offset, CODE_SIZE ());

  CHECK_NONNULL (expr);
  INC_DEPTH (); // Since we need the value
  PUSH_TREE_FOR_DBG (expr);
  expr->accept (*this);
  DEC_DEPTH ();

  // The condition value is on the operand stack, do
  // a jmp_ifn to the start of the body, on false
  PUSH_CODE (INSTR::JMP_IFN);
  PUSH_CODE_SHORT (code_start);

  // The breaks jump to here
  for (int offset : POP_BREAKS ())
    SET_CODE_SHORT (offset, CODE_SIZE ());
}

void
bytecode_walker::
visit_while_command (tree_while_command& cmd)
{
  tree_expression *expr = cmd.condition ();

  // Location data for the condition
  int loc_id = N_LOC ();
  PUSH_LOC ();
  LOC (loc_id).m_ip_start = CODE_SIZE ();

  int cond_offset = CODE_SIZE ();

  CHECK_NONNULL (expr);
  INC_DEPTH (); // Since we need the value
  PUSH_TREE_FOR_DBG (expr);
  expr->accept (*this);
  DEC_DEPTH ();

  // The condition value is on the operand stack, do
  // a jmp_ifn to after the body, on false
  PUSH_CODE (INSTR::JMP_IFN);
  int offset_need_jmp_after = CODE_SIZE ();
  PUSH_CODE_SHORT (-1); // Placeholder

  LOC (loc_id).m_ip_end = CODE_SIZE ();
  LOC (loc_id).m_col = expr->column ();
  LOC (loc_id).m_line = expr->line ();

  tree_statement_list *list = cmd.body ();

  PUSH_CONTINUE_TARGET ();
  PUSH_BREAKS ();

  // Push an opcode that checks for signals, e.g. ctrl-c
  PUSH_CODE (INSTR::HANDLE_SIGNALS);

  // nullptr if body is empty
  m_n_nested_loops++;
  if (list)
    list->accept (*this);
  m_n_nested_loops--;

  // The continue targets can now be set, to jump back
  // to the condition.
  for (int offset : POP_CONTINUE_TARGET())
    SET_CODE_SHORT (offset, cond_offset);

  // Jump back to the condition, TODO: unless all paths are terminated
  PUSH_CODE (INSTR::JMP);
  PUSH_CODE_SHORT (cond_offset);

  // Now we can set where the condition should jump on false, i.e.
  // to here, after the jump back to the condition
  SET_CODE_SHORT (offset_need_jmp_after, CODE_SIZE ());

  // The breaks jump to the same place
  for (int offset : POP_BREAKS ())
    SET_CODE_SHORT (offset, CODE_SIZE ());
}

void
bytecode_walker::
visit_switch_command (tree_switch_command& cmd)
{
  tree_expression *expr = cmd.switch_value ();
  CHECK_NONNULL (expr);

  tree_switch_case_list *lst = cmd.case_list ();

  std::vector<int> need_after_all;

  // First off we need the switch value on the stack
  INC_DEPTH ();
  PUSH_NARGOUT(1);

  expr->accept (*this);

  POP_NARGOUT ();
  DEC_DEPTH ();

  // Since the switch have a value on the stack through the whole switch
  // statement we need to track that so returns can pop it.
  PUSH_NESTING_STATEMENT (nesting_statement::ONE_OV_ON_STACK);

  // Any nested continue or break need to pop the switch value
  PUSH_CONTINUE_TARGET ();
  PUSH_BREAKS ();

  // We now have the switch value on the operand stack,
  // so now we need to compare it with the first label
  // either execute its code or skip it depending on
  // wheter the switch value and the label are "equal"

  tree_switch_case *default_case = nullptr;

  if (lst)
    for (tree_switch_case *t : *lst)
      {
        // We want to do the default case last
        if (t->is_default_case ())
          {
            default_case = t;
            continue;
          }

        // We need to duplicate the switch value on the stack so
        // each label will have its own
        PUSH_CODE (INSTR::DUP);

        INC_DEPTH ();
        PUSH_NARGOUT(1);

        // Walk for code for the case label expression
        t->case_label()->accept(*this);

        POP_NARGOUT ();
        DEC_DEPTH ();

        // case label value is now on the stack

        PUSH_CODE (INSTR::JMP_IFNCASEMATCH);
        int need_next = CODE_SIZE ();
        PUSH_CODE_SHORT (-1);

        // Walk for the case label body

        tree_statement_list *stmt_lst = t->commands ();

        if (stmt_lst)
          stmt_lst->accept (*this);

        // TODO: Unless the body is terminated we need to jump past
        // the rest of the switch bodies
        PUSH_CODE (INSTR::JMP);
        need_after_all.push_back (CODE_SIZE ());
        PUSH_CODE_SHORT (-1); // Placeholder, jump to after all

        // If the label was not "true" we jump to here. Under there will be
        // another case or the end of the switch
        SET_CODE_SHORT (need_next, CODE_SIZE ()); // The placeholder above
      }

  // If there was a default case, generate code for it
  if (default_case)
    {
      tree_statement_list *stmt_lst = default_case->commands();

      if (stmt_lst)
        stmt_lst->accept (*this);
    }

  // Any nested break or continue need to jump here to pop an ov
  // and then jump to a outer break or continue block.
  auto v_breaks = POP_BREAKS ();
  auto v_continues = POP_CONTINUE_TARGET ();

  if (v_breaks.size () || v_continues.size ())
    {
      // Fallthrough from default need to jump past break and continue bridges
      PUSH_CODE (INSTR::JMP);
      int offset = CODE_SIZE ();
      need_after_all.push_back (offset);
      PUSH_CODE_SHORT (-1);
    }

  if (v_breaks.size ())
    {
      for (int offset : v_breaks)
        SET_CODE_SHORT (offset, CODE_SIZE ());
      // We need to pop the switch value
      PUSH_CODE (INSTR::POP);
      // Jump to the outer break target
      PUSH_CODE (INSTR::JMP);
      int offset = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);
      PUSH_NEED_BREAK (offset);
    }
  if (v_continues.size ())
    {
      // Nested continues should jump to there
      int target_offset = CODE_SIZE ();
      for (int offset : v_continues)
        SET_CODE_SHORT (offset, target_offset);
      // We need to pop the switch value
      PUSH_CODE (INSTR::POP);
      // Jump to the outer continue target (i.e. start of whatever loop)
      PUSH_CODE (INSTR::JMP);
      int need_continue_target = CODE_SIZE ();
      PUSH_CODE_SHORT (-1);
      PUSH_NEED_CONTINUE_TARGET (need_continue_target);
    }

  // Some code points might need a jump to after the switch statement
  for (int offset : need_after_all)
    SET_CODE_SHORT (offset, CODE_SIZE ());

  // We need to pop the switch value
  PUSH_CODE (INSTR::POP);

  // We are out of the switch statement so pop it from the nesting stack
  POP_NESTING_STATEMENT ();
}

void
bytecode_walker::
visit_if_command (tree_if_command& cmd)
{
  tree_if_command_list *list = cmd.cmd_list ();
  CHECK_NONNULL (list);

  // Offset to jump addresses that will need to jump
  // to after all the if clauses and bodies. E.g.
  // the end of each if body, if there are more than one if.
  std::vector<int> need_after_all;

  // Offset for the jump address of the condition test, which
  // need to go to after the body.
  int need_after_body = -1;

  std::size_t n = list->size ();

  std::size_t idx = 0;
  for (auto p = list->begin (); p != list->end (); p++, idx++)
    {
      bool is_last = idx + 1 == n;

      tree_if_clause *elt = *p;
      CHECK_NONNULL (elt);

      tree_statement_list *body = elt->commands ();

      bool is_not_else = ! elt->is_else_clause ();
      // Condition
      if (is_not_else)
        {
          tree_expression *cond = elt->condition ();
          CHECK_NONNULL (cond);

          // Location data for the condition
          int loc_id = N_LOC ();
          PUSH_LOC ();
          LOC (loc_id).m_ip_start = CODE_SIZE ();

          PUSH_TREE_FOR_DBG (elt); // We want the debug hit before the condition

          PUSH_NARGOUT (1);
          INC_DEPTH ();
          cond->accept (*this);
          DEC_DEPTH ();
          POP_NARGOUT ();

          // The condition is on the operand stack now
          PUSH_CODE (INSTR::JMP_IFN);
          need_after_body = CODE_SIZE ();
          PUSH_CODE_SHORT (-1); // Placeholder, jump to after all

          LOC (loc_id).m_ip_end = CODE_SIZE ();
          LOC (loc_id).m_col = cond->column ();
          LOC (loc_id).m_line = cond->line ();
        }

      // Body
      // nullptr if body is empty
      if (body)
        body->accept (*this);

      if (!is_last)
        {
          PUSH_CODE (INSTR::JMP);
          need_after_all.push_back (CODE_SIZE ());
          PUSH_CODE_SHORT (-1); // Placeholder, jump to after all
        }

      // Now we can set the address to which failed condition
      // will jump
      if (is_not_else)
        SET_CODE_SHORT (need_after_body, CODE_SIZE ());
    }

  for (int offset : need_after_all)
    SET_CODE_SHORT (offset, CODE_SIZE ());
}

void
bytecode_walker::
visit_anon_fcn_handle (tree_anon_fcn_handle &expr)
{
  INC_DEPTH ();

  PUSH_TREE_FOR_EVAL (&expr);
  int tree_idx = -CODE_SIZE ();

  PUSH_CODE (INSTR::PUSH_ANON_FCN_HANDLE);
  PUSH_CODE_INT (tree_idx);

  maybe_emit_bind_ans_and_disp (expr);

  DEC_DEPTH ();
}

void
bytecode_walker::
emit_args_for_visit_index_expression (tree_argument_list *arg_list,
                                      tree_expression *root_lhs_id)
{
  int nargs = arg_list->size ();
  int idx = 0;
  bool lhs_is_id = root_lhs_id ? root_lhs_id->is_identifier () : false;

  // If we are indexing an object, and have a magic end index
  // we need to save the stack depth in a slot
  bool obj_has_end = false;
  if (!lhs_is_id)
    {
      for (auto it = arg_list->begin (); it != arg_list->end (); it++)
        {
          CHECK_NONNULL (*it);
          obj_has_end = find_end_walker::has_end (**it);
          if (obj_has_end)
            break;
        }
    }

  int obj_stack_depth_slot = -1;
  if (obj_has_end)
    {
      std::string obj_stack_depth_name = "%objsd_" + std::to_string (CODE_SIZE ());
      obj_stack_depth_slot = add_id_to_table (obj_stack_depth_name);

      MAYBE_PUSH_WIDE_OPEXT (obj_stack_depth_slot);
      PUSH_CODE (INSTR::SET_SLOT_TO_STACK_DEPTH);
      PUSH_SLOT (obj_stack_depth_slot);
    }

  // We want to push the args to the stack
  for (auto it = arg_list->begin (); it != arg_list->end (); it++, idx++)
    {
      INC_DEPTH ();
      if (lhs_is_id)
        PUSH_ID_BEGIN_INDEXED (SLOT (root_lhs_id->name ()), idx, nargs, false);
      else
        PUSH_ID_BEGIN_INDEXED (obj_stack_depth_slot, idx, nargs, true);

      PUSH_NARGOUT (1);
      (*it)->accept (*this);
      POP_NARGOUT ();
      POP_ID_BEING_INDEXED ();
      DEC_DEPTH ();
    }
}

void
bytecode_walker::
emit_fields_for_visit_index_expression (string_vector &field_names,
                                        tree_expression *dyn_expr,
                                        tree_expression *lhs_root,
                                        bool *struct_is_id_dot_id)
{
  if (struct_is_id_dot_id)
    *struct_is_id_dot_id = false;
  // For struct the "arg" is the field and not executed.
  // Just add it as an identifier so that we can get it's
  // name as a string in the VM.
  CHECK (field_names.numel() == 1);

  std::string field_name = field_names.elem (0);

  if (lhs_root && lhs_root->is_identifier () && field_name.size ())
    {
      if (struct_is_id_dot_id)
        *struct_is_id_dot_id = true;
      add_id_to_table (field_name);
    }
  else if (field_name.size ())
    {
      octave_value ov_field_name{field_name};
      PUSH_DATA (ov_field_name);

      PUSH_CODE_LOAD_CST (DATA_SIZE () - 1); // Offset of the constant
    }
  else
    {
      CHECK_NONNULL (dyn_expr);

      INC_DEPTH ();
      PUSH_NARGOUT (1);
      dyn_expr->accept (*this);
      POP_NARGOUT ();
      DEC_DEPTH ();
    }
}

void
bytecode_walker::
eval_visit_index_expression (tree_index_expression& expr)
{
  INC_DEPTH ();
  tree_expression *e = expr.expression ();
  CHECK_NONNULL(e);

  PUSH_TREE_FOR_EVAL (&expr);
  int tree_idx = -CODE_SIZE (); // NB: Negative to not collide with debug data

  PUSH_CODE (INSTR::EVAL);
  PUSH_CODE (NARGOUT ());
  PUSH_CODE_INT (tree_idx);

  maybe_emit_bind_ans_and_disp (expr);

  if (DEPTH () == 1 && NARGOUT () > 1)
    TODO ("Silly state");

  DEC_DEPTH ();
}

void
bytecode_walker::
simple_visit_index_expression (tree_index_expression& expr)
{
  INC_DEPTH ();
  tree_expression *e = expr.expression ();
  CHECK_NONNULL(e);

  // Word commands are on the form:
  // foo bar baz; <=> foo('bar', 'baz');
  bool is_wordcmd = expr.is_word_list_cmd ();

  std::string type_tags = expr.type_tags ();

  size_t n_chained = type_tags.size ();
  CHECK (n_chained == 1);

  // Put the object to index on the stack
  INC_DEPTH ();
  e->accept (*this);
  DEC_DEPTH ();

  // The Octave function inputname (i) needs to be able to know the name
  // of th nth argument to a function, so we need to make an entry of
  // the names.
  arg_name_entry arg_name_entry;

  std::list<octave::tree_argument_list *> arg_lists = expr.arg_lists ();
  std::list<string_vector> arg_names = expr.arg_names ();
  std::list<octave::tree_expression *> dyn_fields = expr.dyn_fields ();

  CHECK (arg_lists.size () == n_chained);
  CHECK (arg_names.size () == n_chained);
  CHECK (dyn_fields.size () == n_chained);
  CHECK (type_tags.size () == n_chained);

  auto arg_names_it = arg_names.begin ();
  auto arg_lists_it = arg_lists.begin ();
  auto arg_lists_dyn_it = dyn_fields.begin ();
  auto arg_type_tags_it = type_tags.begin ();

  char type = *arg_type_tags_it;

  int nargout = NARGOUT ();

  bool struct_is_id_dot_id = false;
  if (type == '.')
    emit_fields_for_visit_index_expression (*arg_names_it, *arg_lists_dyn_it, e, &struct_is_id_dot_id);
  else if (*arg_lists_it)
    {
      emit_args_for_visit_index_expression (*arg_lists_it, e);
      // Push the argnames for inputname ()
      size_t n_args = arg_names_it->numel ();
      string_vector names(n_args);
      for (int i = 0; i < arg_names_it->numel (); i++)
        names.elem (i) = arg_names_it->elem (i);
      arg_name_entry.m_arg_names = names;
    }

  if (m_pending_ignore_outputs && DEPTH () == 2)
    {
      PUSH_CODE (INSTR::SET_IGNORE_OUTPUTS);
      PUSH_CODE (m_v_ignored.size ());
      PUSH_CODE (m_ignored_of_total);
      for (int i : m_v_ignored)
        PUSH_CODE (i);
      m_ignored_ip_start = CODE_SIZE (); // visit_multi_assignment () need the code offset to set the proper range for the unwind protect
    }

  int loc_id = N_LOC ();
  PUSH_LOC ();
  LOC (loc_id).m_ip_start = CODE_SIZE ();

  arg_name_entry.m_ip_start = CODE_SIZE ();

  tree_argument_list *args = *arg_lists_it;

  if (is_wordcmd)
    {
      CHECK (e->is_identifier ());

      std::string id_name = e->name ();
      int slot = SLOT (id_name);
      MAYBE_PUSH_WIDE_OPEXT (slot);
      PUSH_CODE (INSTR::WORDCMD);
      // The vm need the name of the identifier for function lookups
      PUSH_SLOT (slot);
      PUSH_CODE (nargout);
      // Push nargin
      PUSH_CODE (args ? args->size () : 0);
    }
  else if (e->is_identifier () && !(type == '.' && !struct_is_id_dot_id))
    {
      std::string id_name = e->name ();
      int slot = SLOT (id_name);

      if (type == '(')
        {
          if (nargout == 0)
            {
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::INDEX_ID_NARGOUT0);
              // The vm need the name of the identifier for function lookups
              PUSH_SLOT (slot);
            }
          else if (nargout == 1)
            {
              // If the id is "sin", "cos", "round" etc, and there is one argument,
              // in the end map(unary_mapper_t) will be called while executing,
              // unless the user have overriden those.
              // We do a special opcode for those to speed them up.
              // Don't do the special opcode if it would need wide slots, i.e. slot nr > 256.
              auto umaped_fn_it = m_name_to_unary_func.find (id_name);
              if (!args || args->size () != 1 || umaped_fn_it == m_name_to_unary_func.end () || slot > 256)
                {
                  MAYBE_PUSH_WIDE_OPEXT (slot);
                  PUSH_CODE (INSTR::INDEX_ID_NARGOUT1);
                }
              else
                {
                  octave_base_value::unary_mapper_t idx = umaped_fn_it->second;
                  PUSH_CODE (INSTR::INDEX_ID1_MATHY_UFUN);
                  PUSH_CODE (static_cast<int> (idx));
                }

              PUSH_SLOT (slot);
            }
          else
            {
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::INDEX_IDN);
              PUSH_SLOT (slot);
              PUSH_CODE (nargout);
            }

          // Push nargin
          PUSH_CODE (args ? args->size () : 0);
        }
      else if (type == '{')
        {
          if (nargout == 0)
            {
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::INDEX_CELL_ID_NARGOUT0);
              // The vm need the name of the identifier for function lookups
              PUSH_SLOT (slot);
            }
          else if (nargout == 1)
            {
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::INDEX_CELL_ID_NARGOUT1);
              PUSH_SLOT (slot);
            }
          else
            {
              MAYBE_PUSH_WIDE_OPEXT (slot);
              PUSH_CODE (INSTR::INDEX_CELL_ID_NARGOUTN);
              PUSH_SLOT (slot);
              PUSH_CODE (nargout);
            }

          // Push nargin
          PUSH_CODE (args ? args->size () : 0);
        }
      else if (type == '.')
        {
          PUSH_CODE (INSTR::INDEX_STRUCT_NARGOUTN);
          PUSH_CODE (nargout);

          string_vector field_names = *arg_names_it;
          CHECK (field_names.numel ());
          std::string field_name = field_names.elem (0);

          PUSH_WSLOT (slot);   // id to index
          PUSH_WSLOT (SLOT (field_name)); // VM need name of the field
        }
      else
        TODO ("Not implemeted typetag");
    }
  else
    {
      // We are not indexing an id, but e.g.:
      // (foo).()
      // I.e. a temporary object.
      PUSH_CODE (INSTR::INDEX_OBJ);
      PUSH_CODE (nargout);
      PUSH_CODE (0); // "has slot"
      PUSH_WSLOT (0); // The w/e slot TODO: Remove?
      // Push nargin
      if (type == '.')
        PUSH_CODE (1); // Nargin always one for struct indexing
      else
        PUSH_CODE (args ? args->size () : 0);
      PUSH_CODE (type);
    }

  arg_name_entry.m_ip_end = CODE_SIZE ();
  PUSH_ARGNAMES_ENTRY (arg_name_entry);

  LOC (loc_id).m_ip_end = CODE_SIZE ();
  LOC (loc_id).m_col = expr.column ();
  LOC (loc_id).m_line = expr.line ();

  maybe_emit_bind_ans_and_disp (expr);

  if (DEPTH () == 1 && NARGOUT () > 1)
    TODO ("Silly state");

  DEC_DEPTH ();
}

void
bytecode_walker::
visit_index_expression (tree_index_expression& expr)
{

  tree_expression *e = expr.expression ();
  CHECK_NONNULL(e);

  std::list<octave::tree_argument_list *> arg_lists = expr.arg_lists ();
  std::list<string_vector> arg_names = expr.arg_names ();
  std::list<octave::tree_expression *> dyn_fields = expr.dyn_fields ();
  std::string type_tags = expr.type_tags ();

  size_t n_chained = type_tags.size ();
  CHECK (n_chained);

  // For un-chained index expressions we use specialized
  // op-codes that has e.g. nargout and type '(','{' and '.'
  // encoded in the op-code it self to speed things up.
  if (n_chained == 1)
    {
      simple_visit_index_expression (expr);
      return;
    }

  // If there is any struct in the chain and an end, we cheat and use
  // eval. We can't use the existing end op-codes since they need a value
  // to check the size of, but e.g. "foo.bar(end)" might be a
  // class cmd form method call "foo.bar" and we can't know the size before
  // that has been checked.
  //
  // TODO: Solve this. Maybe with some special if?
  bool has_end = false;
  for (auto outer_it = arg_lists.begin (); outer_it != arg_lists.end (); outer_it++)
    {
      auto arg_list = *outer_it;
      if (!arg_list)
        continue;
      for (auto it = arg_list->begin (); it != arg_list->end (); it++)
        {
          CHECK_NONNULL (*it);
          has_end = find_end_walker::has_end (**it);
          if (has_end)
            break;
        }
    }
  if (has_end)
    {
      eval_visit_index_expression (expr);
      return;
    }

  INC_DEPTH ();

  // A chained index expression might be: foo.bar(2).baz{1} => n_chained == 4

  int loc_id = N_LOC ();
  PUSH_LOC ();
  LOC (loc_id).m_ip_start = CODE_SIZE ();

  // The Octave function inputname (i) needs to be able to know the name
  // of th nth argument to a function, so we need to make an entry of
  // the names.
  arg_name_entry arg_name_entry;

  // We push the first object to index to the stack.
  // Subsequent indexings will have the prior index result on the
  // stack.
  INC_DEPTH ();
  e->accept (*this);
  DEC_DEPTH ();

  CHECK (arg_lists.size () == n_chained);
  CHECK (arg_names.size () == n_chained);
  CHECK (dyn_fields.size () == n_chained);
  CHECK (type_tags.size () == n_chained);

  auto arg_names_it = arg_names.begin ();
  auto arg_lists_it = arg_lists.begin ();
  auto arg_lists_dyn_it = dyn_fields.begin ();
  auto arg_type_tags_it = type_tags.begin ();

  tree_expression *first_expression = e;
  // Iterate over the chained subexpressions
  std::vector<int> v_n_args {}; // We pushed one field above
  std::vector<int> v_types {};// The type is .
  while (arg_lists_it != arg_lists.end ())
    {
      tree_argument_list *arg_list = *arg_lists_it++;
      string_vector field_names = *arg_names_it++;
      tree_expression *dyn_expr = *arg_lists_dyn_it++;
      char type = *arg_type_tags_it++;

      v_types.push_back (type);

      if (type == '.')
        {
          emit_fields_for_visit_index_expression (field_names, dyn_expr, nullptr, nullptr);
          v_n_args.push_back (1);
        }
      else if (arg_list)
        {
          emit_args_for_visit_index_expression (arg_list, nullptr);
          v_n_args.push_back (arg_list->size ());
          // Push the argnames for inputname ()
          int n_args = field_names.numel ();
          string_vector names(n_args);
          for (int i = 0; i < n_args; i++)
            names.elem (i) = field_names.elem (i);
          arg_name_entry.m_arg_names = names;
        }
      else
        v_n_args.push_back (0); // e.g. the call to "bar" in "foo.bar ()"
    }

  if (m_pending_ignore_outputs && DEPTH () == 2)
    {
      PUSH_CODE (INSTR::SET_IGNORE_OUTPUTS);
      PUSH_CODE (m_v_ignored.size ());
      PUSH_CODE (m_ignored_of_total);
      for (int i : m_v_ignored)
        PUSH_CODE (i);
      m_ignored_ip_start = CODE_SIZE (); // visit_multi_assignment () need the code offset to set the proper range for the unwind protect        
    }

  int nargout = NARGOUT ();

  arg_name_entry.m_ip_start = CODE_SIZE ();

  if (first_expression && first_expression->is_identifier ())
    {
      int slot = SLOT (first_expression->name ());
      MAYBE_PUSH_WIDE_OPEXT (slot);
      PUSH_CODE (INSTR::INDEX_STRUCT_CALL);
      PUSH_SLOT (slot); // the slot
      PUSH_CODE (1); // has slot
      PUSH_CODE (nargout);
    }
  else
    {
      PUSH_CODE (INSTR::INDEX_STRUCT_CALL);
      PUSH_SLOT (0); // slot
      PUSH_CODE (0); // has slot
      PUSH_CODE (nargout);
    }

  PUSH_CODE (v_n_args.size ());
  for (unsigned i = 0; i < v_n_args.size (); i++)
    {
      PUSH_CODE (v_n_args[i]);
      PUSH_CODE (v_types[i]);
    }

  arg_name_entry.m_ip_end = CODE_SIZE ();
  PUSH_ARGNAMES_ENTRY (arg_name_entry);
  arg_name_entry = {}; // TODO: Remove?

  LOC (loc_id).m_ip_end = CODE_SIZE ();
  LOC (loc_id).m_col = expr.column ();
  LOC (loc_id).m_line = expr.line ();

  maybe_emit_bind_ans_and_disp (expr);

  if (DEPTH () == 1 && NARGOUT () > 1)
    TODO ("Silly state");

  DEC_DEPTH ();
}

// For loops are setup like this:
//
// Setup block:
//   * The range variable is on the top of the stack
//   * Push the amount of iterations to the stack, octave_idx_type
//   * Push a counter to the stack initialized to ~0, octave_idx_type,
//     so that it wraps to zero after incrementing.
//   * Fall through to condition block
// Condition block:
//   * Increase counter
//   * If there are no iterations left, go to after.
//   * Write the iteration's value to the local
//   * Fall through to body
// Body block:
//   * Execute the body code
//   * Jump to condition block
// After block:
//   * Pop the type, counter and limit variables
//
// FOR_SETUP = opcode
// FOR_COND  = opcode, after address, local slot

void
bytecode_walker::
visit_simple_for_command (tree_simple_for_command& cmd)
{
  tree_expression *lhs = cmd.left_hand_side ();

  int loc_id = N_LOC ();
  PUSH_LOC ();
  LOC (loc_id).m_ip_start = CODE_SIZE ();

  CHECK_NONNULL (lhs);
  if (! lhs->is_identifier ())
    TODO ("For loop with lhs not id ???");

  std::string id_name = lhs->name ();
  // We don't want the id pushed to the stack so we
  // don't walk it.
  int slot = add_id_to_table (id_name);

  tree_expression *expr = cmd.control_expr ();
  CHECK_NONNULL (expr);

  PUSH_TREE_FOR_DBG (&cmd); // Debug hit before rhs

  // We want the rhs expression on the stack
  INC_DEPTH ();
  PUSH_NARGOUT (1);
  expr->accept (*this);
  POP_NARGOUT ();
  DEC_DEPTH ();

  // For loops need a special unwind entry to destroy the
  // native ints on the stack properly.
  int unwind_idx = N_UNWIND ();
  PUSH_UNWIND();
  UNWIND (unwind_idx).m_ip_start = CODE_SIZE ();

  UNWIND (unwind_idx).m_unwind_entry_type =
    unwind_entry_type::FOR_LOOP;

  // For loops add two native ints and one ov to the stack,
  // and switches add one ov to the stack, so we need to
  // record how many things we have added to the stack,
  // not counting this for loop. From for loops and
  // switches.
  int n_things_on_stack = n_on_stack_due_to_stmt();

  // Store added things on stack (due to for loops and switches)
  // in the unwind table.
  UNWIND (unwind_idx).m_stack_depth = n_things_on_stack;

  PUSH_CODE (INSTR::FOR_SETUP);
  // FOR_COND need to come after FOR_SETUP
  // FOR_SETUP uses FOR_COND's operands the first loop iteration
  PUSH_TREE_FOR_DBG (&cmd); // Debug hit at condition
  int cond_offset = CODE_SIZE ();
  MAYBE_PUSH_WIDE_OPEXT (slot);
  PUSH_CODE (INSTR::FOR_COND);
  PUSH_SLOT (slot); // The for loop variable
  int need_after = CODE_SIZE ();
  PUSH_CODE_SHORT (-1); // Placeholder for after address

  LOC (loc_id).m_ip_end = CODE_SIZE ();
  LOC (loc_id).m_col = cmd.column ();
  LOC (loc_id).m_line = cmd.line ();

  // Walk body
  tree_statement_list *list = cmd.body ();

  // The body can be empty
  if (list)
    {
      m_n_nested_loops++;
      PUSH_NESTING_STATEMENT (nesting_statement::FOR_LOOP);
      PUSH_BREAKS ();
      PUSH_CONTINUE_TARGET ();
      list->accept (*this);
      for (int offset : POP_CONTINUE_TARGET())
        SET_CODE_SHORT (offset, cond_offset);
      POP_NESTING_STATEMENT ();
      m_n_nested_loops--;
    }

  // A new loc for the for loop suffix code, so that any time
  // spent there end up by the "for"-row in the profiler

  int loc_id2 = N_LOC ();
  PUSH_LOC ();
  LOC (loc_id2).m_ip_start = CODE_SIZE ();

  // Jump to condition block, TODO: unless all paths terminated
  PUSH_CODE (INSTR::JMP);
  PUSH_CODE_SHORT (cond_offset);

  // Now we can set the after jump in cond
  SET_CODE_SHORT (need_after, CODE_SIZE ());

  if (list)
    {
      // Also all breaks jump to here
      for (int need_break : POP_BREAKS ())
        {
          SET_CODE_SHORT (need_break, CODE_SIZE ());
        }
    }

  // Mark an end to the special for loop unwind entry
  UNWIND (unwind_idx).m_ip_end = CODE_SIZE ();

  // We need to pop the counter, n and range
  PUSH_CODE (INSTR::POP_N_INTS);
  PUSH_CODE (2);
  // Pop the rhs ov (the range)
  PUSH_CODE (INSTR::POP);

  LOC (loc_id2).m_ip_end = CODE_SIZE ();
  LOC (loc_id2).m_col = cmd.column ();
  LOC (loc_id2).m_line = cmd.line ();
}

void
bytecode_walker::
visit_complex_for_command (tree_complex_for_command& cmd)
{
  tree_argument_list *lhs = cmd.left_hand_side ();

  CHECK (lhs);
  CHECK (lhs->size () == 2);

  auto p = lhs->begin ();
  tree_expression *val = *p++;
  tree_expression *key = *p++;

  CHECK (val); CHECK (key);

  CHECK (val->is_identifier ());
  CHECK (key->is_identifier ());

  std::string val_name = val->name ();
  std::string key_name = key->name ();

  add_id_to_table (val_name);
  add_id_to_table (key_name);

  tree_expression *expr = cmd.control_expr ();
  CHECK_NONNULL (expr);

  // We want the rhs expression on the stack
  INC_DEPTH ();
  PUSH_NARGOUT (1);
  expr->accept (*this);
  POP_NARGOUT ();
  DEC_DEPTH ();

  // For loops need a special unwind entry to destroy the
  // native ints on the stack properly.
  int unwind_idx = N_UNWIND ();
  PUSH_UNWIND();
  UNWIND (unwind_idx).m_ip_start = CODE_SIZE ();

  UNWIND (unwind_idx).m_unwind_entry_type =
    unwind_entry_type::FOR_LOOP;

  // For loops add two native ints and one ov to the stack,
  // and switches add one ov to the stack, so we need to
  // record how many things we have added to the stack,
  // not counting this for loop. From for loops and
  // switches.
  int n_things_on_stack = n_on_stack_due_to_stmt();

  // Store added things on stack (due to for loops and switches)
  // in the unwind table.
  UNWIND (unwind_idx).m_stack_depth = n_things_on_stack;

  PUSH_CODE (INSTR::FOR_COMPLEX_SETUP);
  int need_after0 = CODE_SIZE ();
  PUSH_CODE_SHORT (-1); // Placeholder for after address for a jump if rhs is undefined

  int cond_offset = CODE_SIZE ();
  PUSH_CODE (INSTR::FOR_COMPLEX_COND);
  int need_after1 = CODE_SIZE ();
  PUSH_CODE_SHORT (-1); // Placeholder for after address
  PUSH_WSLOT (SLOT (key_name));
  PUSH_WSLOT (SLOT (val_name));

  // Walk body
  tree_statement_list *list = cmd.body ();
    // The body can be empty
  if (list)
    {
      m_n_nested_loops++;
      PUSH_NESTING_STATEMENT (nesting_statement::FOR_LOOP);
      PUSH_BREAKS ();
      PUSH_CONTINUE_TARGET ();
      list->accept (*this);
      for (int offset : POP_CONTINUE_TARGET())
        SET_CODE_SHORT (offset, cond_offset);
      POP_NESTING_STATEMENT ();
      m_n_nested_loops--;
    }

  // Jump to condition block, TODO: unless all paths terminated
  PUSH_CODE (INSTR::JMP);
  PUSH_CODE_SHORT (cond_offset);

  // Now we can set the after jump in cond and setup
  SET_CODE_SHORT (need_after0, CODE_SIZE ());
  SET_CODE_SHORT (need_after1, CODE_SIZE ());

  if (list)
    {
      // Also all breaks jump to here
      for (int need_break : POP_BREAKS ())
        {
          SET_CODE_SHORT (need_break, CODE_SIZE ());
        }
    }

  // Mark an end to the special for loop unwind entry
  UNWIND (unwind_idx).m_ip_end = CODE_SIZE ();

  // We need to pop the counter, n and rhs struct
  PUSH_CODE (INSTR::POP_N_INTS);
  PUSH_CODE (2);
  // Pop the rhs ov (the struct)
  PUSH_CODE (INSTR::POP);
}

void
bytecode_walker::
visit_fcn_handle (tree_fcn_handle &handle)
{
  INC_DEPTH ();
  std::string name = handle.name ();
  // We prepend the handles with @ to not risk collisions with
  // other identifiers in the id table
  std::string aname = "@" + name;

  if (name.find ('.') != std::string::npos)
    TODO ("No support for method fcn handles yet");

  // slot for the handle function cache
  int slot = add_id_to_table(aname);

  MAYBE_PUSH_WIDE_OPEXT (slot);
  PUSH_CODE (INSTR::PUSH_FCN_HANDLE);
  PUSH_SLOT (slot);

  maybe_emit_bind_ans_and_disp (handle);

  DEC_DEPTH ();
}

void
bytecode_walker::
visit_colon_expression (tree_colon_expression& expr)
{
  INC_DEPTH ();

  tree_expression *op1 = expr.base ();

  CHECK_NONNULL (op1);
  op1->accept (*this);

  tree_expression *op2 = expr.increment ();

  if (op2)
    op2->accept (*this);

  tree_expression *op3 = expr.limit ();

  CHECK_NONNULL (op3);
  op3->accept (*this);

  // Colon expressions have some different semantics
  // in command expressions.
  if (expr.is_for_cmd_expr ())
    {
      if (op2)
        PUSH_CODE (INSTR::COLON3_CMD);
      else
        PUSH_CODE (INSTR::COLON2_CMD);
    }
  else
    {
      if (op2)
        PUSH_CODE (INSTR::COLON3);
      else
        PUSH_CODE (INSTR::COLON2);
    }

  maybe_emit_bind_ans_and_disp (expr);

  DEC_DEPTH ();
}

void
bytecode_walker::
visit_break_command (tree_break_command&)
{
  PUSH_CODE (INSTR::JMP);
  // Need to set where to jump to after we know where the loop ends
  PUSH_NEED_BREAK (CODE_SIZE ());
  PUSH_CODE_SHORT (-1); // Placeholder
}



void
bytecode_walker::
visit_continue_command (tree_continue_command&)
{
  PUSH_CODE (INSTR::JMP);
  // The address to jump to need to be set by the loop
  // visitor (do until jumps forward), så push the code
  // address that need a target address.
  PUSH_NEED_CONTINUE_TARGET (CODE_SIZE ());
  PUSH_CODE_SHORT (-1); // Placeholder
}
