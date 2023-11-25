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

#if ! defined (octave_pt_bytecode_vm_internal_h)
#define octave_pt_bytecode_vm_internal_h 1

#include "octave-config.h"

#define EXPAND_CSLIST_PUSH_N_OVL_ELEMENTS_TO_STACK(ovl,nargout) \
do {\
  if (nargout <= 1)\
    PUSH_OV (ovl.first_or_nil_ov());\
else /* TODO: Should be function call to keep code shorter. */\
  {\
    int actual_nargout = 0;\
\
    int n_retval = std::min (static_cast<int> (ovl.length ()), static_cast<int> (nargout));\
    /* We want to push the ovl backwards */\
    for (int ii = n_retval - 1; ii >= 0 && actual_nargout < nargout; ii--)\
    {\
      octave_value &arg = ovl (ii);\
\
      if (arg.is_cs_list ())\
        {\
          /* cs-list are also pushed backwards */\
          octave_value_list args = arg.list_value ();\
          /* We might need to skip the elements in the cs-list's end */ \
          int n_left = nargout - actual_nargout;\
          for (int j = std::min (static_cast<int> (args.length () - 1), n_left - 1);\
                j >= 0 && actual_nargout < nargout; \
                j--)\
            {\
              PUSH_OV (args (j));\
              actual_nargout++;\
            }\
        }\
      else\
        {\
          PUSH_OV (std::move (arg));\
          actual_nargout++;\
        }\
    }\
\
    /* TODO: Need errors here for missing arguments in assignment somehow */ \
    if (actual_nargout != nargout)\
      {\
        int diff = nargout - actual_nargout;\
        stack_element *start = sp - actual_nargout;\
        stack_lift (start, actual_nargout, diff);\
        sp += diff;\
      }\
  }\
} while (0)


#define MAKE_BINOP(op) \
{                                       \
  octave_value &rhs = TOP_OV ();        \
  octave_value &lhs = SEC_OV ();        \
                                        \
  try                                   \
    {                                   \
      octave_value ans =                \
        binary_op (*m_ti,               \
                    octave_value::op,   \
                    lhs, rhs);          \
      STACK_DESTROY (2);                \
      PUSH_OV (std::move (ans));        \
    }                                   \
  CATCH_INTERRUPT_EXCEPTION             \
  CATCH_INDEX_EXCEPTION                 \
  CATCH_EXECUTION_EXCEPTION             \
  CATCH_BAD_ALLOC                       \
}                                       \

#define MAKE_BINOP_SPECIALIZED(op_fn,jmp_target,op_target,target_type) \
{                                                                                        \
  octave_value &rhs = TOP_OV ();                                                         \
  octave_value_vm &lhs = SEC_OV_VM ();                                                   \
                                                                                         \
  int rhs_type = rhs.type_id ();                                                         \
  int lhs_type = lhs.type_id ();                                                         \
  int t_type = target_type;                                                              \
  if (OCTAVE_UNLIKELY (rhs_type != lhs_type || rhs_type != t_type))                      \
    {                                                                                    \
      ip[-2] = static_cast<unsigned char> (INSTR::op_target);                            \
      goto jmp_target;                                                                   \
    }                                                                                    \
                                                                                         \
  try                                                                                    \
    {                                                                                    \
      lhs = op_fn (lhs.get_rep (), rhs.get_rep ());                                      \
      rhs.~octave_value ();                                                              \
      STACK_SHRINK (1);                                                                  \
    }                                                                                    \
  CATCH_INTERRUPT_EXCEPTION                                                              \
  CATCH_INDEX_EXCEPTION                                                                  \
  CATCH_EXECUTION_EXCEPTION                                                              \
  CATCH_BAD_ALLOC                                                                        \
}                                                                                        \

#define MAKE_BINOP_CST_SPECIALIZED(op_fn,jmp_target,op_target,target_type) \
{                                                                              \
  octave_value &cst = data [arg0];                                             \
  octave_value &arg = TOP_OV ();                                               \
  int lhs_is_cst = *ip++;                                                      \
                                                                               \
  int arg_type = arg.type_id ();                                               \
  if (OCTAVE_UNLIKELY (arg_type != target_type))                               \
    {                                                                          \
      ip[-3] = static_cast<unsigned char> (INSTR::op_target);                  \
      ip--;                                                                    \
      goto jmp_target;                                                         \
    }                                                                          \
                                                                               \
  try                                                                          \
    {                                                                          \
      octave_value ret =  lhs_is_cst ?                                         \
        op_fn (cst.get_rep (), arg.get_rep ()) :                               \
        op_fn (arg.get_rep (), cst.get_rep ());                                \
      STACK_DESTROY (1);                                                       \
      PUSH_OV (ret);                                                           \
    }                                                                          \
  CATCH_INTERRUPT_EXCEPTION                                                    \
  CATCH_INDEX_EXCEPTION                                                        \
  CATCH_EXECUTION_EXCEPTION                                                    \
  CATCH_BAD_ALLOC                                                              \
}                                                                              \

#define MAKE_UNOP_SPECIALIZED(op_fn, jmp_target, op_target, target_type) \
{                                                                                        \
  octave_value &ov = TOP_OV ();                                                          \
                                                                                         \
  if (OCTAVE_UNLIKELY (ov.type_id () != target_type))                                    \
    {                                                                                    \
      /* Change the specialized opcode to the generic one */                             \
      ip[-2] = static_cast<unsigned char> (INSTR::op_target);                            \
      goto jmp_target;                                                                   \
    }                                                                                    \
                                                                                         \
  try                                                                                    \
    {                                                                                    \
      ov = op_fn (ov.get_rep ());                                          \
    }                                                                                    \
  CATCH_INTERRUPT_EXCEPTION                                                              \
  CATCH_INDEX_EXCEPTION                                                                  \
  CATCH_EXECUTION_EXCEPTION                                                              \
  CATCH_BAD_ALLOC                                                                        \
}                                                                                        \

#define MAKE_BINOP_SELFMODIFYING(op, jmp_target, op_target) \
{                                                                                          \
  octave_value &rhs = TOP_OV ();                                                           \
  octave_value &lhs = SEC_OV ();                                                           \
                                                                                           \
  int rhs_type = rhs.type_id ();                                                           \
  int lhs_type = lhs.type_id ();                                                           \
  if (rhs_type == lhs_type && rhs_type == m_scalar_typeid)                                 \
    {                                                                                      \
      ip[-2] = static_cast<unsigned char> (INSTR::op_target);                              \
      goto jmp_target;                                                                     \
    }                                                                                      \
                                                                                           \
  try                                                                                      \
    {                                                                                      \
      octave_value ans =                                                                   \
        binary_op (*m_ti,                                                                  \
                    octave_value::op,                                                      \
                    lhs, rhs);                                                             \
      STACK_DESTROY (2);                                                                   \
      PUSH_OV (std::move (ans));                                                           \
    }                                                                                      \
  CATCH_INTERRUPT_EXCEPTION                                                                \
  CATCH_INDEX_EXCEPTION                                                                    \
  CATCH_EXECUTION_EXCEPTION                                                                \
  CATCH_BAD_ALLOC                                                                          \
}

#define MAKE_BINOP_CST_SELFMODIFYING(op, jmp_target, op_target) \
{                                                                                             \
  octave_value &cst = data [arg0];                                                            \
  octave_value &arg = TOP_OV ();                                                              \
  int lhs_is_cst = *ip++;                                                                     \
                                                                                              \
  int cst_type = cst.type_id ();                                                              \
  int arg_type = arg.type_id ();                                                              \
  if (OCTAVE_UNLIKELY (cst_type == arg_type && cst_type == m_scalar_typeid))                  \
    {                                                                                         \
      ip[-3] = static_cast<unsigned char> (INSTR::op_target);                                 \
      ip--;                                                                                   \
      goto jmp_target;                                                                        \
    }                                                                                         \
                                                                                              \
  try                                                                                         \
    {                                                                                         \
      octave_value ans = lhs_is_cst ?                                                         \
          binary_op (*m_ti,                                                                   \
                      octave_value::op,                                                       \
                      cst, arg)                                                               \
        : binary_op (*m_ti,                                                                   \
                      octave_value::op,                                                       \
                      arg, cst);                                                              \
      STACK_DESTROY (1);                                                                      \
      PUSH_OV (std::move (ans));                                                              \
    }                                                                                         \
  CATCH_INTERRUPT_EXCEPTION                                                                   \
  CATCH_INDEX_EXCEPTION                                                                       \
  CATCH_EXECUTION_EXCEPTION                                                                   \
  CATCH_BAD_ALLOC                                                                             \
}

#define CATCH_INDEX_EXCEPTION \
catch (index_exception& ie)                              \
{                                                        \
  (*sp++).pee = ie.dup ();                               \
  (*sp++).i = static_cast<int> (error_type::INDEX_ERROR);\
  goto unwind;                                           \
}                                                        \

#define CATCH_INDEX_EXCEPTION_WITH_NAME \
catch (index_exception& ie)                              \
{                                                        \
  ie.set_var (name_data [slot]);                         \
  (*sp++).pee = ie.dup ();                               \
  (*sp++).i = static_cast<int> (error_type::INDEX_ERROR);\
  goto unwind;                                           \
}                                                        \

#define CATCH_INDEX_EXCEPTION_WITH_MAYBE_NAME(has_name)  \
catch (index_exception& ie)                              \
{                                                        \
  if (has_name)                                          \
    ie.set_var (name_data [slot]);                       \
  (*sp++).pee = ie.dup ();                               \
  (*sp++).i = static_cast<int> (error_type::INDEX_ERROR);\
  goto unwind;                                           \
}                                                        \

#define CATCH_INTERRUPT_EXCEPTION \
catch (interrupt_exception& e)                                          \
  {                                                                     \
    (*sp++).i = static_cast<int> (error_type::INTERRUPT_EXC);           \
    goto unwind;                                                        \
  }                                                                     \

#define CATCH_EXECUTION_EXCEPTION \
catch (execution_exception& e)                                          \
  {                                                                     \
    /* TODO: Id? */                                                     \
    (*sp++).pee = new execution_exception {e};                          \
    (*sp++).i = static_cast<int> (error_type::EXECUTION_EXC);           \
                                                                        \
    goto unwind;                                                        \
  }                                                                     \

#define CATCH_STACKPUSH_EXECUTION_EXCEPTION \
catch (execution_exception& e)                                          \
  {                                                                     \
    m_could_not_push_frame = true;                                      \
    (*sp++).pee = new execution_exception {e};                          \
    (*sp++).i = static_cast<int> (error_type::EXECUTION_EXC);           \
                                                                        \
    goto unwind;                                                        \
  }                                                                     \

#define CATCH_STACKPUSH_BAD_ALLOC \
catch (const std::bad_alloc&)                                           \
{                                                                       \
  m_could_not_push_frame = true;                                        \
  (*sp++).i = static_cast<int> (error_type::BAD_ALLOC);                 \
                                                                        \
  goto unwind;                                                          \
}

#define CATCH_EXIT_EXCEPTION \
catch (const exit_exception& e)                                         \
{                                                                       \
  (*sp++).i = e.exit_status ();                                         \
  (*sp++).i = e.safe_to_return ();                                      \
  (*sp++).i = static_cast<int> (error_type::EXIT_EXCEPTION);            \
                                                                        \
  goto unwind;                                                          \
}

#define CATCH_BAD_ALLOC \
catch (const std::bad_alloc&)                                           \
{                                                                       \
  (*sp++).i = static_cast<int> (error_type::BAD_ALLOC);                 \
                                                                        \
  goto unwind;                                                          \
}

#define MAKE_BYTECODE_CALL \
if (sp + stack_min_for_new_call >= m_stack + stack_size)                                          \
  {                                                                                               \
    (*sp++).pee = new execution_exception {"error","","VM is running out of stack space"};        \
    (*sp++).i = static_cast<int> (error_type::EXECUTION_EXC);                                     \
    goto unwind;                                                                                  \
  }                                                                                               \
/* We are now going to call another function */                                                   \
/* compiled to bytecode */                                                                        \
                                                                                                  \
m_tw->set_active_bytecode_ip (ip - code);                                                         \
stack_element *first_arg = sp - n_args_on_stack;                                                  \
                                                                                                  \
/* Push address to first arg (or would one would have been */                                     \
/* if there are no args), so we can restore the sp at return */                                   \
(*sp++).pse = first_arg;                                                                          \
                                                                                                  \
/* Push unwind data */                                                                            \
(*sp++).pud = unwind_data;                                                                        \
                                                                                                  \
/* Push code */                                                                                   \
(*sp++).puc = code;                                                                               \
                                                                                                  \
/* Push data */                                                                                   \
(*sp++).pov = data;                                                                               \
                                                                                                  \
/* Push id names */                                                                               \
(*sp++).ps = name_data;                                                                           \
                                                                                                  \
/* Push bsp */                                                                                    \
(*sp++).pse = bsp;                                                                                \
                                                                                                  \
/* Push the instruction pointer */                                                                \
(*sp++).puc = ip;                                                                                 \
                                                                                                  \
/* The amount of return values the caller actually wants. Not necesserely the */                  \
/* same as the amount of return values the caller wants the callee to produce. */                 \
/* (last on caller stack) */                                                                      \
(*sp++).u = caller_nvalback;                                                                      \
                                                                                                  \
/* set callee bsp */                                                                              \
m_sp = bsp = sp;                                                                                  \
                                                                                                  \
/* Push nargout (first on callee stack) */                                                        \
(*sp++).u = nargout;                                                                              \
                                                                                                  \
/* Set the new data, code etc */                                                                  \
bytecode &bc = usr_fcn->get_bytecode ();                                                          \
if (OCTAVE_UNLIKELY (m_profiler_enabled))                                                         \
  {                                                                                               \
    auto p = vm::m_vm_profiler;                                                                   \
    if (p)                                                                                        \
      {                                                                                           \
        std::string caller_name = data[2].string_value (); /* profiler_name () querried at compile time */ \
        p->enter_fn (caller_name, bc);                                                            \
      }                                                                                           \
  }                                                                                               \
m_data = data = bc.m_data.data ();                                                                \
m_code = code = bc.m_code.data ();                                                                \
m_name_data = name_data = bc.m_ids.data ();                                                       \
m_unwind_data = unwind_data = &bc.m_unwind_data;                                                  \
                                                                                                  \
                                                                                                  \
/* Set the ip to 0 */                                                                             \
ip = code;                                                                                        \
int n_returns_callee = static_cast<signed char> (*ip++); /* Negative for varargout */             \
if (OCTAVE_UNLIKELY (n_returns_callee < 0))                                                       \
  {                                                                                               \
    if (n_returns_callee == -128) /* Anonymous function */                                        \
      n_returns_callee = 1;                                                                       \
    else                                                                                          \
      n_returns_callee = -n_returns_callee;                                                       \
  }                                                                                               \
int n_args_callee = static_cast<signed char> (*ip++); /* Negative for varargin */                 \
int n_locals_callee = POP_CODE_USHORT ();                                                         \
                                                                                                  \
if (n_args_callee < 0)                                                                            \
{                                                                                                 \
  sp[0].pv = static_cast<void*> (usr_fcn);                                                        \
  goto varargin_call;                                                                             \
}                                                                                                 \
                                                                                                  \
/* Construct return values - note nargout */                                                      \
/* is allready pushed as a uint64 */                                                              \
for (int ii = 1; ii < n_returns_callee; ii++)                                                     \
  PUSH_OV ();                                                                                     \
                                                                                                  \
int n_args_on_callee_stack = 0;                                                                   \
bool all_too_many_args = false;                                                                   \
/* Move the args to the new stack */                                                              \
for (int ii = 0; ii < n_args_on_stack; ii++)                                                      \
  {                                                                                               \
    octave_value &arg = first_arg[ii].ov;                                                         \
                                                                                                  \
    if (arg.is_cs_list ())                                                                        \
      {                                                                                           \
        octave_value_list args = arg.list_value ();                                               \
        octave_idx_type n_el = args.length ();                                                    \
        if (n_el + n_args_on_callee_stack > 512)                                                  \
          {                                                                                       \
            all_too_many_args = true;                                                             \
          }                                                                                       \
        else                                                                                      \
          {                                                                                       \
            for (int j = 0; j < n_el; j++)                                                        \
              {                                                                                   \
                PUSH_OV (args (j));                                                               \
                n_args_on_callee_stack++;                                                         \
              }                                                                                   \
          }                                                                                       \
      }                                                                                           \
    else                                                                                          \
      {                                                                                           \
        PUSH_OV (std::move (arg));                                                                \
        n_args_on_callee_stack++;                                                                 \
      }                                                                                           \
    /* Destroy the args */                                                                        \
    arg.~octave_value ();                                                                         \
  }                                                                                               \
/* Construct missing args */                                                                      \
for (int ii = n_args_on_callee_stack; ii < n_args_callee; ii++)                                   \
  PUSH_OV ();                                                                                     \
                                                                                                  \
/* Construct locals */                                                                            \
int n_locals_to_ctor =                                                                            \
  n_locals_callee - n_args_callee - n_returns_callee;                                             \
for (int ii = 0; ii < n_locals_to_ctor; ii++)                                                     \
  PUSH_OV ();                                                                                     \
                                                                                                  \
try                                                                                               \
  {                                                                                               \
    m_tw->push_stack_frame(*this, usr_fcn, nargout, n_args_on_callee_stack);                      \
  }                                                                                               \
CATCH_STACKPUSH_EXECUTION_EXCEPTION /* Sets m_could_not_push_frame to true */                     \
CATCH_STACKPUSH_BAD_ALLOC                                                                         \
                                                                                                  \
if (OCTAVE_UNLIKELY (m_output_ignore_data))                                                       \
  {                                                                                               \
    /* Called fn needs to know about ignored outputs .e.g. [~, a] = foo() */                      \
    m_output_ignore_data->push_frame (*this);                                                     \
  }                                                                                               \
                                                                                                  \
/* "auto var" in the frame object. This is needed if nargout() etc are called */                  \
set_nargout (nargout);                                                                            \
                                                                                                  \
if (all_too_many_args)                                                                            \
  {                                                                                               \
    std::string fn_name = unwind_data->m_name;                                                    \
    (*sp++).pee = new execution_exception {"error", "Octave:invalid-fun-call",                    \
                                           fn_name + ": function called with over 512 inputs."    \
                                           " Consider using varargin."};                          \
    (*sp++).i = static_cast<int> (error_type::EXECUTION_EXC);                                     \
    goto unwind;                                                                                  \
  }                                                                                               \
if (n_args_on_callee_stack > n_args_callee)                                                       \
  {                                                                                               \
    std::string fn_name = unwind_data->m_name;                                                    \
    (*sp++).pee = new execution_exception {"error", "Octave:invalid-fun-call",                    \
                                           fn_name + ": function called with too many inputs"};   \
    (*sp++).i = static_cast<int> (error_type::EXECUTION_EXC);                                     \
    goto unwind;                                                                                  \
  }                                                                                               \
/* N_RETURNS is negative for varargout */                                                         \
int n_returns = N_RETURNS () - 1; /* %nargout in N_RETURNS */                                     \
if (n_returns >= 0 && nargout > n_returns)                                                        \
  {                                                                                               \
    std::string fn_name = unwind_data->m_name;                                                    \
    (*sp++).pee = new execution_exception {"error", "Octave:invalid-fun-call",                    \
                                           fn_name + ": function called with too many outputs"};  \
    (*sp++).i = static_cast<int> (error_type::EXECUTION_EXC);                                     \
    goto unwind;                                                                                  \
  }                                                                                               \


#endif
