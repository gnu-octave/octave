////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023-2024 The Octave Project Developers
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

/*

  -- About the experimental VM for GNU Octave

  The VM is a "monkey tracing" stack based VM, executing linear bytecode compiled
  from the abstract syntax tree (class tree_expression etc.).

  Files of interest are:
    * pt-bytecode-walk.cc:
        The compiler translating the AST to bytecode
    * pt-bytecode-vm.cc:
        The VM
    * stack-frame.cc:
      bytecode_fcn_stack_frame, is the dynamic stack frame

  -- Stack
    The VM has one stack where it put arguments, returns, locals and temporaries.
    The stack elements have the type 'union stack_element', which is a union of
    octave_value and some pointers and native long long, double etc.

    I.e. octave_value:s are constructed inplace on the stack. Not all stack elements
    are octave_value:s.

    Nested calls to compiled bytecode functions use the same stack.

    The stack area does not grow. If the stack space runs out, the execution aborts.

    To access arguments, returns and locals their "slot number" is used. I.e. offset
    from the base stack register.

    At VM termination, the end and start of the stack is checked for magic numbers
    that should be there, and aborts if they are changed.

  -- Registers
    The VM uses the following pseudo-register:
      * instruction pointer ('ip')
      * base instruction register ('code')
      * stack register ('sp')
      * base stack register ('bsp')
          The start of the current stack frame
      * constant base register ('data')
          A pointer to an array of octave_value "literal constants", like "3"

    The registers are popped and pushed in each return or call together with the
    follwing auxilliary data:
      * unwind data
      * name data (names of the identifiers)
      * nargout
      * nargin

    Note that 'argnames' is lazy in the VM. There is more kludge state stored in
    the VM object other then the happy path state noted above.

  -- Dynamic stack frame
    The VM uses its own stack frames, but also pushes a 'stack_frame' of the subclass
    'bytecode_fcn_stack_frame' to the 'tree_evaluator', to be able to cooperate
    with C++-compiled functions and the 'tree_evaluator'.

    'bytecode_fcn_stack_frame' is quite lazy and lets e.g. a compiled function
    or user code executed with the 'tree_evaluator' create, read and write variables
    on the VM stack.

  -- Monkey tracing
    During execution of some op-codes the VM checks the type of the operands
    and might modify the bytecode to execute a specialized op-code.

    E.g. the "index identifier"-opcode becomes the "index matrix with one scalar"-opcode
    if the index is one double and the object to index is a matrix.

    If later the preconditions are not true, the specialized opcode replaces itself
    with the general opcode.

    "Monkey tracing" is a made up term for this concept.

  -- Function caching
    Function lookups are cached in the corrensponding slot of an identifier on
    the VM stack. If any function is added to the symbol table, the current
    directory is changed or 'clear' is called, all function caches are invalidated.

    The function cache is dependent on the argument types. If the argument types
    change, the cache is invalidated.

    Binary and unary operators for doubles are looked up on VM start and cached in
    the VM. They are not invalidated aslong the VM is running.

  -- Compilation
    At runtime when user code is about to be executed, it is compiled, if VM
    evaluation is turned on.

    It is also possible to compile ahead of time.

    Compiled code is reused the next invocation of the user function. If the
    user function is changed, the compiled code is cleared.

    Compilation is done be the 'bytecode_walker' class in 'pt-bytecode-walk.cc'.

  -- Opcodes

  The op-codes are byte aligned and some are variable length.

  The first byte always identifies the op-code and is also the offset used in the
  dispatch table 'instr'.

  'octave_value' is abbreviated 'ov' in this table.
  "<-" means, state of stack before operation. The right most element is the toppiest of the stack.
  "->" means, state of stack after operation.

    ** Binary math operations
      Pop two 'ov:s' of the stack and do the appropiate operation, then push the
      resulting 'ov'. The top of the stack is the right hand side.

      For all:
      <- (ov lhs) (ov rhs)
      -> (ov ans)

      -- MUL DIV ADD SUB POW LDIV EL_MUL EL_DIV EL_POW EL_LDIV
          *   /   +   -   ^   \     .*     ./     .^     .\

      The following specializations for double arguments exist:
        MUL_DBL, ADD_DBL, SUB_DBL, DIV_DBL, POW_DBL

    ** Compound math operations
      Pop two 'ov:s' off the stack and do the appropiate operation, then push the
      resulting 'ov'. The top of the stack is the right hand side.

      The opcodes are combinations of an unary math operation and a binary.

      <- (ov lhs) (ov rhs)
      -> (ov ans)

      -- TRANS_MUL  MUL_TRANS  HERM_MUL  MUL_HERM  TRANS_LDIV HERM_LDIV
          a.'*b      a*b.'      a'*b      a*b'      a.'\b      a'\b

    ** Unary math operations
      Pop one 'ov' and do the appropiate operation, then push the
      resulting 'ov'.

      <- (ov arg)
      -> (ov ans)

      -- UADD
        Unary addition. Note that unary plus is not a nop in GNU Octave. It
        can be overloaded to do whatever.
      -- USUB
        Unary subtraction
      -- TRANS
        Transpose, ".'"
      -- HERM
        Hermitian, "'"

    ** Logical unary operations
      Pop one 'ov' and do the appropiate operation, then push the
      resulting 'ov'.

      <- (ov arg)
      -> (ov ans)

      -- NOT
        "!", "~"

      -- UNARY_TRUE
        Converts an ov on the stack to either ov false or ov true.
        The op-code is used to construct control flow for e.g. shortcircuits.
        User values' truthness are checked by JMP_IF and JMP_IFN which errors
        on undefined values.

    ** Logical binary operations
      Pop two 'ov:s' of the stack and do the appropiate operation, then push the
      resulting 'ov'. The top of the stack is the right hand side.

      For all:
      <- (ov lhs) (ov rhs)
      -> (ov ans)

      -- LE  GR  EQ  NEQ  GR_EQ  LE_EQ EL_AND EL_OR
         <   >   ==  !=   >=     <=     &      |

      Note that EL_AND and EL_OR does not emulate braindamaged if-conditions. That
      is done by the bytecode compiler with the help of the opcodes
      BRAINDEAD_PRECONDITION and BRAINDEAD_WARNING together with some convoluted
      bytecode.

      The following specializations exist:
        LE_DBL, LE_EQ_DBL, GR_DBL, GR_EQ_DBL, EQ_DBL, NEQ_DBL

    ** Stack control
      -- POP
      <- (ov)
      ->
        Pop one 'ov' element of the stack.

      -- DUP
      <- (ov1)
      -> (ov1) (ov1)

        Duplicate the 'ov' on top of the stack and push it to the stack.

      -- ROT
      <- (ov1) (ov2)
      -> (ov2) (ov1)
        Rotate the top two 'ov:s' on the stack.

      -- DUPN (uint8 offset) (uint8 n)
      <- (ov -offset - n) ... (ov -offset) ... (offset amount of stack elements)
      -> The range "(ov -offset - n) ... (ov -offset)" copied to the top of the stack in the same order.
        Pushes 'n' ov:s from the stack at depth 'offset' to the top of the
        stack. The copies have the same order on the stack as the originals.
        An 'offset' of 0 means the top element of the stack.

      -- PUSH_SLOT_NARGOUT0 (uint8 slot)
      -- PUSH_SLOT_NARGOUT1 (uint8 slot)
      -- PUSH_SLOT_NARGOUTN (uint8 slot) (uint8 nargout)
      <-
      -> (ov 1) (ov 2)? ... (ov n)?
        If the local 'ov' at 'bsp[slot]' is an ordinary variable, push it
        to the stack.

        If the local is undefined, assume it is a command call function,
        look the function name up, and call it with the nargout 0, 1 or n.

        If the local is a function object, call it with the nargout 0, 1 or n.

        PUSH_SLOT_NARGOUT1_SPECIAL is like PUSH_SLOT_NARGOUT1 but pushes 'classdef_metas'
        instead of trying to execute them. PUSH_SLOT_DISP keeps track of whether the
        slot variable was executed or not for a correct display call.

      -- PUSH_SLOT_INDEXED (uint8 slot)
      <-
      -> (ov)
        Push the local 'ov' at 'bsp[slot]' to the stack. This opcode is used for
        e.g. pushing 'x' in "x(2)".

      -- PUSH_PI (uint8 slot)
      <-
      -> (ov 1) (ov 2)? ... (ov n)?
        Like PUSH_SLOT_NARGOUT1, but if the slot variable resolves to a call to
        the builtin function 'pi', just push pi to the stack as a double ov.

      -- PUSH_OV_U64
      <-
      -> (ov1)
        Push an ov of the type uint64 with the value 0, to the stack.

      -- PUSH_CELL
      <- (ov i) (ov j) ... [a mess of ov:s]
      -> (ov ans)
        Create a cell ov on the stack with up to i*j ov objects in it. Note that the last row can
        shorter than the other rows and that any row can be empty and ignored.

        Each row is initially pushed as following:
          1. element by element, if any
          2. an integer ov with the row length

      -- PUSH_NIL
      <-
      -> (ov nil)
        Push a default constructed 'octave_value' to the stack.

      -- POP_N_INTS (uint8 n)
      <- (int i1) ... (int in)
      ->
        Pops 'n' native values of the stack. It could be pointers or doubles, not just int:s.

    ** Data control
      -- LOAD_CST (uint8 offset)
      <-
      -> (ov)
        Load the 'ov' at 'data[offset]' and push it to the stack.
        LOAD_CST_ALTx are duplicates of the opcode existing for branch prediction reasons.

      -- LOAD_FAR_CST (int32 offset)
      <-
      -> (ov)
        Load the 'ov' at 'data[offset]' and push it to the stack.

      -- INIT_GLOBAL (uint8 type) (uint8 slot) (uint8 unused) (bool has_init_code) (uint16 target)?
        Initializes a persistent or global variable depending on 'type'.
        If 'has_init_code' is true, jumps to 'target' if the variable does not exist yet
        in the global namespace. If 'has_init_code' is false, it is the end of the instruction.

    ** Flow control
      -- JMP (uint16 target)
        Set the intstruction register to the instruction base register plus target.

      -- JMP_IF (uint16 target)
      -- JMP_IFN (uint16 target)
      <- (ov)
      ->
        Set the intstruction register to the instruction base register plus target
        if the argument is true/untrue.

      -- RET
      <- [saved caller frame] (int nargout) (ov ret1) ... (ov retn) (ov arg1) ... (ov argn) (ov local1) ... (ov localn)
      -> (ov retn) ... (ov ret1)
        Return from a bytecode frame to another.

        There is always atleast one ov on the stack after RET is executed. It might be the nil ov.

      -- FOR_SETUP FOR_COND (uint16 after_target) (uint8 slot)
      <- (ov range1)
      -> (ov range1) (int64 n) (int64 i)
        Executes a for-loop setup. Then falls through to the FOR_COND op-code which checks
        if a loop body is to be executed.

        FOR_SETUP is always followed by a FOR_COND opcode.

        The 'slot' is the slot for the iteration variable.

        The 'after_target' is the instruction offset to after the loop body.

        The end of the loop body jumps to the FOR_COND op-code.

        After the loop body, and at each escape point in the body,
        the two native integers and the ov range are popped.

        FOR_COMPLEX_SETUP and FOR_COMPLEX_COND is similar for "struct key-value for-loops"
        but needs two slots.

      -- THROW_IFERROBJ
      <- (ov)
      ->
        Unwinds the stack until any exception handler if ov is an error object.


      ... there are more op-codes.
*/




#if ! defined (octave_pt_bytecode_vm_h)
#define octave_pt_bytecode_vm_h 1

#include "octave-config.h"

#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

#include "octave-config.h"

#include <cstddef>
#include <cstdint>
#include <vector>
#include <memory>

#include "oct-lvalue.h"
#include "ovl.h"

#include "interpreter-private.h"
#include "symtab.h"

#include "pt-bytecode.h"

#if defined(__FILE_NAME__)
#define CHECK_PANIC(cond) \
do { if (!(cond)) panic ("VM internal error at %s:%d, " #cond, __FILE_NAME__, __LINE__);} while ((0))
#else
#define CHECK_PANIC(cond) \
do { if (!(cond)) panic ("VM internal error at %d, " #cond, __LINE__);} while ((0))
#endif


OCTAVE_BEGIN_NAMESPACE(octave)

class tree_evaluator;

struct vm_profiler
{
  struct vm_profiler_call
  {
    std::string m_caller;
    std::string m_callee;
    int64_t m_entry_time;
    int64_t m_t_self_cum; // Time spent in callee it-self
    int64_t m_t_call_cum; // Time spent in bytecode calls, called from callee
  };

  struct vm_profiler_fn_stats
  {
    // Cumulative ns time at op-code at offset
    std::vector<int64_t> m_v_cum_t;
    // Cumulative hits at op-code at offset
    std::vector<int64_t> m_v_n_cum;
    // Cumulative time spent in nested calls to a bytecode function at op-code at offset
    std::vector<int64_t> m_v_cum_call_t;

    void maybe_resize (unsigned ip)
    {
      if (ip >= m_v_cum_t.size ())
        m_v_cum_t.resize (ip + 1);
      if (ip >= m_v_n_cum.size ())
        m_v_n_cum.resize (ip + 1);
      if (ip >= m_v_cum_call_t.size ())
        m_v_cum_call_t.resize (ip + 1);
    }

    // The last bytecode timestamp, i.e. the start of the currently running opcode. One level per call
    std::vector<int64_t> m_v_t;
    // The last ip, i.e. the ip being executed. One level per call
    std::vector<int> m_v_ip;
    // Set of callers. One entry for each caller
    std::set<std::string> m_set_callers;
    // Amount of calls to this function
    int64_t m_n_calls;

    // Data structures to keep track of calls. One level per call
    std::vector<std::string> m_v_callers; // Used in callee to change the last timestamp of caller

    std::string m_fn_name;
    std::string m_fn_file;
    std::vector<unsigned char> m_code; // Copy of the actual opcodes executed
    std::vector<std::string> m_ids; // Copy of the name data
    std::vector<loc_entry> m_loc_entries; // Copy of source code location data

    void add_t (int64_t dt);
  };

  void add_t (int64_t dt);

  std::vector<vm_profiler_call> m_shadow_call_stack;

  std::map<std::string, vm_profiler_fn_stats> m_map_fn_stats;

  std::vector<std::string> m_fn_first_call_order;

  static int64_t unow ();
  void print_to_stdout ();
  void enter_fn (std::string callee_name, std::string caller_name, octave::unwind_data *unwind_data, std::string *name_data, unsigned char *code);
  void enter_fn (std::string caller_name, bytecode &bc);
  void exit_fn (std::string fn);
  void purge_shadow_stack ();
};

class vm
{
 public:

  static constexpr size_t stack_size = 2048 * 8;
  static constexpr size_t stack_pad = 32;

#if SIZE_MAX == 0xFFFFFFFF
  static constexpr size_t stack_magic_int = 0xBABEBEEF; // 32bit systems
#else
  static constexpr size_t stack_magic_int = 0xBABEBEEFCAFE1234;
#endif
  static constexpr size_t stack_min_for_new_call = 1024;

  vm (tree_evaluator *tw, bytecode &initial_bytecode);

  ~vm ();

  bool m_dbg_proper_return = false;
  bool m_could_not_push_frame = false;
  bool m_unwinding_interrupt = false;
  stack_element *m_stack0 = nullptr;

  std::vector<std::shared_ptr<stack_frame>> m_frame_ptr_cache;

  tree_evaluator *m_tw;
  type_info *m_ti;
  symbol_table *m_symtab;
  stack_element *m_stack = nullptr;
  stack_element *m_sp = 0;
  stack_element *m_bsp = 0;
  stack_element *m_rsp = 0;

  type_info::binary_op_fcn m_fn_dbl_mul = nullptr;
  type_info::binary_op_fcn m_fn_dbl_add = nullptr;
  type_info::binary_op_fcn m_fn_dbl_sub = nullptr;
  type_info::binary_op_fcn m_fn_dbl_div = nullptr;
  type_info::binary_op_fcn m_fn_dbl_pow = nullptr;
  type_info::binary_op_fcn m_fn_dbl_le = nullptr;
  type_info::binary_op_fcn m_fn_dbl_le_eq = nullptr;
  type_info::binary_op_fcn m_fn_dbl_gr = nullptr;
  type_info::binary_op_fcn m_fn_dbl_gr_eq = nullptr;
  type_info::binary_op_fcn m_fn_dbl_eq = nullptr;
  type_info::binary_op_fcn m_fn_dbl_neq = nullptr;

  type_info::unary_op_fcn m_fn_dbl_usub = nullptr;
  type_info::unary_op_fcn m_fn_dbl_not = nullptr;
  type_info::unary_op_fcn m_fn_bool_not = nullptr;

  octave_function * m_pi_builtin_fn = nullptr;
  octave_function * m_i_builtin_fn = nullptr;
  octave_function * m_e_builtin_fn = nullptr;

  static int constexpr m_scalar_typeid = 2;
  static int constexpr m_matrix_typeid = 4;
  static int constexpr m_bool_typeid = 10;
  static int constexpr m_cslist_typeid = 36;

  // If there are any ignored outputs, e.g. "[x, ~] = foo ()", we need to push a separate
  // stack frame with the ignored outputs for isargout () to be able to querry for ignored
  // outputs in the callees.
  //
  //
  struct output_ignore_data {
    octave_value m_ov_pending_ignore_matrix;
    std::vector<const std::list<octave::octave_lvalue>*> m_v_lvalue_list;
    std::vector<bool> m_v_owns_lvalue_list; // If true, should call delete on active lvalue list
    // A sanity check flag. Set to true if the first ignorer is calling from outside the VM
    bool m_external_root_ignorer = false;

    output_ignore_data ()
    {
      m_v_lvalue_list.push_back (nullptr);
      m_v_owns_lvalue_list.push_back (false);
    }

    static void maybe_delete_ignore_data (vm &vm, unsigned target_depth)
    {
      if (!vm.m_output_ignore_data)
        return;
      if (vm.m_output_ignore_data->m_v_owns_lvalue_list.size () > target_depth)
        return;

      delete vm.m_output_ignore_data;
      vm.m_output_ignore_data = nullptr;
    }

    void push_frame (vm &vm);
    void pop_frame (vm &vm);
    void clear_ignore (vm &vm);
    void set_ignore (vm &vm, octave_value ignore_matrix,
                     std::list<octave_lvalue> *new_lval_list);

    void set_ignore_anon (vm &vm, octave_value ignore_matrix);

    octave_value get_and_null_ignore_matrix ()
    {
      octave_value ret = m_ov_pending_ignore_matrix;
      m_ov_pending_ignore_matrix = {};

      return ret;
    }

    octave_value get_ignore_matrix ()
    {
      return m_ov_pending_ignore_matrix;
    }

    const std::list<octave::octave_lvalue>* pop_lvalue_list ()
    {
      auto *p = m_v_lvalue_list.back ();
      m_v_lvalue_list.pop_back ();
      return p;
    }
  };

  output_ignore_data *m_output_ignore_data = nullptr;
  const std::list<octave::octave_lvalue> *m_original_lvalue_list = nullptr;

  unsigned char *m_code;
  octave_value *m_data;
  std::string *m_name_data;
  unwind_data *m_unwind_data;

  bool m_echo_prior_op_was_cond = false;

  int m_ip;

  // Generic data container to recreate exceptions
  struct error_data
  {
    // Execution exception
    int m_exit_status;
    bool m_safe_to_return;
    // Debug quit
    bool m_debug_quit_all;
  };

  error_data
  handle_error (error_type et);

  static
  loc_entry find_loc (int ip, std::vector<octave::loc_entry> &loc_entries);

  // Disable some optimizations in GCC that are not suitable for dynamic label dispatch
#if defined (__has_attribute) && __has_attribute (optimize)
#  define OCTAVE_VM_EXECUTE_ATTR __attribute__ ((optimize("no-gcse","no-crossjumping")))
#else
#  define OCTAVE_VM_EXECUTE_ATTR
#endif

  // Returns true if the VM should be used to call the function
  static bool maybe_compile_or_compiled (octave_user_code *fn, stack_frame::local_vars_map *locals = nullptr);

  // Allocate a VM and call the function
  static octave_value_list call (tree_evaluator& tw,
                                 int nargout,
                                 const octave_value_list& args,
                                 octave_user_code *fn,
                                 std::shared_ptr<stack_frame> context = nullptr);

  octave_value_list execute_code (const octave_value_list &args, int root_nargout) OCTAVE_VM_EXECUTE_ATTR;

  octave_value find_fcn_for_cmd_call (std::string *name);
  octave_value handle_object_end (octave_value ov, int idx, int nargs);

  void set_nargin (int nargin);

  void set_nargout (int nargout);

  void caller_ignores_output ();

  unwind_entry* find_unwind_entry_for_current_state (bool only_find_unwind_protect);
  int find_unwind_entry_for_forloop (int current_stack_depth);

  static std::shared_ptr<vm_profiler> m_vm_profiler;
  static bool m_profiler_enabled;
  static bool m_trace_enabled;
};

OCTINTERP_API
void print_bytecode (bytecode &bc);

OCTINTERP_API
std::vector<std::pair<int, std::string>>
opcodes_to_strings (bytecode &bc);

OCTINTERP_API
std::vector<std::pair<int, std::string>>
opcodes_to_strings (std::vector<unsigned char> &code, std::vector<std::string> &names);

OCTAVE_END_NAMESPACE(octave)

#endif

#endif
