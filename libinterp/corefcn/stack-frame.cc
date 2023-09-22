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

#include <iostream>

#include "lo-regexp.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "oct-map.h"
#include "ov.h"
#include "ov-fcn.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pt-eval.h"
#include "stack-frame.h"
#include "syminfo.h"
#include "symrec.h"
#include "symscope.h"
#include "variables.h"
#include <array>
#include "ov-ref.h"

#include "pt-bytecode-vm.h"

OCTAVE_BEGIN_NAMESPACE(octave)


// bytecode_fcn_stack_frame is for the VM.
//
// The VM stack does not directly translate to the tree walker's stack,
// so there is a translation table to make accessing the VM stack opaque
// for non-VM use.
//
// The octave_values are not acctually stored in the bytecode_fcn_stack_frame
// object, but on the VM stack. Extra values created by eg. 'eval ("e = 3;")'
// are stored in this frame object though.
class bytecode_fcn_stack_frame : public stack_frame
{
public:

  enum class bytecode_offsets
  {
    DATA_NLOCALS = 2,
  };

  bytecode_fcn_stack_frame (void) = delete;

  bytecode_fcn_stack_frame (tree_evaluator& tw,
                            octave_user_code *fcn,
                            std::size_t index,
                            const std::shared_ptr<stack_frame>& parent_link,
                            const std::shared_ptr<stack_frame>& static_link,
                            vm &vm,
                            int nargout, int nargin)
    : stack_frame (tw, index, parent_link, static_link,
                   nullptr),
      m_fcn (fcn),
      m_unwind_data (vm.m_unwind_data),
      m_name_data (vm.m_name_data),
      m_stack_start (vm.m_sp),
      m_code (vm.m_code),
      m_size (m_unwind_data->m_ids_size),
      // The above fields in vm change during execution so we need to store them in the frame
      m_vm (&vm),
      m_nargin (nargin),
      m_nargout (nargout)
  {
    // If the function scope has more variables now due to something adding e.g.
    // a global too it after the compilation of the function was done we need
    // to resize the bytecode frame size.
    std::size_t n_syms =  fcn->scope_num_symbols ();

    m_orig_size = m_unwind_data->m_n_orig_scope_size;

    // If any symbols been added to the scope since the compilation we allocate room for
    // them on the dynamic stack frame. Unless it is a script, which should put stuff in the
    // eval frame.
    if (n_syms > m_orig_size && !fcn->is_user_script ())
      {
        int n_to_add = n_syms - m_orig_size;
        // Make room for the extra symbols
        internal_resize (internal_size () + n_to_add);
        
        // We need to get the names of the extra symbols too
        auto scope = get_scope ();
        auto &sym_map = scope.symbols ();

        for (auto &kv : sym_map)
          {
            symbol_record &sr = kv.second;
            if (sr.frame_offset ())
              continue;
            unsigned offset = sr.data_offset ();
            if (offset >= m_orig_size)
              internal_set_extra_name (external_to_local_offset (offset), sr.name ());
          }
      }
  }

  bytecode_fcn_stack_frame (const bytecode_fcn_stack_frame& elt)
    : stack_frame(elt.m_evaluator, elt.m_index, elt.m_parent_link,
                  elt.m_static_link, elt.m_access_link)
  {
    // A copy of a bytecode frame has no pointers to the actual VM stack or VM itself.
    m_nargin = elt.m_nargin;
    m_nargout = elt.m_nargout;
    m_name_data = elt.m_name_data;
    m_size = elt.m_size;
    m_orig_size = elt.m_orig_size;
    m_ip = elt.m_ip;
    m_unwind_data = elt.m_unwind_data; // TODO: Ownership?
    m_size = m_unwind_data->m_ids_size;

    if (elt.m_lazy_data)
      {
        auto &lazy = lazy_data ();

        lazy.m_extra_slots = elt.m_lazy_data->m_extra_slots;
        lazy.m_extra_names = elt.m_lazy_data->m_extra_names;
      }
  }

  bytecode_fcn_stack_frame&
  operator = (const bytecode_fcn_stack_frame& elt) = delete;

  bytecode_fcn_stack_frame&
  operator = (bytecode_fcn_stack_frame&& elt) = delete;

  // vm_clear_for_cache () and the dtor need to mirror eachother
  // so they both call dispose()
  void dispose ()
  {
    if (m_lazy_data)
      {
        if (m_lazy_data->m_stack_cpy)
          {
            // Note: int nargout at offset 0
            for (unsigned i = 1; i < m_size; i++)
              m_lazy_data->m_stack_cpy[i].ov.~octave_value ();
            delete [] m_lazy_data->m_stack_cpy;
          }
        delete m_lazy_data->m_unwind_protect_frame;
        delete m_lazy_data;
        m_lazy_data = nullptr;
      }
  }

  ~bytecode_fcn_stack_frame ()
  {
    // vm_clear_for_cache () need to mirror the dtor
    dispose ();
  }


  void local_to_external_offset (size_t local_offset, size_t &external_offset_out, size_t &frame_offset_out) const
  {
    for (size_t frame_offset = 0; frame_offset < m_unwind_data->m_external_frame_offset_to_internal.size (); frame_offset++)
      {
        for (auto it : m_unwind_data->m_external_frame_offset_to_internal[frame_offset])
          {
            size_t local_tmp = it.second;
            if (local_tmp != local_offset)
              continue;

            external_offset_out = it.first;
            frame_offset_out = frame_offset;

            return;
          }
      }

    if (local_offset < m_size)
      error ("VM internal error: Invalid internal offset. Smaller than original size and not in table");

    external_offset_out = local_offset - m_size + m_orig_size;
    frame_offset_out = 0;
  }

  std::size_t external_to_local_offset (std::size_t external_offset, std::size_t frame_offset = 0) const
  {
    auto it = m_unwind_data->m_external_frame_offset_to_internal.at (frame_offset).find (external_offset);
    if (it == m_unwind_data->m_external_frame_offset_to_internal[frame_offset].end ())
    {
      CHECK_PANIC (frame_offset == 0);

      if (external_offset < m_orig_size)
        error ("VM internal error: Invalid external offset. Smaller than original size and not in table");
      // The offsets that are not in the original translation table are in the extra slots added dynamically
      return m_size + (external_offset - m_orig_size);
    }

    return it->second;
  }

  // Returns true if the symbol with the external offset and frame offset is stored in this frame.
  // Note that this does not imply that frame offset is zero, if the return value is true.
  bool maybe_external_to_local_offset (std::size_t external_offset, std::size_t frame_offset, 
                                       std::size_t &internal_offset_out, bool &allready_added_out) const
  {
    allready_added_out = false;

    if (frame_offset >= m_unwind_data->m_external_frame_offset_to_internal.size ())
      return false;

    auto it = m_unwind_data->m_external_frame_offset_to_internal.at (frame_offset).find (external_offset);
    bool found = it != m_unwind_data->m_external_frame_offset_to_internal[frame_offset].end ();

    if (frame_offset == 0 && !found)
      {
        if (external_offset < m_orig_size)
          error ("VM internal error: Invalid external offset. Smaller than original size and not in table");
        // The offsets that are not in the original translation table are in the extra slots added dynamically
        internal_offset_out = m_size + (external_offset - m_orig_size);
        allready_added_out = internal_offset_out < internal_size ();
        return true;
      }
    else if (!found)
      return false;

    internal_offset_out = it->second;
    allready_added_out = true;

    return true;
  }

  void vm_clear_for_cache ()
  {
    m_parent_link = nullptr;
    m_static_link = nullptr;
    m_access_link = nullptr;
    m_dispatch_class.clear ();

    dispose ();
  }

  // Since a reference to the stackframe can be saved somewhere
  // we need to check at stack unwind in the VM if that is the case
  // and save the variables on the VM stack in this frame object so
  // they can be accessed.
  void vm_unwinds ()
  {
    bool is_alone = m_weak_ptr_to_self.use_count () <= 2; // Two seems about right

    if (m_lazy_data)
      {
        delete m_lazy_data->m_unwind_protect_frame;
        m_lazy_data->m_unwind_protect_frame = nullptr;

        // Restore warningstates
        if (m_fcn)
          {
            auto usr_fn_p = m_fcn->user_function_value (true);
            if (usr_fn_p)
              usr_fn_p->restore_warning_states (); // TODO: octave_user_function::restore_warning_states() could be static.
          }
      }

    if (is_alone)
      {
        if (m_lazy_data)
          delete m_lazy_data;

        // Zero these so it is easier to find a "use-after-unwind"
        // error
        m_lazy_data = nullptr;
        m_stack_start = nullptr;
        m_code = nullptr;
        m_name_data = nullptr;
        m_unwind_data = nullptr;
        m_vm = nullptr;

        return;
      }

    // These pointers might become invalid
    m_vm = nullptr;
    //m_unwind_data  = nullptr;

    // Copy the stack to the frame
    size_t stack_slots = m_size;

    lazy_data ();

    m_lazy_data->m_stack_cpy = new octave::stack_element[stack_slots];

    m_lazy_data->m_stack_cpy[0].i = m_stack_start[0].i; // Copy int nargout at offset 0
    // Copy each octave_value in slots on the stack
    for (unsigned i = 1; i < m_size; i++)
      new (&m_lazy_data->m_stack_cpy[i].ov) octave_value {m_stack_start[i].ov};

    m_stack_start = m_lazy_data->m_stack_cpy;
  }

  std::size_t size (void) const
  {
    return m_orig_size +
      (m_lazy_data ? m_lazy_data->m_extra_slots.size () : 0);
  }

  std::size_t internal_size (void) const
  {
    return m_size +
      (m_lazy_data ? m_lazy_data->m_extra_slots.size () : 0);
  }

  void resize (std::size_t arg)
  {
    int diff = static_cast<int> (arg) - static_cast<int>(size ());

    if (diff > 0)
      internal_resize (internal_size () + diff);
  }

  void internal_set_extra_name (std::size_t local_offset, std::string name)
  {
    std::size_t extra_offset = local_offset - m_size;
    std::size_t extra_size = (m_lazy_data ? m_lazy_data->m_extra_slots.size () : 0);

    if (!m_lazy_data || extra_offset >= extra_size)
      panic ("VM internal panic: Trying to set extra slot name out of range");

    m_lazy_data->m_extra_names.at (extra_offset) = name;
  }

  void internal_resize (std::size_t arg)
  {
    // Not ok to resize for scripts. Extra slots should be in the eval frame.
    CHECK_PANIC (!m_fcn->is_user_script ());

    int diff = static_cast<int> (arg) - static_cast<int>(internal_size ());

    if (diff > 0)
      {
        auto &lazy = lazy_data ();
        lazy.m_extra_slots.resize (lazy.m_extra_slots.size () + diff);

        lazy.m_extra_names.resize (lazy.m_extra_names.size () + diff);
      }
  }

  bool slot_is_global (std::size_t local_offset) const
  {
    if (local_offset >= m_size)
      {
        if (!m_lazy_data)
          panic ("bytecode_fcn_stack_frame::slot_is_global(%zu): Bad request", local_offset);

        octave_value ov = m_lazy_data->m_extra_slots.at (local_offset - m_size);
        if (!ov.is_ref ())
          return false;
        return ov.ref_rep ()->get_scope_flag () == GLOBAL;
      }

    octave_value &ov = m_stack_start [local_offset].ov;
    if (!ov.is_ref ())
      return false;

    return ov.ref_rep ()->get_scope_flag () == GLOBAL;
  }

  bool slot_is_persistent (std::size_t local_offset) const
  {
    if (local_offset >= m_size)
      {
        if (!m_lazy_data)
          panic ("bytecode_fcn_stack_frame::slot_is_global(%zu): Bad request", local_offset);
        octave_value ov = m_lazy_data->m_extra_slots.at (local_offset - m_size);
        if (!ov.is_ref ())
          return false;
        return ov.ref_rep ()->get_scope_flag () == PERSISTENT;
      }

    octave_value &ov = m_stack_start [local_offset].ov;
    if (!ov.is_ref ())
      return false;
    return ov.ref_rep ()->get_scope_flag () == PERSISTENT;
  }

  stack_frame::scope_flags get_scope_flag_internal (std::size_t external_offset, std::size_t frame_offset) const
  {
    std::size_t local_offset;
    bool allready_added;
    bool in_this_frame = maybe_external_to_local_offset (external_offset, frame_offset, local_offset, allready_added);

    // Note, quirk: A variable that is not added to a nested frame is not to be reported as global
    // eventhough it is global in the parent frame.
    if (!in_this_frame)
      {
        if (!m_fcn->is_nested_function ())
          error ("VM internal error: Invalid call to get_scope_flag_internal");

        return LOCAL;
      }

    // Is the slot on the original bytecode stack frame?
    if (local_offset < m_size)
      {
        octave_value ov = m_stack_start [local_offset].ov;

        if (!ov.is_ref ())
          return LOCAL;
        return ov.ref_rep ()->get_scope_flag ();
      }

    size_t extra_offset = local_offset - m_size;
    if (m_lazy_data && extra_offset < m_lazy_data->m_extra_slots.size ())
      {
        octave_value ov = m_lazy_data->m_extra_slots.at (extra_offset);
        if (!ov.is_ref ())
          return LOCAL;
        return ov.ref_rep ()->get_scope_flag ();
      }

    return LOCAL;
  }

  stack_frame::scope_flags get_scope_flag (std::size_t external_offset) const
  {
    return get_scope_flag_internal (external_offset, 0);
  }

  void set_scope_flag (std::size_t external_offset, scope_flags flag)
  {
    scope_flags current_flag = get_scope_flag (external_offset);

    bool is_global = current_flag == GLOBAL;
    bool is_pers = current_flag == PERSISTENT;

    std::size_t local_offset = external_to_local_offset (external_offset);

    octave_value *ov; // Pointer to the slot
    std::string name; // Used for globals

    if (local_offset >= m_size)
      {
        if (!m_lazy_data)
          error ("VM internal error: Trying to set scope flag on invalid offset");

        ov = &m_lazy_data->m_extra_slots.at (local_offset - m_size);
        name = m_lazy_data->m_extra_names.at (local_offset - m_size);
      }
    else
      {
        ov = &m_stack_start [local_offset].ov;
        name = m_name_data [local_offset];
      }

    if (flag == GLOBAL)
      {
        if (is_global)
          return;
        if (is_pers)
          error ("VM internal error: Trying to make persistent variable global");
        if (name == "")
          error ("VM internal error: Trying to make unnamed symbol global");

        if (ov->is_ref ())
          {
            octave_value_ref *r = ov->ref_rep ();
            if (r->is_local_ref ())
              r->mark_globalness_in_owning_frame (true);
            else
              *ov = octave_value {new octave_value_ref_global {name}};
          }
        else
          *ov = octave_value {new octave_value_ref_global {name}};
        return;
      }

    if (flag == PERSISTENT)
      {
        if (is_pers)
          return;
        if (is_global)
          error ("VM internal error: Trying to make global variable persistent");

        *ov = octave_value {new octave_value_ref_persistent {get_scope (), static_cast<int> (external_offset)}};

        return;
      }

    if (flag == LOCAL)
      {
        if (!is_global && !is_pers)
          return;

        // Clear the global or persistent ref on the stack
        if (is_global || is_pers)
          {
            // Clear the ref in its slot
            if (ov->is_ref ())
              {
                octave_value_ref *r = ov->ref_rep ();
                if (r->is_local_ref ())
                  r->mark_globalness_in_owning_frame (false);
                else
                  *ov = octave_value {};
              }
            else
              *ov = octave_value {};
          }

        return;
      }

    panic ("VM internal error: Strange state: %d", flag);
  }

  stack_frame::scope_flags scope_flag (const symbol_record& sym) const
  {
    std::size_t external_offset = sym.data_offset ();
    std::size_t frame_offset = sym.frame_offset ();

    if (m_fcn->is_nested_function ())
      return get_scope_flag_internal (external_offset, frame_offset);

    bool is_script = m_fcn->is_user_script ();

    std::size_t local_offset;
    bool allready_added;
    bool in_this_frame = maybe_external_to_local_offset (external_offset, frame_offset, local_offset, allready_added);

    if (frame_offset == 0 && is_script && !allready_added)
      {
        // Try to find the symbol in the root frame instead.
        auto sym_in_root = access_link ()->lookup_symbol (sym.name ());
        return access_link ()->scope_flag (sym_in_root);
      }
    else if (frame_offset == 0 && in_this_frame)
      return get_scope_flag_internal (external_offset, frame_offset);

    // If we have a frame offset and are in a bytecode script frame
    // we need to follow the access links until we got the correct frame
    auto access_frame = access_link ();
    while (--frame_offset && access_frame)
      access_frame = access_frame->access_link ();

    if (!access_frame)
      error ("Invalid call to bytecode_fcn_stack_frame::scope_flag()");

    return access_frame->get_scope_flag (external_offset);
  }

  virtual octave_value get_active_bytecode_call_arg_names ()
  {

    // Handle ARG_NAMES
    if (!m_unwind_data || !m_vm)
      return Cell {};

    int best_match = -1;
    int best_start = -1;

    auto &entries = m_unwind_data->m_argname_entries;
    for (unsigned i = 0; i < entries.size (); i++)
      {
        int start = entries[i].m_ip_start;
        int end = entries[i].m_ip_end;

        if (start > (m_ip - 1) || end < (m_ip - 1))
          continue;

        if (best_match != -1)
          {
            if (best_start > start)
              continue;
          }

        best_match = i;
        best_start = start;
      }

    if (best_match == -1)
      return Cell {};

    Cell c = entries[best_match].m_arg_names;
    return c;
  }

  virtual void set_active_bytecode_ip (int ip)
  {
    m_ip = ip;
  }

  octave_value get_auto_fcn_var (auto_var_type avt) const
  {
    switch (avt)
      {
        case stack_frame::NARGIN:
          return octave_value {m_nargin};
        case stack_frame::NARGOUT:
          return octave_value {m_nargout};
        case stack_frame::SAVED_WARNING_STATES:
          if (!m_lazy_data)
            return {};
          else
            return m_lazy_data->m_saved_warnings_states;
        case stack_frame::IGNORED:
          if (!m_lazy_data)
            return {};
          else
            return m_lazy_data->m_ignored;
        case stack_frame::ARG_NAMES:
        {
          // If the current bytecode stack frame is the root one in the VM, the caller
          // sets ARG_NAMES in the root bytecode stack frame
          if (m_lazy_data)
            {
              octave_value ov = m_lazy_data->m_arg_names;
              if (ov.is_defined ())
                return ov;
            }
          // In bytecode stack frames, the arg names are stored in the caller frame.
          return m_parent_link->get_active_bytecode_call_arg_names ();
        }
        default:
          panic ("bytecode_fcn_stack_frame::get_auto_fcn_var() : Invalid call idx=%d", static_cast<int> (avt));
      }
  }

  void set_nargin (int nargin) { m_nargin = nargin; }
  void set_nargout (int nargout) { m_nargout = nargout; }

  void set_auto_fcn_var (auto_var_type avt, const octave_value& val)
  {
    switch (avt)
      {
        case stack_frame::NARGIN:
          m_nargin = val.int_value ();
          return;
        case stack_frame::NARGOUT:
          m_nargout = val.int_value ();
          return;
        case stack_frame::SAVED_WARNING_STATES:
          lazy_data ().m_saved_warnings_states = val;
          return;
        case stack_frame::IGNORED:
          lazy_data ().m_ignored = val;
          return;
        case stack_frame::ARG_NAMES:
          lazy_data ().m_arg_names = val;
          return;
        default:
          panic ("bytecode_fcn_stack_frame::set_auto_fcn_var() : Invalid call idx=%d", static_cast<int> (avt));
      }
  }

  // We only need to override one of each of these functions.  The
  // using declaration will avoid warnings about partially-overloaded
  // virtual functions.
  using stack_frame::varval;
  using stack_frame::varref;

  octave_value varval_internal (std::size_t local_offset) const
  {
    size_t extra_size = (m_lazy_data ? m_lazy_data->m_extra_slots.size () : 0);
    size_t stack_slots = m_size;

    if (local_offset == 0) // Handle native int %nargout specially
      return octave_value {m_stack_start [0].i};
    if (local_offset < stack_slots)
      {
        octave_value ov = m_stack_start [local_offset].ov;
        if (ov.is_ref ())
          return ov.ref_rep ()->deref ();
        return ov;
      }
    else
      {
        std::size_t extra_offset = local_offset - stack_slots;
        if (!m_lazy_data || extra_offset >= extra_size)
          error ("VM internal error: Trying to access extra slot out of range, %zu", extra_offset);
        octave_value ov = m_lazy_data->m_extra_slots.at (extra_offset);
        if (ov.is_ref ())
          return ov.ref_rep ()->deref ();
        return ov;
      }
  }

  octave_value varval (std::size_t external_offset) const
  {
    std::size_t local_offset = external_to_local_offset (external_offset);
    return varval_internal (local_offset);
  }

  octave_value varval (const symbol_record& sym) const
  {
    std::size_t external_offset = sym.data_offset ();
    std::size_t frame_offset = sym.frame_offset ();
    std::size_t local_offset;
    bool allready_added;
    bool in_this_frame = maybe_external_to_local_offset (external_offset, frame_offset, local_offset, allready_added);

    bool is_script = m_fcn->is_user_script ();
    bool is_nested = m_fcn->is_nested_function ();

    if (!allready_added && (is_nested || is_script))
      {
        // Try to find the symbol in the root frame instead.
        auto sym_in_root = access_link ()->lookup_symbol (sym.name ());
        return access_link ()->varval (sym_in_root);
      }
    else if (!in_this_frame)
      error ("VM internal error: Invalid call to varval() with frame offset %zu, name '%s'", frame_offset, sym.name ().c_str ());

    // If the offset is out of range we return a nil ov, unless this is a script frame,
    // in which case we need to look in enclosing frames for the symbol.
    //
    // Normaly any symbol will be moved to the current script frame, but a call to eval might
    // refer to a symbol in an enclosing frame that has not been moved.
    if (local_offset >= internal_size ())
      {
        if (!m_fcn->is_user_script ())
          return {};
        else
          {
            // Look in the enclosing frames for the symbol
            auto frame = access_link ();
            std::string name = sym.name ();

            while (frame)
            {
              symbol_scope scope = frame->get_scope ();

              symbol_record rec = scope.lookup_symbol (name);

              if (rec)
                return frame->varval (rec);

              frame = frame->access_link ();
            }

            return {}; // Nothing found
          }
      }
    if (local_offset >= m_size)
      {
        std::string nm_src = sym.name ();
        if (!m_lazy_data)
          panic ("bytecode_fcn_stack_frame::varval() Invalid request");
        std::string &nm = m_lazy_data->m_extra_names.at (local_offset - m_size);
        if (nm == "")
          nm = nm_src;
        else if (nm != nm_src && nm_src != "")
          error ("VM internal error: Trying to access extra slot with the wrong name. Current: %s, New: %s\n",
            nm.c_str (), nm_src.c_str ());
      }

    bool is_global = slot_is_global (local_offset);
    bool is_pers = slot_is_persistent (local_offset);

    if (is_global)
      return m_evaluator.global_varval (sym.name ());
    if (is_pers)
      return get_scope ().persistent_varval (external_offset);

    return varval_internal (local_offset);
  }

  octave_value& varref_internal (std::size_t local_offset, bool deref_refs)
  {
    static octave_value fake_dummy_nargout{0};

    std::size_t extra_size = (m_lazy_data ? m_lazy_data->m_extra_slots.size () : 0);
    std::size_t stack_slots = m_size;

    // Handle native int %nargout specially. Note that changing
    // the value of %nargout via the this ref wont work.
    if (local_offset == 0)
      return fake_dummy_nargout = octave_value {m_stack_start [0].i};
    if (local_offset < stack_slots)
      {
        octave_value &ov = m_stack_start [local_offset].ov;
        if (deref_refs && ov.is_ref ())
          return ov.ref_rep ()->ref ();
        return ov;
      }
    else
      {
        std::size_t extra_offset = local_offset - stack_slots;
        if (!m_lazy_data || extra_offset >= extra_size)
          error ("VM internal error: Trying to access extra slot out of range, %zu", extra_offset);
        octave_value &ov = m_lazy_data->m_extra_slots.at (extra_offset);
        if (deref_refs && ov.is_ref ())
          return ov.ref_rep ()->ref ();
        return ov;
      }
  }

  octave_value& varref (std::size_t external_offset, bool deref_refs)
  {
    std::size_t local_offset = external_to_local_offset (external_offset);
    return varref_internal (local_offset, deref_refs);
  }

  octave_value& varref (const symbol_record& sym, bool deref_refs)
  {
    bool add_to_parent_scriptframes = false;
    std::size_t external_offset = sym.data_offset ();
    std::size_t frame_offset = sym.frame_offset ();

    std::size_t local_offset;
    bool allready_added;
    bool in_this_frame = maybe_external_to_local_offset (external_offset, frame_offset, local_offset, allready_added);

    bool is_nested = m_fcn->is_nested_function ();
    bool is_script = m_fcn->is_user_script ();

    if (!allready_added && (is_nested || is_script))
      {
        // Try to find the symbol in the root frame instead.
        auto sym_in_root = access_link ()->insert_symbol (sym.name ());
        CHECK_PANIC (sym_in_root.frame_offset () == 0);
        return access_link ()->varref (sym_in_root, deref_refs);
      }
    else if (!in_this_frame)
      panic ("VM internal error: Invalid call to varref() with frame offset %zu, name '%s'", frame_offset, sym.name ().c_str ());

    // If the offset is out of range we make room for it
    if (local_offset >= internal_size ())
    {
      internal_resize (local_offset + 1);
      internal_set_extra_name (local_offset, sym.name ());

      // If this bytecode frame is a script we need to add the symbol to the parent frames
      if (m_fcn->is_user_script ())
        add_to_parent_scriptframes = true;
    }
    if (local_offset >= m_size)
      {
        std::string nm_src = sym.name ();
        if (!m_lazy_data)
          panic ("bytecode_fcn_stack_frame::varval() Invalid request");
        std::string &nm = m_lazy_data->m_extra_names.at (local_offset - m_size);
        if (nm == "")
          nm = nm_src;
        else if (nm != nm_src && nm_src != "")
          error ("VM internal error: Trying to access extra slot with wrong name. Current: %s, New: %s\n",
            nm.c_str (), nm_src.c_str ());
      }

    if (add_to_parent_scriptframes)
      {
        // Mirrors what happens in vm_enter_script ().
        // Essentially the octave_value in the enclosing script frame
        // is stolen and put in this frame, and a pointer to the value in this
        // frame is put in the enclosing frame.
        symbol_record script_sr = sym;
        std::string name = sym.name ();

        auto eval_frame = access_link ();
        auto eval_scope = eval_frame->get_scope ();
        auto parent_frame = parent_link ();
        bool caller_is_eval_frame = eval_frame == parent_frame;


        symbol_record eval_scope_sr;

        const std::map<std::string, symbol_record>& eval_scope_symbols
          = eval_scope.symbols ();

        auto p = eval_scope_symbols.find (name);

        if (p == eval_scope_symbols.end ())
          eval_scope_sr = eval_scope.insert (name);
        else
          eval_scope_sr = p->second;

        if (eval_frame->is_global (eval_scope_sr))
          {
            mark_global (script_sr);

            octave_value &orig = eval_frame->varref (eval_scope_sr.data_offset (), false);
            orig = octave_value (new octave_value_ref_vmlocal {script_sr, this});

            if (!caller_is_eval_frame)
              {
                symbol_record sr_parent = parent_frame->lookup_symbol (script_sr.name ());
                CHECK_PANIC (sr_parent.is_valid ());
                CHECK_PANIC (sr_parent.frame_offset () == 0);

                octave_value &parent_ref = parent_frame->varref (sr_parent, false);

                CHECK_PANIC (parent_frame->is_global (sr_parent));

                if (&parent_ref != &orig)
                  {
                    CHECK_PANIC (parent_ref.is_ref () && parent_ref.ref_rep ()->is_global_ref ());
                    parent_ref = octave_value (new octave_value_ref_vmlocal {script_sr, this});
                  }
              }
          }
        else
          {
            octave_value &orig_ref = eval_frame->varref (eval_scope_sr, false); // Ref to value on parent's VM stack
            octave_value &ref = varref (script_sr, false); // Pointer to current VM stack frame

            CHECK_PANIC (&ref != &orig_ref);

            if (!caller_is_eval_frame)
              {
                symbol_record sr_parent = parent_frame->lookup_symbol (script_sr.name ());
                CHECK_PANIC (sr_parent.is_valid ());

                // If the symbol is not in the parent frame, we add it to it.
                if (sr_parent.frame_offset ())
                  {
                    // Move the value from whatever parent has the it to the current stack frame
                    ref = parent_frame->varref (sr_parent, false);

                    sr_parent = parent_frame->insert_symbol (script_sr.name ()); // TODO: Need to be done to all parents missing it?
                    octave_value &parent_ref = parent_frame->varref (sr_parent, false);

                    CHECK_PANIC (&parent_ref != &orig_ref);
                    CHECK_PANIC (&parent_ref != &ref);

                    parent_ref = octave_value (new octave_value_ref_vmlocal {script_sr, this});
                  }
                else
                  {
                    octave_value &parent_ref = parent_frame->varref (sr_parent, false);

                    CHECK_PANIC (&ref != &parent_ref);
                    CHECK_PANIC (&parent_ref != &orig_ref);

                    ref = parent_ref; // Move parent's value to the current stack frame
                    parent_ref = octave_value (new octave_value_ref_vmlocal {script_sr, this});
                  }
              }
            else
              {
                ref = orig_ref; // Move eval frame's value to current stack frame
              }

            orig_ref = octave_value (new octave_value_ref_vmlocal {script_sr, this}); // Replace parents value with reference ov.
          }
      }

    return varref_internal (local_offset, deref_refs);
  }

  void mark_scope (const symbol_record& sym,
                   scope_flags flag)
  {
    bool was_global = is_global (sym);
    bool is_script = m_lazy_data && m_lazy_data->m_is_script;
    bool add_global_to_scripts = flag == GLOBAL && is_script;
    bool remove_global_from_scripts = flag != GLOBAL && is_script && was_global;

    bool is_nested = m_fcn->is_nested_function ();

    std::size_t external_offset = sym.data_offset ();
    std::size_t frame_offset = sym.frame_offset ();

    std::size_t local_offset;
    bool allready_added;
    bool in_this_frame = maybe_external_to_local_offset (external_offset, frame_offset, local_offset, allready_added);

    if (!in_this_frame || ((is_script || is_nested) && !allready_added))
      {
        if (!is_script && !is_nested)
          error ("VM internal error: Invalid call to mark_scope");

        auto sym_in_root = access_link ()->insert_symbol (sym.name ()); // Adds symbol if doesn't exist
        access_link ()->mark_scope (sym_in_root, flag);
        return;
      }

    if (local_offset >= internal_size ())
      {
        internal_resize (local_offset + 1);
        internal_set_extra_name (local_offset, sym.name ());
      }

    set_scope_flag (external_offset, flag);

    if (add_global_to_scripts)
      {
        symbol_record sym_access =  m_access_link->insert_symbol (sym.name ());
        octave_value &ov_access = m_access_link->varref (sym_access.data_offset (), false);

        bool make_ref = true;

        if (ov_access.is_ref ())
          {
            octave_value_ref *r = ov_access.ref_rep ();
            if (r->is_local_ref ())
              make_ref = false;
          }

        m_access_link->mark_global (sym_access);

        if (make_ref)
          ov_access = octave_value (new octave_value_ref_vmlocal {sym, this});
      }
    else if (remove_global_from_scripts)
      {
        symbol_record sym_access =  m_access_link->insert_symbol (sym.name ());
        octave_value &ov_access = m_access_link->varref (sym_access.data_offset (), false);

        bool make_ref = true;

        if (ov_access.is_ref ())
          {
            octave_value_ref *r = ov_access.ref_rep ();
            if (r->is_local_ref ())
              make_ref = false;
          }

        m_access_link->unmark_global (sym_access);

        if (make_ref)
          ov_access = octave_value (new octave_value_ref_vmlocal {sym, this});
      }
  }

  bool is_bytecode_fcn_frame (void) const { return true; }

  symbol_record lookup_symbol (const std::string& name) const
  {
    int local_offset = -1;
    scope_flags flag = LOCAL;

    for (int i = 0; i < static_cast<int> (m_size); i++)
      {
        if (m_name_data [i] == name)
          {
            local_offset = i;

            bool is_global = slot_is_global (local_offset);
            bool is_pers = slot_is_persistent (local_offset);

            if (is_global)
              flag = GLOBAL;
            else if (is_pers)
              flag = PERSISTENT;

            break;
          }
      }

    if (local_offset >= 0)
      {
        symbol_record ret (name, flag);

        size_t frame_offset;
        size_t offset;

        local_to_external_offset (static_cast<std::size_t> (local_offset), offset, frame_offset);

        ret.set_data_offset (offset);
        ret.set_frame_offset (frame_offset);

        // Check if the symbol is an argument or return symbol. Note: Negative count for vararg and varargin
        int n_returns = static_cast<signed char> (m_code[0]);
        if (n_returns == -128) // Magic number for anonymous function's "dynamic amount of returns"
          n_returns = 0;
        else
          n_returns = abs (n_returns);
        int n_args = abs (static_cast<signed char> (m_code[1]));
        if (local_offset < n_returns + n_args)
          ret.mark_formal ();

        return ret;
      }

    if (m_lazy_data)
      {
        for (unsigned i = 0; i < m_lazy_data->m_extra_slots.size (); i++)
          {
            if (m_lazy_data->m_extra_names.at (i) == name)
              {
                symbol_record ret (name, flag);
                ret.set_data_offset (m_orig_size + i);
                return ret;
              }
          }
      }

    // Search the "scope" object of this and any nested frame
    // The scope object will have e.g. variables added by scripts or eval
    const stack_frame *frame = this;
    std::size_t frame_cntr = 0;
    while (frame)
      {
        symbol_scope scope = frame->get_scope ();

        symbol_record sym = scope.lookup_symbol (name);

        if (sym)
          {
            // Return symbol record with adjusted frame offset (relative to the one lookup is done on)
            symbol_record new_sym = sym.dup ();
            new_sym.set_frame_offset (frame_cntr);
            return new_sym;
          }

        std::shared_ptr<stack_frame> nxt = frame->access_link ();
        frame = nxt.get ();
        frame_cntr++;
      }

    return symbol_record ();
  }

  symbol_record insert_symbol (const std::string& name)
  {
    bool is_script = m_lazy_data && m_lazy_data->m_is_script;
    bool allready_added;

    // If the symbols is already in the immediate scope, there is
    // nothing more to do.

    symbol_scope scope = get_scope ();

    symbol_record sym = scope.lookup_symbol (name);

    if (!sym)
      {
        // If we have not created the extra slots, now is the time
        lazy_data ();

        sym = scope.find_symbol (name);

        CHECK_PANIC (sym.is_valid ());
        CHECK_PANIC (sym.frame_offset () == 0);

        unsigned local_offset = external_to_local_offset (sym.data_offset ());

        if (local_offset >= internal_size ())
          internal_resize (local_offset + 1);

        internal_set_extra_name (local_offset, sym.name ());

        allready_added = false;
      }
    else
      allready_added = true;

    // If we are in a script, we might need to add the symbol to the eval frame too,
    // and point a ref to the script's slot from the eval frame.
    if (is_script)
      {
        CHECK_PANIC (m_access_link);

        bool access_allready_added;
        
        symbol_record sym_access = m_access_link->lookup_symbol (name);
        if (sym_access.is_valid ())
          access_allready_added = true;
        else
          {
            access_allready_added = false;
            sym_access =  m_access_link->insert_symbol (name);
          }

        CHECK_PANIC ((allready_added && access_allready_added) || !allready_added);

        if (!allready_added)
          {
            octave_value &ov_access = m_access_link->varref (sym_access.data_offset (), false);
            octave_value &ov = varref (sym.data_offset (), false);

            bool make_ref = true;

            if (ov_access.is_ref ())
              {
                octave_value_ref *r = ov_access.ref_rep ();
                if (r->is_local_ref ())
                  make_ref = false;
              }
            
            if (make_ref)
              {
                ov = ov_access;
                ov_access = octave_value (new octave_value_ref_vmlocal {sym, this});
              }
          }
      }

    return sym;
  }

  symbol_scope get_scope (void) const
  {
    return m_fcn->scope ();
  }

  octave_function * function () const { return m_fcn; }

  void accept (stack_frame_walker& sfw);

  void display (bool follow = true) const;

  int line () const
  {
    if (! m_vm)
      return -1;

    loc_entry loc = vm::find_loc (m_ip, m_unwind_data->m_loc_entry); // TODO: Does not work in nested bytecode stack frames
    return loc.m_line;
  }

  int column () const
  {
    if (! m_vm)
      return -1;

    loc_entry loc = vm::find_loc (m_ip, m_unwind_data->m_loc_entry);
    return loc.m_col;
  }

  unwind_protect *unwind_protect_frame ()
  {
    if (! lazy_data ().m_unwind_protect_frame)
      lazy_data ().m_unwind_protect_frame = new unwind_protect ();

    return lazy_data ().m_unwind_protect_frame;
  }

  std::weak_ptr<stack_frame> m_weak_ptr_to_self;

  void vm_enter_nested ()
  {
    // We got multiple scenarios.
    //
    // 1. The parents are bytecode functions in the order
    //    according to the nesting of nested functions. This should be the
    //    most common use. Self-recursive calls are handled too.
    //
    // 2. If a nested function calls a sibling nested function, the parent frames
    //    which are not direct parents to the nested sibling funtion need to be skipped
    //    while searching for the matriarch frame.
    //
    // 3. A nested function is called via a handle.

    bool is_direct_call = true;

    auto parent_frame = static_link ();

    auto *child_bc_frame = this;

    // Walk the parent(s) to see if they are in direct order and all bytecode frames.
    // Collect the bsp. (base stack pointer)
    int n_nested_depth = m_unwind_data->m_n_nested_fn;
    std::vector<stack_element*> v_parent_bsps;
    int i;
    for (i = 0; i < n_nested_depth; i++)
      {
        bool collect_frame_bsp = true;
        bool forward_child_ptr = true;

        if (!parent_frame->is_bytecode_fcn_frame ())
          {
            is_direct_call = false;
            break;
          }

        auto *parent_bc_frame = static_cast<bytecode_fcn_stack_frame*> (parent_frame.get ());

        // Recursive self call?
        if (parent_bc_frame->m_unwind_data->m_id == child_bc_frame->m_unwind_data->m_id)
          {
            collect_frame_bsp = false;
            i--;
          }
        // Not direct parent?
        else if (parent_bc_frame->m_unwind_data->m_id != child_bc_frame->m_unwind_data->m_parent_id)
          {
            // Sibling or their children?
            if (parent_bc_frame->m_unwind_data->m_matriarch_id == child_bc_frame->m_unwind_data->m_matriarch_id)
              {
                collect_frame_bsp = false;
                forward_child_ptr = false; // Keep looking for the parent of the current child ptr
                i--;
              }
            else
              {
                is_direct_call = false;
                break;
              }
          }

        // We don't collect recursive self-calls' or siblings' or siblings' childrens' frames
        if (collect_frame_bsp)
          v_parent_bsps.push_back (parent_bc_frame->m_stack_start);

        // Skip copying the shared pointer if we don't need it as there is no next iteration
        // since shared_ptr:s are quite expensive to use.
        if (i + 1 == n_nested_depth)
          break;

        parent_frame = parent_frame->static_link ();
        if (forward_child_ptr)
          child_bc_frame = parent_bc_frame;
        parent_bc_frame = static_cast<bytecode_fcn_stack_frame*> (parent_frame.get ());
      }

    // Nested function handles have a closure context
    bool has_closure = access_link ()->is_closure_context ();

    if (is_direct_call && ! has_closure)
      {
        for (unwind_data::nested_var_offset &d : m_unwind_data->m_v_nested_vars)
        {
          int parent_slot = d.m_slot_parent;
          int nested_slot = d.m_slot_nested;
          int depth = d.m_depth;

          stack_element *owner_bsp = v_parent_bsps.at (depth - 1);

          octave_value &orig_ov = owner_bsp[parent_slot].ov; // On the parent stack
          octave_value &nested_ov = m_stack_start[nested_slot].ov; // On the current stack

          // If the ov on the parent stack is a pointer reference we need to follow it.
          if (orig_ov.is_ref ())
            {
              auto ref_rep = orig_ov.ref_rep ();
              if (ref_rep->is_ptr_ref ())
                orig_ov = ref_rep->ref ();
            }

          CHECK_PANIC (&orig_ov != &nested_ov);

          // Make the nested ov reference the ov on the parent stack.
          nested_ov = new octave_value_ref_ptr (&orig_ov);
        }
      }
    else
      {
        // For a nested function at nesting depth n we need to collect n
        // access links.
        auto first_context_frame = access_link ();
        CHECK_PANIC (first_context_frame);

        std::vector<decltype(first_context_frame)> v_frames {std::move (first_context_frame)};

        for (int j = 1; j < m_unwind_data->m_n_nested_fn; j++) // 1, since first one already added
          {
            auto &upper_frame = v_frames.back ();
            auto lower_frame = upper_frame->access_link ();
            CHECK_PANIC (lower_frame);
            v_frames.push_back (lower_frame);
          }

        // For each variable that refer to variables on the parent frames, we need to link
        // the local variable of the current frame to the correct slot on the parents'.
        for (unwind_data::nested_var_offset &d : m_unwind_data->m_v_nested_vars)
          {
            int parent_slot = d.m_slot_parent;
            int nested_slot = d.m_slot_nested;
            int depth = d.m_depth;

            octave_value &nested_ov = m_stack_start[nested_slot].ov; // On the current stack

            auto &context_frame = v_frames.at (depth - 1);
            auto context_scope = context_frame->get_scope ();

            auto sym = context_scope.find_symbol (m_name_data [nested_slot]);
            CHECK_PANIC (sym.is_valid ());

            // For bytecode frames we just do a pointer octave_value object refering to
            // to address in memory. For other frames we need to access it via the dynamic frame pointer
            // (since the memory in those can move around)
            if (context_frame->is_bytecode_fcn_frame ())
              {
                auto *context_bc_frame = static_cast<bytecode_fcn_stack_frame*> (context_frame.get ());
                auto owner_bsp = context_bc_frame->m_stack_start;
                octave_value &orig_ov = owner_bsp[parent_slot].ov; // On the parent stack

                // If the ov on the parent stack is a pointer reference we need to follow it.
                if (orig_ov.is_ref ())
                  {
                    auto ref_rep = orig_ov.ref_rep ();
                    if (ref_rep->is_ptr_ref ())
                      orig_ov = ref_rep->ref ();
                  }

                CHECK_PANIC (&orig_ov != &nested_ov);

                nested_ov = new octave_value_ref_ptr (&orig_ov); // Pointer object octave_value_ref_ptr to parent stack
              }
            else
              nested_ov = new octave_value_ref_vmlocal (sym, context_frame.get ());
          }
      }
  }

  void vm_enter_script ()
  {
    CHECK_PANIC (m_fcn->is_user_script ());
    // Check that there are no "extra slots" in the current frame. Those should have been added to the eval frame.
    CHECK_PANIC (!(m_lazy_data && m_lazy_data->m_extra_slots.size () != 0));

    lazy_data ().m_is_script = true;

    auto eval_frame = access_link ();
    auto parent_frame = parent_link ();

    bool caller_is_eval_frame = eval_frame == parent_frame;
    bool eval_frame_is_bytecode = eval_frame->is_bytecode_fcn_frame ();

    if (!caller_is_eval_frame)
      {
        CHECK_PANIC (parent_frame->is_bytecode_fcn_frame ());
        auto *parent_frame_bc = static_cast<bytecode_fcn_stack_frame*> (parent_frame.get ());

        // Check that there are no "extra slots" in the parent frame. Those should have been added to the eval frame
        CHECK_PANIC (!(parent_frame_bc->m_lazy_data && parent_frame_bc->m_lazy_data->m_extra_slots.size () != 0));

        // Move all user symbol values from the parent frame to the eval frame.
        // Replace the values in the parent frame with a pointer-like object "octave_value_ref_vmlocal"
        // pointing to the eval frame.
        for (std::string id_name : parent_frame_bc->m_unwind_data->m_set_user_locals_names)
          {
            symbol_record sr_eval = eval_frame->lookup_symbol (id_name);
            if (!sr_eval.is_valid ())
              sr_eval = eval_frame->insert_symbol (id_name);
            eval_frame->varref (sr_eval, false); // A bit silly, but allocates space for it

            // We need to use the varref(size_t) since it gets in "behind" any global value in
            // top scope, directly to the m_value vector. While the varref(symbol_record) returns
            // a ref to the global value itself. Unless the frame offset is set, in which case we
            // need to use the varref(symbol_record) variant to walk access frames properly. E.g.
            // 'nest.tst' need frame offset when a script tries to access a variable from a nested
            // function.
            // TODO: Fix this hack
            octave_value *ov_eval = sr_eval.frame_offset () ? 
                                      &eval_frame->varref (sr_eval, false) :
                                      &eval_frame->varref (sr_eval.data_offset (), false);

            symbol_record sr_parent = parent_frame->lookup_symbol (id_name); // TODO: Store slot nr instead?
            CHECK_PANIC (sr_parent.is_valid () && sr_parent.frame_offset () == 0);
            octave_value &ov_parent = parent_frame->varref (sr_parent.data_offset (), false);

            CHECK_PANIC (!(ov_parent.is_ref () && ov_parent.ref_rep ()->is_local_ref ()));

            bool is_global_in_eval_frame = eval_frame->is_global (sr_eval);
            bool is_global_in_parent_frame = parent_frame->is_global (sr_parent);
            CHECK_PANIC (is_global_in_eval_frame == is_global_in_parent_frame);

            if (is_global_in_parent_frame)
              CHECK_PANIC (ov_parent.is_ref () && ov_parent.ref_rep ()->is_global_ref ());

            if (!is_global_in_parent_frame || eval_frame_is_bytecode)
              *ov_eval = ov_parent;
            else
              *ov_eval = {};

            ov_parent = octave_value {new octave_value_ref_vmlocal {sr_eval, eval_frame.get ()}};
          }
      }

    // Move all user symbols from the eval frame to the current frame we are entering.
    // Replace the moved values in the eval frame with a pointer-like object "octave_value_ref_vmlocal"
    // pointing to the current frame.
    for (std::string id_name : m_unwind_data->m_set_user_locals_names)
      {
        symbol_record sr_eval = eval_frame->lookup_symbol (id_name);
        if (!sr_eval.is_valid ())
          sr_eval = eval_frame->insert_symbol (id_name);
        eval_frame->varref (sr_eval, false); // A bit silly, but allocates space for it

        octave_value *ov_eval = sr_eval.frame_offset () ? 
                                  &eval_frame->varref (sr_eval, false) :
                                  &eval_frame->varref (sr_eval.data_offset (), false);

        symbol_record sr_current = lookup_symbol (id_name);
        CHECK_PANIC (sr_current.is_valid () && sr_current.frame_offset () == 0);
        octave_value &ov_current = varref (sr_current.data_offset (), false);

        CHECK_PANIC (!(ov_current.is_ref () && ov_current.ref_rep ()->is_local_ref ()));
        CHECK_PANIC (!(ov_eval->is_ref () && ov_eval->ref_rep ()->is_local_ref ()));

        bool is_global_in_eval_frame = eval_frame->is_global (sr_eval);

        if (is_global_in_eval_frame)
          ov_current = octave_value {new octave_value_ref_global {id_name}};
        else
          ov_current = *ov_eval;

        *ov_eval = octave_value {new octave_value_ref_vmlocal {sr_current, this}};
      }
  }

  void vm_exit_script ()
  {
    if (!m_fcn->is_user_script ()) // Nothing to do for non-script frames
      return;

    // Restore values from the VM stack frame to the original frame

    // Check that there are no "extra slots" in the current frame. Those should have been added to the eval frame.
    CHECK_PANIC (!(m_lazy_data && m_lazy_data->m_extra_slots.size () != 0));

    lazy_data ().m_is_script = true;

    auto eval_frame = access_link ();
    auto parent_frame = parent_link ();

    bool caller_is_eval_frame = eval_frame == parent_frame;
    bool eval_frame_is_bytecode = eval_frame->is_bytecode_fcn_frame ();

    // Move all user symbols from the current frame to the eval frame.
    for (std::string id_name : m_unwind_data->m_set_user_locals_names)
      {
        symbol_record sr_eval = eval_frame->lookup_symbol (id_name);
        CHECK_PANIC (sr_eval.is_valid ());
        octave_value *ov_eval = sr_eval.frame_offset () ?
                                  &eval_frame->varref (sr_eval, false) :
                                  &eval_frame->varref (sr_eval.data_offset (), false);

        symbol_record sr_current = lookup_symbol (id_name);
        CHECK_PANIC (sr_current.is_valid () && sr_current.frame_offset () == 0);
        octave_value &ov_current = varref (sr_current.data_offset (), false);

        CHECK_PANIC (!(ov_current.is_ref () && ov_current.ref_rep ()->is_local_ref ()));

        bool is_global_in_eval_frame = eval_frame->is_global (sr_eval);
        bool is_global_in_current_frame = is_global (sr_current);
        CHECK_PANIC (is_global_in_eval_frame == is_global_in_current_frame);

        if (is_global_in_current_frame)
          CHECK_PANIC (ov_current.is_ref () && ov_current.ref_rep ()->is_global_ref ());

        if (!is_global_in_current_frame || eval_frame_is_bytecode)
          *ov_eval = ov_current;
        else
          *ov_eval = {};

        ov_current = {};
      }

    // Move all values the parent frame needs to it from the eval frame
    if (!caller_is_eval_frame)
      {
        CHECK_PANIC (parent_frame->is_bytecode_fcn_frame ());
        auto *parent_frame_bc = static_cast<bytecode_fcn_stack_frame*> (parent_frame.get ());

        // Check that there are no "extra slots" in the parent frame. Those should have been added to the eval frame
        CHECK_PANIC (!(parent_frame_bc->m_lazy_data && parent_frame_bc->m_lazy_data->m_extra_slots.size () != 0));

        // Move all values the parent frame needs to it from the eval frame.
        // In the eval frame, put a pointer-like object "octave_value_ref_vmlocal"
        // pointing to the parent frame
        for (std::string id_name : parent_frame_bc->m_unwind_data->m_set_user_locals_names)
          {
            symbol_record sr_eval = eval_frame->lookup_symbol (id_name);
            CHECK_PANIC (sr_eval.is_valid ());
            octave_value *ov_eval = sr_eval.frame_offset () ?
                                      &eval_frame->varref (sr_eval, false) :
                                      &eval_frame->varref (sr_eval.data_offset (), false);

            symbol_record sr_parent = parent_frame->lookup_symbol (id_name); // TODO: Store slot nr instead?
            CHECK_PANIC (sr_parent.is_valid () && sr_parent.frame_offset () == 0);
            octave_value &ov_parent = parent_frame->varref (sr_parent.data_offset (), false);

            CHECK_PANIC (!(ov_eval->is_ref () && ov_eval->ref_rep ()->is_local_ref ()));

            bool is_global_in_eval_frame = eval_frame->is_global (sr_eval);
            bool is_global_in_parent_frame = parent_frame->is_global (sr_parent);
            CHECK_PANIC (is_global_in_eval_frame == is_global_in_parent_frame);

            if (!is_global_in_parent_frame || eval_frame_is_bytecode)
              ov_parent = *ov_eval;
            else
              ov_parent = octave_value {new octave_value_ref_global {id_name}};

            *ov_eval = octave_value {new octave_value_ref_vmlocal {sr_parent, parent_frame.get ()}};
          }
      }
  }

  void break_closure_cycles (const std::shared_ptr<stack_frame> &frame)
  {
    if (m_stack_start)
      {
        for (unsigned i = 1; i < m_size; i++)
          m_stack_start[i].ov.break_closure_cycles (frame);
      }

    if (m_lazy_data)
      {
        for (octave_value &ov : m_lazy_data->m_extra_slots)
          ov.break_closure_cycles (frame);
      }

    if (m_access_link)
      m_access_link->break_closure_cycles (frame);
  }

  // To keep down the footprint of the frame some seldom used
  // variables are lazy initialized and stored in *m_lazy_data
  struct lazy_data_struct
  {
    octave_value m_ignored;
    octave_value m_arg_names;
    octave_value m_saved_warnings_states;

    std::vector<octave_value> m_extra_slots;
    std::vector<std::string> m_extra_names;

    unwind_protect *m_unwind_protect_frame = nullptr;
    stack_element *m_stack_cpy = nullptr;
    bool m_is_script;
  };

  lazy_data_struct & lazy_data ()
  {
    if (!m_lazy_data)
      m_lazy_data = new lazy_data_struct {};
    return *m_lazy_data;
  }

  bool is_script_frame () { return m_unwind_data->m_is_script; }

  lazy_data_struct *m_lazy_data = nullptr;

private:
  octave_user_code *m_fcn;

  unwind_data *m_unwind_data;

  std::string *m_name_data;
  stack_element *m_stack_start;

  unsigned char *m_code;
  unsigned m_size;
  unsigned m_orig_size;
  vm *m_vm;
  int m_ip;

  int m_nargin;
  int m_nargout;
};

class compiled_fcn_stack_frame : public stack_frame
{
public:

  compiled_fcn_stack_frame () = delete;

  compiled_fcn_stack_frame (tree_evaluator& tw, octave_function *fcn,
                            std::size_t index,
                            const std::shared_ptr<stack_frame>& parent_link,
                            const std::shared_ptr<stack_frame>& static_link)
    : stack_frame (tw, index, parent_link, static_link,
                   static_link->access_link ()),
      m_fcn (fcn)
  { }

  compiled_fcn_stack_frame (const compiled_fcn_stack_frame& elt) = default;

  compiled_fcn_stack_frame&
  operator = (const compiled_fcn_stack_frame& elt) = delete;

  ~compiled_fcn_stack_frame () = default;

  bool is_compiled_fcn_frame () const { return true; }

  symbol_scope get_scope () const
  {
    return m_static_link->get_scope ();
  }

  octave_function * function () const { return m_fcn; }

  symbol_record lookup_symbol (const std::string& name) const
  {
    return m_static_link->lookup_symbol (name);
  }

  symbol_record insert_symbol (const std::string& name)
  {
    return m_static_link->insert_symbol (name);
  }

  stack_frame::scope_flags scope_flag (const symbol_record& sym) const
  {
    // Look in closest stack frame that contains values (either the
    // top scope, or a user-defined function or script).

    return m_static_link->scope_flag (sym);
  }

  void set_auto_fcn_var (auto_var_type avt, const octave_value& val)
  {
    m_static_link->set_auto_fcn_var (avt, val);
  }

  octave_value get_auto_fcn_var (auto_var_type avt) const
  {
    return m_static_link->get_auto_fcn_var (avt);
  }

  // We only need to override one of each of these functions.  The
  // using declaration will avoid warnings about partially-overloaded
  // virtual functions.
  using stack_frame::varval;
  using stack_frame::varref;

  octave_value varval (const symbol_record& sym) const
  {
    // Look in closest stack frame that contains values (either the
    // top scope, or a user-defined function or script).

    return m_static_link->varval (sym);
  }

  octave_value& varref (const symbol_record& sym, bool deref_refs)
  {
    // Look in closest stack frame that contains values (either the
    // top scope, or a user-defined function or script).

    return m_static_link->varref (sym, deref_refs);
  }

  void mark_scope (const symbol_record& sym, scope_flags flag)
  {
    // Look in closest stack frame that contains values (either the
    // top scope, or a user-defined function or script).

    m_static_link->mark_scope (sym, flag);
  }

  void display (bool follow = true) const;

  void accept (stack_frame_walker& sfw);

private:

  // Compiled function object associated with this stack frame.
  // Should always be a built-in, .oct or .mex file function and
  // should always be valid.
  octave_function *m_fcn;
};

// Scripts have a symbol_scope object to store the set of variables
// in the script, but values for those variables are stored in the
// stack frame corresponding to the nearest calling function or in
// the top-level scope (the evaluation stack frame).
//
// Accessing values in a scope requires a mapping from the index of
// the variable for the script scope to the list of values in the
// evaluation frame(s).  The frame offset tells us how many access
// links we must follow to find the stack frame that holds the
// value.  The value offset is the index into the vector of values
// in that stack frame that we should use to find the value.
//
// Frame and value offsets are set in this stack frame when it is
// created using information from the script and enclosing scopes.
//
// If a script is invoked in a nested function context, the frame
// offsets for individual values may be different.  Some may be
// accessed from the invoking function and some may come from a
// parent function.

class script_stack_frame : public stack_frame
{
public:

  script_stack_frame () = delete;

  script_stack_frame (tree_evaluator& tw, octave_user_script *script,
                      std::size_t index,
                      const std::shared_ptr<stack_frame>& parent_link,
                      const std::shared_ptr<stack_frame>& static_link);

  script_stack_frame (const script_stack_frame& elt) = default;

  script_stack_frame& operator = (const script_stack_frame& elt) = delete;

  ~script_stack_frame ()
  {
    delete m_unwind_protect_frame;
  }

  bool is_user_script_frame () const { return true; }

  static std::shared_ptr<stack_frame>
  get_access_link (const std::shared_ptr<stack_frame>& static_link);

  static std::size_t get_num_symbols (octave_user_script *script);

  void set_script_offsets ();

  void set_script_offsets_internal (const std::map<std::string,
                                    symbol_record>& symbols);

  void resize_and_update_script_offsets (const symbol_record& sym);

  symbol_scope get_scope () const { return m_script->scope (); }

  octave_function * function () const { return m_script; }

  unwind_protect * unwind_protect_frame ();

  symbol_record lookup_symbol (const std::string& name) const;

  symbol_record insert_symbol (const std::string&);

  std::size_t size () const { return m_lexical_frame_offsets.size (); }

  void resize (std::size_t size)
  {
    m_lexical_frame_offsets.resize (size, 0);
    m_value_offsets.resize (size, 0);
  }

  void get_val_offsets_with_insert (const symbol_record& sym,
                                    std::size_t& frame_offset,
                                    std::size_t& data_offset);

  bool get_val_offsets_internal (const symbol_record& sym,
                                 std::size_t& frame_offset,
                                 std::size_t& data_offset) const;

  bool get_val_offsets (const symbol_record& sym, std::size_t& frame_offset,
                        std::size_t& data_offset) const;

  scope_flags scope_flag (const symbol_record& sym) const;

  void set_auto_fcn_var (auto_var_type avt, const octave_value& val)
  {
    m_access_link->set_auto_fcn_var (avt, val);
  }

  octave_value get_auto_fcn_var (auto_var_type avt) const
  {
    return m_access_link->get_auto_fcn_var (avt);
  }

  // We only need to override one of each of these functions.  The
  // using declaration will avoid warnings about partially-overloaded
  // virtual functions.
  using stack_frame::varval;
  using stack_frame::varref;

  octave_value varval (const symbol_record& sym) const;

  octave_value& varref (const symbol_record& sym, bool deref_refs);

  void mark_scope (const symbol_record& sym, scope_flags flag);

  void display (bool follow = true) const;

  void accept (stack_frame_walker& sfw);

private:

  // Script object associated with this stack frame.  Should always
  // be valid.
  octave_user_script *m_script;

  // The nearest unwind protect frame that was active when this
  // stack frame was created.  Should always be valid.
  unwind_protect *m_unwind_protect_frame;

  // Mapping between the symbols in the symbol_scope object of the
  // script to the stack frame in which the script is executed.  The
  // frame offsets may be greater than one if the script is executed
  // in a nested function context.

  std::vector<std::size_t> m_lexical_frame_offsets;
  std::vector<std::size_t> m_value_offsets;
};

// Base class for values and offsets shared by user_fcn and scope
// frames.

class base_value_stack_frame : public stack_frame
{
public:

  base_value_stack_frame () = delete;

  base_value_stack_frame (tree_evaluator& tw, std::size_t num_symbols,
                          std::size_t index,
                          const std::shared_ptr<stack_frame>& parent_link,
                          const std::shared_ptr<stack_frame>& static_link,
                          const std::shared_ptr<stack_frame>& access_link)
    : stack_frame (tw, index, parent_link, static_link, access_link),
      m_values (num_symbols, octave_value ()),
      m_flags (num_symbols, LOCAL),
      m_auto_vars (NUM_AUTO_VARS, octave_value ())
  { }

  base_value_stack_frame (const base_value_stack_frame& elt) = default;

  base_value_stack_frame&
  operator = (const base_value_stack_frame& elt) = delete;

  ~base_value_stack_frame ()
  {
    // The C++ standard doesn't guarantee in which order the elements of a
    // std::vector are destroyed.  GNU libstdc++ and LLVM libc++ seem to
    // destroy them in a different order.  So, erase elements manually
    // from first to last to be able to guarantee a destructor call order
    // independent of the used STL, e.g., for classdef objects.

    // Member dtor order is last to first.  So, m_auto_vars before m_values.

    for (auto auto_vars_iter = m_auto_vars.begin ();
         auto_vars_iter != m_auto_vars.end ();)
      auto_vars_iter = m_auto_vars.erase (auto_vars_iter);

    for (auto values_iter = m_values.begin ();
         values_iter != m_values.end ();)
      values_iter = m_values.erase (values_iter);
  }

  std::size_t size () const
  {
    return m_values.size ();
  }

  void resize (std::size_t size)
  {
    m_values.resize (size, octave_value ());
    m_flags.resize (size, LOCAL);
  }

  stack_frame::scope_flags get_scope_flag (std::size_t data_offset) const
  {
    return m_flags.at (data_offset);
  }

  void set_scope_flag (std::size_t data_offset, scope_flags flag)
  {
    bool was_global = m_flags.at (data_offset) == scope_flags::GLOBAL;

    m_flags.at (data_offset) = flag;

    bool is_global = flag == scope_flags::GLOBAL;

    // If the VM is running scripts it places octave_value_ref objects in
    // the top scope to "steal" the variables from it to be able to keep the
    // canonical copy on its active stack frame. When it unwinds, the top scope
    // gets the value back.
    //
    // If e.g. evalin () changes the globalness state of a symbol in the top scope,
    // the VM need to be notified, if the variable is on the VM stack.
    if (was_global != is_global)
      {
        octave_value ov = m_values.at (data_offset);
        // Only the VM spreads ref objects around, so this is only true if the VM is running
        if (ov.is_ref ())
          {
            octave_value cpy = ov;
            // Pop the value in the top scope to avoid recursive loop, since mark_globalness_in_owning_frame()
            // will call mark_global (), which will walk the stack frames down to the root.
            m_values.at (data_offset) = octave_value {};
            octave_value_ref *ref = cpy.ref_rep ();
            ref->mark_globalness_in_owning_frame (is_global);
            // We need the ref back in place if the flag changes again
            m_values.at (data_offset) = cpy;
          }
      }
  }

  octave_value get_auto_fcn_var (auto_var_type avt) const
  {
    if (avt != stack_frame::auto_var_type::ARG_NAMES)
      return m_auto_vars.at (avt);
    if (m_parent_link->is_bytecode_fcn_frame ())
      return m_parent_link->get_active_bytecode_call_arg_names ();
    return m_auto_vars.at (avt);
  }

  void set_auto_fcn_var (auto_var_type avt, const octave_value& val)
  {
    m_auto_vars.at (avt) = val;
  }

  // We only need to override one of each of these functions.  The
  // using declaration will avoid warnings about partially-overloaded
  // virtual functions.
  using stack_frame::varval;
  using stack_frame::varref;

  octave_value varval (std::size_t data_offset) const
  {
    return m_values.at (data_offset);
  }

  octave_value& varref (std::size_t data_offset, bool)
  {
    return m_values.at (data_offset);
  }

  void display (bool follow = true) const;

protected:

  // Variable values.  This array is indexed by the data_offset
  // value stored in the symbol_record objects of the scope
  // associated with this stack frame.
  std::vector<octave_value> m_values;

  // The type of each variable (local, global, persistent) of each
  // value.  This array is indexed by the data_offset value stored
  // in the symbol_record objects of the scope associated with this
  // stack frame.  Local values are found in the M_VALUES array.
  // Global values are stored in the tree_evaluator object that contains
  // the stack frame.  Persistent values are stored in the function
  // scope corresponding to the stack frame.
  std::vector<scope_flags> m_flags;

  // A fixed list of Automatic variables created for this function.
  // The elements of this vector correspond to the auto_var_type
  // enum.
  std::vector<octave_value> m_auto_vars;
};

// User-defined functions have a symbol_scope object to store the set
// of variables in the function and values are stored in the stack
// frame corresponding to the invocation of the function or one of
// its parents.  The frame offset tells us how many access links we
// must follow to find the stack frame that holds the value.  The
// value offset is the index into the vector of values in that stack
// frame that we should use to find the value.
//
// Frame and value offsets are determined when the corresponding
// function is parsed.

class user_fcn_stack_frame : public base_value_stack_frame
{
public:

  user_fcn_stack_frame () = delete;

  user_fcn_stack_frame (tree_evaluator& tw, octave_user_function *fcn,
                        std::size_t index,
                        const std::shared_ptr<stack_frame>& parent_link,
                        const std::shared_ptr<stack_frame>& static_link,
                        const std::shared_ptr<stack_frame>& access_link = std::shared_ptr<stack_frame> ())
    : base_value_stack_frame (tw, get_num_symbols (fcn), index,
                              parent_link, static_link,
                              (access_link
                               ? access_link
                               : get_access_link (fcn, static_link))),
      m_fcn (fcn), m_unwind_protect_frame (nullptr)
  { }

  user_fcn_stack_frame (tree_evaluator& tw, octave_user_function *fcn,
                        std::size_t index,
                        const std::shared_ptr<stack_frame>& parent_link,
                        const std::shared_ptr<stack_frame>& static_link,
                        const local_vars_map& local_vars,
                        const std::shared_ptr<stack_frame>& access_link = std::shared_ptr<stack_frame> ())
    : base_value_stack_frame (tw, get_num_symbols (fcn), index,
                              parent_link, static_link,
                              (access_link
                               ? access_link
                               : get_access_link (fcn, static_link))),
      m_fcn (fcn), m_unwind_protect_frame (nullptr)
  {
    // Initialize local variable values.

    for (const auto& nm_ov : local_vars)
      assign (nm_ov.first, nm_ov.second);
  }

  user_fcn_stack_frame (const user_fcn_stack_frame& elt) = default;

  user_fcn_stack_frame&
  operator = (const user_fcn_stack_frame& elt) = delete;

  ~user_fcn_stack_frame ()
  {
    delete m_unwind_protect_frame;
  }

  bool is_user_fcn_frame () const { return true; }

  static std::shared_ptr<stack_frame>
  get_access_link (octave_user_function *fcn,
                   const std::shared_ptr<stack_frame>& static_link);

  static std::size_t get_num_symbols (octave_user_function *fcn)
  {
    symbol_scope fcn_scope = fcn->scope ();

    return fcn_scope.num_symbols ();
  }

  void clear_values ();

  symbol_scope get_scope () const { return m_fcn->scope (); }

  octave_function * function () const { return m_fcn; }

  unwind_protect * unwind_protect_frame ();

  symbol_record lookup_symbol (const std::string& name) const;

  symbol_record insert_symbol (const std::string&);

  scope_flags scope_flag (const symbol_record& sym) const;

  // We only need to override one of each of these functions.  The
  // using declaration will avoid warnings about partially-overloaded
  // virtual functions.
  using base_value_stack_frame::varval;
  using base_value_stack_frame::varref;

  octave_value varval (const symbol_record& sym) const;

  octave_value& varref (const symbol_record& sym, bool deref_refs);

  void mark_scope (const symbol_record& sym, scope_flags flag);

  void display (bool follow = true) const;

  void accept (stack_frame_walker& sfw);

  void break_closure_cycles (const std::shared_ptr<stack_frame>& frame);

private:

  // User-defined object associated with this stack frame.  Should
  // always be valid.
  octave_user_function *m_fcn;

  // The nearest unwind protect frame that was active when this
  // stack frame was created.  Should always be valid.
  unwind_protect *m_unwind_protect_frame;
};

// Pure scope stack frames (primarily the top-level workspace) have
// a set of variables and values are stored in the stack frame.  All
// variable accesses are direct as there are no parent stack frames.
//
// Value offsets are determined when the corresponding variable is
// entered into the symbol_scope object corresponding to the frame.

class scope_stack_frame : public base_value_stack_frame
{
public:

  scope_stack_frame () = delete;

  scope_stack_frame (tree_evaluator& tw, const symbol_scope& scope,
                     std::size_t index,
                     const std::shared_ptr<stack_frame>& parent_link,
                     const std::shared_ptr<stack_frame>& static_link)
    : base_value_stack_frame (tw, scope.num_symbols (), index,
                              parent_link, static_link, nullptr),
      m_scope (scope)
  { }

  scope_stack_frame (const scope_stack_frame& elt) = default;

  scope_stack_frame& operator = (const scope_stack_frame& elt) = delete;

  ~scope_stack_frame () = default;

  bool is_scope_frame () const { return true; }

  symbol_scope get_scope () const { return m_scope; }

  symbol_record lookup_symbol (const std::string& name) const
  {
    return m_scope.lookup_symbol (name);
  }

  symbol_record insert_symbol (const std::string&);

  scope_flags scope_flag (const symbol_record& sym) const;

  // We only need to override one of each of these functions.  The
  // using declaration will avoid warnings about partially-overloaded
  // virtual functions.
  using base_value_stack_frame::varval;
  using base_value_stack_frame::varref;

  octave_value varval (const symbol_record& sym) const;

  octave_value& varref (const symbol_record& sym, bool deref_refs);

  void mark_scope (const symbol_record& sym, scope_flags flag);

  void display (bool follow = true) const;

  void accept (stack_frame_walker& sfw);

private:

  // The scope object associated with this stack frame.
  symbol_scope m_scope;
};

// FIXME: There should probably be a display method for the script,
// fcn, and scope objects and the script and function objects should
// be responsible for displaying the scopes they contain.

static void display_scope (std::ostream& os, const symbol_scope& scope)
{
  if (scope)
    {
      os << "scope: " << scope.name () << std::endl;

      if (scope.num_symbols () > 0)
        {
          os << "name (frame offset, data offset, storage class):"
             << std::endl;

          std::list<symbol_record> symbols = scope.symbol_list ();

          for (auto& sym : symbols)
            {
              os << "  " << sym.name () << " (" << sym.frame_offset ()
                 << ", " << sym.data_offset () << ", " << sym.storage_class ()
                 << ")" << std::endl;
            }
        }
    }
}

class stack_frame_walker
{
protected:

  stack_frame_walker () { }

  virtual ~stack_frame_walker () = default;

public:

  OCTAVE_DISABLE_COPY_MOVE (stack_frame_walker)

  virtual void
  visit_compiled_fcn_stack_frame (compiled_fcn_stack_frame&) = 0;

  virtual void
  visit_script_stack_frame (script_stack_frame&) = 0;

  virtual void
  visit_user_fcn_stack_frame (user_fcn_stack_frame&) = 0;

  virtual void
  visit_scope_stack_frame (scope_stack_frame&) = 0;

  virtual void
  visit_bytecode_fcn_stack_frame (bytecode_fcn_stack_frame&) = 0;
};

class symbol_cleaner : public stack_frame_walker
{
public:

  symbol_cleaner (const std::string& pattern, bool have_regexp = false)
    : stack_frame_walker (), m_patterns (pattern),
      m_clear_all_names (false), m_clear_objects (false),
      m_have_regexp (have_regexp), m_cleared_names ()
  { }

  symbol_cleaner (const string_vector& patterns, bool have_regexp = false)
    : stack_frame_walker (), m_patterns (patterns),
      m_clear_all_names (false), m_clear_objects (false),
      m_have_regexp (have_regexp), m_cleared_names ()
  { }

  symbol_cleaner (bool clear_all_names = true, bool clear_objects = false)
    : stack_frame_walker (), m_patterns (),
      m_clear_all_names (clear_all_names), m_clear_objects (clear_objects),
      m_have_regexp (false), m_cleared_names ()
  { }

  OCTAVE_DISABLE_COPY_MOVE (symbol_cleaner)

  ~symbol_cleaner () = default;

  void visit_compiled_fcn_stack_frame (compiled_fcn_stack_frame& frame)
  {
    // This one follows static link always.  Hmm, should the access
    // link for a compiled_fcn_stack_frame be the same as the static
    // link?

    std::shared_ptr<stack_frame> slink = frame.static_link ();

    if (slink)
      slink->accept (*this);
  }

  void visit_script_stack_frame (script_stack_frame& frame)
  {
    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

  void visit_user_fcn_stack_frame (user_fcn_stack_frame& frame)
  {
    clean_frame (frame);

    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

  void visit_scope_stack_frame (scope_stack_frame& frame)
  {
    clean_frame (frame);

    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

  void visit_bytecode_fcn_stack_frame (bytecode_fcn_stack_frame& frame)
  {
    clean_frame (frame);

    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

private:

  void maybe_clear_symbol (stack_frame& frame, const symbol_record& sym)
  {
    std::string name = sym.name ();

    if (m_cleared_names.find (name) == m_cleared_names.end ())
      {
        // FIXME: Should we check that the name is defined and skip if
        // it is not?  Is it possible for another symbol with the same
        // name to appear in a later stack frame?

        // FIXME: If we are clearing objects and a symbol is found,
        // should we add it to the list of cleared names (since
        // we did find a symbol) but skip clearing the object?

        if (m_clear_objects && ! frame.is_object (sym))
          return;

        m_cleared_names.insert (name);

        frame.clear (sym);
      }
  }

  // FIXME: It would be nice to avoid the duplication in the following
  // function.

  void clear_symbols (stack_frame& frame,
                      const std::list<symbol_record>& symbols)
  {
    if (m_clear_all_names)
      {
        for (const auto& sym : symbols)
          maybe_clear_symbol (frame, sym);
      }
    else if (m_have_regexp)
      {
        octave_idx_type npatterns = m_patterns.numel ();

        for (octave_idx_type j = 0; j < npatterns; j++)
          {
            std::string pattern = m_patterns[j];

            regexp pat (pattern);

            for (const auto& sym : symbols)
              {
                if (pat.is_match (sym.name ()))
                  maybe_clear_symbol (frame, sym);
              }
          }
      }
    else
      {
        octave_idx_type npatterns = m_patterns.numel ();

        for (octave_idx_type j = 0; j < npatterns; j++)
          {
            std::string pattern = m_patterns[j];

            glob_match pat (pattern);

            for (const auto& sym : symbols)
              {
                if (pat.match (sym.name ()))
                  maybe_clear_symbol (frame, sym);
              }
          }
      }
  }

  void clean_frame (stack_frame& frame)
  {
    symbol_scope scope = frame.get_scope ();

    std::list<symbol_record> symbols = scope.symbol_list ();

    if (m_clear_all_names || ! m_patterns.empty ())
      clear_symbols (frame, symbols);
  }

  string_vector m_patterns;

  bool m_clear_all_names;
  bool m_clear_objects;
  bool m_have_regexp;

  std::set<std::string> m_cleared_names;
};

class symbol_info_accumulator : public stack_frame_walker
{
public:

  symbol_info_accumulator (const std::string& pattern,
                           bool have_regexp = false)
    : stack_frame_walker (), m_patterns (pattern), m_match_all (false),
      m_first_only (false), m_have_regexp (have_regexp), m_sym_inf_list (),
      m_found_names ()
  { }

  symbol_info_accumulator (const string_vector& patterns,
                           bool have_regexp = false)
    : stack_frame_walker (), m_patterns (patterns), m_match_all (false),
      m_first_only (false), m_have_regexp (have_regexp), m_sym_inf_list (),
      m_found_names ()
  { }

  symbol_info_accumulator (bool match_all = true, bool first_only = true)
    : stack_frame_walker (), m_patterns (), m_match_all (match_all),
      m_first_only (first_only), m_have_regexp (false),
      m_sym_inf_list (), m_found_names ()
  { }

  OCTAVE_DISABLE_COPY_MOVE (symbol_info_accumulator)

  ~symbol_info_accumulator () = default;

  bool is_empty  () const
  {
    for (const auto& nm_sil : m_sym_inf_list)
      {
        const symbol_info_list& lst = nm_sil.second;

        if (! lst.empty ())
          return false;
      }

    return true;
  }

  std::list<std::string> names () const
  {
    std::list<std::string> retval;

    for (const auto& nm_sil : m_sym_inf_list)
      {
        const symbol_info_list& lst = nm_sil.second;

        std::list<std::string> nm_list = lst.names ();

        for (const auto& nm : nm_list)
          retval.push_back (nm);
      }

    return retval;
  }

  symbol_info_list symbol_info () const
  {
    symbol_info_list retval;

    for (const auto& nm_sil : m_sym_inf_list)
      {
        const symbol_info_list& lst = nm_sil.second;

        for (const auto& syminf : lst)
          retval.push_back (syminf);
      }

    return retval;
  }

  octave_map map_value () const
  {
    octave_map retval;

    // FIXME: is there a better way to concatenate structures?

    std::size_t n_frames = m_sym_inf_list.size ();

    OCTAVE_LOCAL_BUFFER (octave_map, map_list, n_frames);

    std::size_t j = 0;
    for (const auto& nm_sil : m_sym_inf_list)
      {
        std::string scope_name = nm_sil.first;
        const symbol_info_list& lst = nm_sil.second;

        map_list[j] = lst.map_value (scope_name, n_frames-j);

        j++;
      }

    return octave_map::cat (-1, n_frames, map_list);
  }

  void display (std::ostream& os, const std::string& format) const
  {
    for (const auto& nm_sil : m_sym_inf_list)
      {
        os << "\nvariables in scope: " << nm_sil.first << "\n\n";

        const symbol_info_list& lst = nm_sil.second;

        lst.display (os, format);
      }
  }

  void visit_compiled_fcn_stack_frame (compiled_fcn_stack_frame& frame)
  {
    // This one follows static link always.  Hmm, should the access
    // link for a compiled_fcn_stack_frame be the same as the static
    // link?

    std::shared_ptr<stack_frame> slink = frame.static_link ();

    if (slink)
      slink->accept (*this);
  }

  void visit_script_stack_frame (script_stack_frame& frame)
  {
    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

  void visit_user_fcn_stack_frame (user_fcn_stack_frame& frame)
  {
    append_list (frame);

    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

  void visit_scope_stack_frame (scope_stack_frame& frame)
  {
    append_list (frame);

    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

  void visit_bytecode_fcn_stack_frame (bytecode_fcn_stack_frame& frame)
  {
    // For scripts, only collect symbol info in the outer most frame
    if (!frame.is_script_frame ())
      append_list (frame);

    std::shared_ptr<stack_frame> alink = frame.access_link ();

    if (alink)
      alink->accept (*this);
  }

private:

  typedef std::pair<std::string, symbol_info_list> syminf_list_elt;

  // FIXME: the following is too complex and duplicates too much
  // code.  Maybe it should be split up so we have separate classes
  // that do each job that is needed?

  std::list<symbol_record>
  filter (stack_frame& frame, const std::list<symbol_record>& symbols)
  {
    std::list<symbol_record> new_symbols;

    if (m_match_all)
      {
        for (const auto& sym : symbols)
          {
            if (frame.is_defined (sym))
              {
                std::string name = sym.name ();

                if (m_first_only
                    && m_found_names.find (name) != m_found_names.end ())
                  continue;

                m_found_names.insert (name);

                new_symbols.push_back (sym);
              }
          }
      }
    else if (m_have_regexp)
      {
        octave_idx_type npatterns = m_patterns.numel ();

        for (octave_idx_type j = 0; j < npatterns; j++)
          {
            std::string pattern = m_patterns[j];

            regexp pat (pattern);

            for (const auto& sym : symbols)
              {
                std::string name = sym.name ();

                if (pat.is_match (name) && frame.is_defined (sym))
                  {
                    if (m_first_only
                        && m_found_names.find (name) != m_found_names.end ())
                      continue;

                    m_found_names.insert (name);

                    new_symbols.push_back (sym);
                  }
              }
          }
      }
    else
      {
        octave_idx_type npatterns = m_patterns.numel ();

        for (octave_idx_type j = 0; j < npatterns; j++)
          {
            std::string pattern = m_patterns[j];

            glob_match pat (pattern);

            for (const auto& sym : symbols)
              {
                std::string name = sym.name ();

                if (pat.match (name) && frame.is_defined (sym))
                  {
                    if (m_first_only
                        && m_found_names.find (name) == m_found_names.end ())
                      continue;

                    m_found_names.insert (name);

                    new_symbols.push_back (sym);
                  }
              }
          }
      }

    return new_symbols;
  }

  void append_list (stack_frame& frame)
  {
    symbol_scope scope = frame.get_scope ();

    std::list<symbol_record> symbols = scope.symbol_list ();

    if (m_match_all || ! m_patterns.empty ())
      symbols = filter (frame, symbols);

    symbol_info_list syminf_list = frame.make_symbol_info_list (symbols);

    m_sym_inf_list.push_back (syminf_list_elt (scope.name (), syminf_list));
  }

  string_vector m_patterns;

  bool m_match_all;
  bool m_first_only;
  bool m_have_regexp;

  std::list<std::pair<std::string, symbol_info_list>> m_sym_inf_list;

  std::set<std::string> m_found_names;
};

stack_frame *stack_frame::create (tree_evaluator& tw, octave_function *fcn,
                                  std::size_t index,
                                  const std::shared_ptr<stack_frame>& parent_link,
                                  const std::shared_ptr<stack_frame>& static_link)
{
  return new compiled_fcn_stack_frame (tw, fcn, index,
                                       parent_link, static_link);
}

stack_frame *stack_frame::create (tree_evaluator& tw,
                                  octave_user_script *script,
                                  std::size_t index,
                                  const std::shared_ptr<stack_frame>& parent_link,
                                  const std::shared_ptr<stack_frame>& static_link)
{
  return new script_stack_frame (tw, script, index, parent_link, static_link);
}

stack_frame *stack_frame::create (tree_evaluator& tw,
                                  octave_user_function *fcn, std::size_t index,
                                  const std::shared_ptr<stack_frame>& parent_link,
                                  const std::shared_ptr<stack_frame>& static_link,
                                  const std::shared_ptr<stack_frame>& access_link)
{
  return new user_fcn_stack_frame (tw, fcn, index,
                                   parent_link, static_link, access_link);
}

stack_frame *stack_frame::create (tree_evaluator& tw,
                                  octave_user_function *fcn, std::size_t index,
                                  const std::shared_ptr<stack_frame>& parent_link,
                                  const std::shared_ptr<stack_frame>& static_link,
                                  const local_vars_map& local_vars,
                                  const std::shared_ptr<stack_frame>& access_link)
{
  return new user_fcn_stack_frame (tw, fcn, index,
                                   parent_link, static_link, local_vars,
                                   access_link);
}

stack_frame *stack_frame::create (tree_evaluator& tw,
                                  const symbol_scope& scope, std::size_t index,
                                  const std::shared_ptr<stack_frame>& parent_link,
                                  const std::shared_ptr<stack_frame>& static_link)
{
  return new scope_stack_frame (tw, scope, index, parent_link, static_link);
}

std::shared_ptr<stack_frame> stack_frame::create_bytecode (
                   tree_evaluator& tw,
                   octave_user_script *fcn,
                   vm &vm,
                   std::size_t index,
                   const std::shared_ptr<stack_frame>& parent_link,
                   const std::shared_ptr<stack_frame>& static_link,
                   int nargout, int nargin)
{
  auto frame = create_bytecode (tw, static_cast<octave_user_code*> (fcn), vm, index, parent_link, static_link, nargout, nargin);

  std::shared_ptr<stack_frame> eval_frame = static_link;

  while (true)
    {
      if (eval_frame->is_user_script_frame ())
        eval_frame = eval_frame->access_link ();
      else if (eval_frame->is_bytecode_fcn_frame ())
        {
          bytecode_fcn_stack_frame *bcf = static_cast<bytecode_fcn_stack_frame *> (eval_frame.get ());
          if (bcf->m_lazy_data && bcf->m_lazy_data->m_is_script)
            eval_frame = eval_frame->access_link ();
          else
            break;
        }
      else
        break;
    }

  frame->m_access_link = eval_frame;

  return frame;
}

std::shared_ptr<stack_frame> stack_frame::create_bytecode (
                   tree_evaluator& tw,
                   octave_user_code *fcn,
                   vm &vm,
                   std::size_t index,
                   const std::shared_ptr<stack_frame>& parent_link,
                   const std::shared_ptr<stack_frame>& static_link,
                   const std::shared_ptr<stack_frame>& access_link,
                   int nargout, int nargin)
{
  auto frame = create_bytecode (tw, fcn, vm, index, parent_link, static_link, nargout, nargin);
  frame->m_access_link = access_link;

  return frame;
}

std::shared_ptr<stack_frame> stack_frame::create_bytecode (
                   tree_evaluator& tw,
                   octave_user_code *fcn,
                   vm &vm,
                   std::size_t index,
                   const std::shared_ptr<stack_frame>& parent_link,
                   const std::shared_ptr<stack_frame>& static_link,
                   int nargout, int nargin)
{
  // If we have any cached shared_ptr to empty bytecode_fcn_stack_frame objects
  // we use on of those
  if (vm.m_frame_ptr_cache.size ())
    {
      std::shared_ptr<stack_frame> new_frame = std::move (vm.m_frame_ptr_cache.back ());
      vm.m_frame_ptr_cache.pop_back ();

      bytecode_fcn_stack_frame *p = static_cast<bytecode_fcn_stack_frame*> (new_frame.get ());
      // Most objects where cleared when the shared_ptr was put into the cache but call the
      // dtor anyways to be sure.
      p->~bytecode_fcn_stack_frame ();
      // Placement new into the storage managed by the shared_ptr
      new (p) bytecode_fcn_stack_frame (tw, fcn, index, parent_link, static_link, vm, nargout, nargin);

      // The bytecode stackframe needs to know if it needs to save away
      // all the stack variables. So it need to keep track of if it is saved
      // somewhere outsite the VM
      p->m_weak_ptr_to_self = new_frame;

      // For nested frames we need to set the access link to the root function
      if (fcn->is_nested_function ())
        {
          // If the parent have an access link, that should be the root function.
          // E.g. if the current frame we are pushing is a nested nested frame.
          //
          // Otherwise, the parent_link is the root frame.
          //
          if (parent_link->m_access_link)
            new_frame->m_access_link = parent_link->m_access_link;
          else
            new_frame->m_access_link = parent_link;
        }

      return new_frame;
    }
  else
    {
      bytecode_fcn_stack_frame *new_frame_raw
        = new bytecode_fcn_stack_frame (tw, fcn, index, parent_link, static_link,
                                        vm, nargout, nargin);
      std::shared_ptr<stack_frame> new_frame (new_frame_raw);

      // The bytecode stackframe needs to know if it needs to save away
      // all the stack variables. So it need to keep track of if it is saved
      // somewhere outsite the VM
      new_frame_raw->m_weak_ptr_to_self = new_frame;

      // For nested frames we need to set the access link to the root function
      if (fcn->is_nested_function ())
        {
          // If the parent have an access link, that should be the root function.
          // E.g. if the current frame we are pushing is a nested nested frame.
          //
          // Otherwise, the parent_link is the root frame.
          //
          if (parent_link->m_access_link)
            new_frame->m_access_link = parent_link->m_access_link;
          else
            new_frame->m_access_link = parent_link;
        }

      return new_frame;
    }
}

// This function is only implemented and should only be called for
// user_fcn stack frames.  Anything else indicates an error in the
// implementation, but we'll simply warn if that happens.

void stack_frame::clear_values ()
{
  warning ("invalid call to stack_frame::clear_values; please report");
}

symbol_info_list
stack_frame::make_symbol_info_list (const std::list<symbol_record>& symrec_list) const
{
  symbol_info_list symbol_stats;

  for (const auto& sym : symrec_list)
    {
      octave_value value = varval (sym);

      if (! value.is_defined ()
          || (is_user_fcn_frame () && sym.frame_offset () > 0))
        continue;

      symbol_info syminf (sym.name (), value, sym.is_formal (),
                          is_global (sym), is_persistent (sym));

      symbol_stats.append (syminf);
    }

  return symbol_stats;
}

octave_value stack_frame::who (const string_vector& patterns,
                               bool have_regexp, bool return_list,
                               bool verbose, const std::string& whos_line_fmt,
                               const std::string& msg)
{
  symbol_info_accumulator sym_inf_accum (patterns, have_regexp);

  accept (sym_inf_accum);

  if (return_list)
    {
      if (verbose)
        return sym_inf_accum.map_value ();
      else
        return Cell (string_vector (sym_inf_accum.names ()));
    }
  else if (! sym_inf_accum.is_empty ())
    {

      if (msg.empty ())
        octave_stdout << "Variables visible from the current scope:\n";
      else
        octave_stdout << msg;

      if (verbose)
        sym_inf_accum.display (octave_stdout, whos_line_fmt);
      else
        {
          octave_stdout << "\n";
          string_vector names (sym_inf_accum.names ());
          names.list_in_columns (octave_stdout);
        }

      octave_stdout << "\n";
    }

  return octave_value ();
}

// Return first occurrence of variables in current stack frame and any
// parent frames reachable through access links.

symbol_info_list stack_frame::all_variables ()
{
  symbol_info_accumulator sia (true, true);

  accept (sia);

  return sia.symbol_info ();
}

octave_value stack_frame::workspace ()
{
  std::list<octave_scalar_map> ws_list;

  stack_frame *frame = this;

  while (frame)
    {
      symbol_info_list symbols = frame->all_variables ();

      octave_scalar_map ws;

      for (const auto& sym_name : symbols.names ())
        {
          octave_value val = symbols.varval (sym_name);

          if (val.is_defined ())
            ws.assign (sym_name, val);
        }

      ws_list.push_back (ws);

      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  Cell ws_frames (ws_list.size (), 1);

  octave_idx_type i = 0;
  for (const auto& elt : ws_list)
    ws_frames(i++) = elt;

  return ws_frames;
}

// FIXME: Should this function also find any variables in parent
// scopes accessible through access_links?

std::list<std::string> stack_frame::variable_names () const
{
  std::list<std::string> retval;

  symbol_scope scope = get_scope ();

  const std::map<std::string, symbol_record>& symbols = scope.symbols ();

  for (const auto& nm_sr : symbols)
    {
      if (is_variable (nm_sr.second))
        retval.push_back (nm_sr.first);
    }

  retval.sort ();

  return retval;
}

symbol_info_list stack_frame::glob_symbol_info (const std::string& pattern)
{
  symbol_info_accumulator sia (pattern, false);

  accept (sia);

  return sia.symbol_info ();
}

symbol_info_list stack_frame::regexp_symbol_info (const std::string& pattern)
{
  symbol_info_accumulator sia (pattern, true);

  accept (sia);

  return sia.symbol_info ();
}

std::size_t stack_frame::size () const
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

void stack_frame::resize (std::size_t)
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

stack_frame::scope_flags stack_frame::get_scope_flag (std::size_t) const
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

void stack_frame::set_scope_flag (std::size_t, scope_flags)
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

void stack_frame::install_variable (const symbol_record& sym,
                                    const octave_value& value, bool global)
{
  if (global && ! is_global (sym))
    {
      octave_value val = varval (sym);

      if (val.is_defined ())
        {
          std::string nm = sym.name ();

          warning_with_id ("Octave:global-local-conflict",
                           "global: '%s' is defined in the current scope.\n",
                           nm.c_str ());
          warning_with_id ("Octave:global-local-conflict",
                           "global: in a future version, global variables must be declared before use.\n");

          // If the symbol is defined in the local but not the
          // global scope, then use the local value as the
          // initial value.  This value will also override any
          // initializer in the global statement.
          octave_value global_val = m_evaluator.global_varval (nm);

          if (global_val.is_defined ())
            {
              warning_with_id ("Octave:global-local-conflict",
                               "global: global value overrides existing local value");

              clear (sym);
            }
          else
            {
              warning_with_id ("Octave:global-local-conflict",
                               "global: existing local value used to initialize global variable");

              m_evaluator.global_varref (nm) = val;
            }
        }

      mark_global (sym);
    }

  if (value.is_defined ())
    assign (sym, value);
}

octave_value stack_frame::varval (std::size_t) const
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

octave_value& stack_frame::varref (std::size_t, bool)
{
  // This function should only be called for user_fcn_stack_frame or
  // scope_stack_frame objects.  Anything else indicates an error in
  // the implementation.

  panic_impossible ();
}

void stack_frame::clear_objects ()
{
  symbol_cleaner sc (true, true);

  accept (sc);
}

void stack_frame::clear_variable (const std::string& name)
{
  symbol_cleaner sc (name);

  accept (sc);
}

void stack_frame::clear_variable_pattern (const std::string& pattern)
{
  symbol_cleaner sc (pattern);

  accept (sc);
}

void stack_frame::clear_variable_pattern (const string_vector& patterns)
{
  symbol_cleaner sc (patterns);

  accept (sc);
}

void stack_frame::clear_variable_regexp (const std::string& pattern)
{
  symbol_cleaner sc (pattern, true);

  accept (sc);
}

void stack_frame::clear_variable_regexp (const string_vector& patterns)
{
  symbol_cleaner sc (patterns, true);

  accept (sc);
}

void stack_frame::clear_variables ()
{
  symbol_cleaner sc;

  accept (sc);
}

void stack_frame::display_stopped_in_message (std::ostream& os) const
{
  if (index () == 0)
    os << "at top level" << std::endl;
  else
    {
      os << "stopped in " << fcn_name ();

      int l = line ();
      if (l > 0)
        os << " at line " << line ();

      os << " [" << fcn_file_name () << "] " << std::endl;
    }
}

void stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [stack_frame] (" << this << ") --" << std::endl;

  os << "parent link: ";
  if (m_parent_link)
    os << m_parent_link.get ();
  else
    os << "NULL";
  os << std::endl;

  os << "static link: ";
  if (m_static_link)
    os << m_static_link.get ();
  else
    os << "NULL";
  os << std::endl;

  os << "access link: ";
  if (m_access_link)
    os << m_access_link.get ();
  else
    os << "NULL";
  os << std::endl;

  os << "line: " << m_line << std::endl;
  os << "column: " << m_column << std::endl;
  os << "index: " << m_index << std::endl;

  os << std::endl;

  if (! follow)
    return;

  os << "FOLLOWING ACCESS LINKS:" << std::endl;
  std::shared_ptr<stack_frame> frm = access_link ();
  while (frm)
    {
      frm->display (false);
      os << std::endl;

      frm = frm->access_link ();
    }
}

void compiled_fcn_stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [compiled_fcn_stack_frame] (" << this << ") --" << std::endl;
  stack_frame::display (follow);

  os << "fcn: " << m_fcn->name ()
     << " (" << m_fcn->type_name () << ")" << std::endl;
}

void compiled_fcn_stack_frame::accept (stack_frame_walker& sfw)
{
  sfw.visit_compiled_fcn_stack_frame (*this);
}

script_stack_frame::script_stack_frame (tree_evaluator& tw,
                                        octave_user_script *script,
                                        std::size_t index,
                                        const std::shared_ptr<stack_frame>& parent_link,
                                        const std::shared_ptr<stack_frame>& static_link)
  : stack_frame (tw, index, parent_link, static_link,
                 get_access_link (static_link)),
    m_script (script), m_unwind_protect_frame (nullptr),
    m_lexical_frame_offsets (get_num_symbols (script), 1),
    m_value_offsets (get_num_symbols (script), 0)
{
  set_script_offsets ();
}

std::size_t script_stack_frame::get_num_symbols (octave_user_script *script)
{
  symbol_scope script_scope = script->scope ();

  return script_scope.num_symbols ();
}

void script_stack_frame::set_script_offsets ()
{
  // Set frame and data offsets inside stack frame based on enclosing
  // scope(s).

  symbol_scope script_scope = m_script->scope ();

  std::size_t num_script_symbols = script_scope.num_symbols ();

  resize (num_script_symbols);

  const std::map<std::string, symbol_record>& script_symbols
    = script_scope.symbols ();

  set_script_offsets_internal (script_symbols);
}

void script_stack_frame::set_script_offsets_internal
(const std::map<std::string, symbol_record>& script_symbols)
{
  // This scope will be used to evaluate the script.  Find (or
  // possibly insert) symbols from the dummy script scope here.

  symbol_scope eval_scope = m_access_link->get_scope ();

  if (eval_scope.is_nested ())
    {
      bool found = false;

      for (const auto& nm_sr : script_symbols)
        {
          std::string name = nm_sr.first;
          symbol_record script_sr = nm_sr.second;

          symbol_scope parent_scope = eval_scope;

          std::size_t count = 1;

          while (parent_scope)
            {
              const std::map<std::string, symbol_record>& parent_scope_symbols
                = parent_scope.symbols ();

              auto p = parent_scope_symbols.find (name);

              if (p != parent_scope_symbols.end ())
                {
                  found = true;
                  symbol_record parent_scope_sr = p->second;

                  std::size_t script_sr_data_offset = script_sr.data_offset ();

                  m_lexical_frame_offsets.at (script_sr_data_offset)
                    = parent_scope_sr.frame_offset () + count;

                  m_value_offsets.at (script_sr_data_offset)
                    = parent_scope_sr.data_offset ();

                  break;
                }
              else
                {
                  count++;
                  parent_scope = parent_scope.parent_scope ();
                }
            }

          if (! found)
            error ("symbol '%s' cannot be added to static scope",
                   name.c_str ());
        }
    }
  else
    {
      const std::map<std::string, symbol_record>& eval_scope_symbols
        = eval_scope.symbols ();

      for (const auto& nm_sr : script_symbols)
        {
          std::string name = nm_sr.first;
          symbol_record script_sr = nm_sr.second;

          auto p = eval_scope_symbols.find (name);

          symbol_record eval_scope_sr;

          if (p == eval_scope_symbols.end ())
            eval_scope_sr = eval_scope.insert (name);
          else
            eval_scope_sr = p->second;

          std::size_t script_sr_data_offset = script_sr.data_offset ();

          // The +1 is for going from the script frame to the eval
          // frame.  Only one access_link should need to be followed.

          m_lexical_frame_offsets.at (script_sr_data_offset)
            = eval_scope_sr.frame_offset () + 1;

          m_value_offsets.at (script_sr_data_offset)
            = eval_scope_sr.data_offset ();
        }
    }
}

void script_stack_frame::resize_and_update_script_offsets (const symbol_record& sym)
{
  std::size_t data_offset = sym.data_offset ();

  // This function is called when adding new symbols to a script
  // scope.  If the symbol wasn't present before, it should be outside
  // the range so we need to resize then update offsets.

  panic_unless (data_offset >= size ());

  resize (data_offset+1);

  // FIXME: We should be able to avoid creating the map object and the
  // looping in the set_scripts_offsets_internal function.  Can we do
  // that without (or with minimal) code duplication?

  std::map<std::string, symbol_record> tmp_symbols;
  tmp_symbols[sym.name ()] = sym;
  set_script_offsets_internal (tmp_symbols);
}

// If this is a nested scope, set access_link to nearest parent
// stack frame that corresponds to the lexical parent of this scope.

std::shared_ptr<stack_frame>
script_stack_frame::get_access_link (const std::shared_ptr<stack_frame>& static_link)
{
  // If this script is called from another script, set access
  // link to ultimate parent stack frame.

  std::shared_ptr<stack_frame> alink = static_link;

  while (alink->is_user_script_frame ())
    {
      if (alink->access_link ())
        alink = alink->access_link ();
      else
        break;
    }

  return alink;
}

unwind_protect *script_stack_frame::unwind_protect_frame ()
{
  if (! m_unwind_protect_frame)
    m_unwind_protect_frame = new unwind_protect ();

  return m_unwind_protect_frame;
}

symbol_record script_stack_frame::lookup_symbol (const std::string& name) const
{
  symbol_scope scope = get_scope ();

  symbol_record sym = scope.lookup_symbol (name);

  if (sym)
    {
      panic_unless (sym.frame_offset () == 0);

      return sym;
    }

  sym = m_access_link->lookup_symbol (name);

  // Return symbol record with adjusted frame offset.
  symbol_record new_sym = sym.dup ();

  new_sym.set_frame_offset (sym.frame_offset () + 1);

  return new_sym;
}

symbol_record script_stack_frame::insert_symbol (const std::string& name)
{
  // If the symbols is already in the immediate scope, there is
  // nothing more to do.

  symbol_scope scope = get_scope ();

  symbol_record sym = scope.lookup_symbol (name);

  if (sym)
    {
      // All symbol records in a script scope should have zero offset,
      // which means we redirect our lookup using
      // lexical_frame_offsets and values_offets.
      panic_unless (sym.frame_offset () == 0);

      return sym;
    }

  // Insert the symbol in the current scope then resize and update
  // offsets.  This operation should never fail.

  sym = scope.find_symbol (name);

  panic_unless (sym.is_valid ());

  resize_and_update_script_offsets (sym);

  return sym;
}

// Similar to set_script_offsets_internal except that we only return
// frame and data offsets for symbols found by name in parent scopes
// instead of updating the offsets stored in the script frame itself.

bool
script_stack_frame::get_val_offsets_internal (const symbol_record& script_sr,
    std::size_t& frame_offset,
    std::size_t& data_offset) const
{
  bool found = false;

  // This scope will be used to evaluate the script.  Find symbols
  // here by name.

  symbol_scope eval_scope = m_access_link->get_scope ();

  if (eval_scope.is_nested ())
    {
      std::string name = script_sr.name ();

      symbol_scope parent_scope = eval_scope;

      std::size_t count = 1;

      while (parent_scope)
        {
          const std::map<std::string, symbol_record>& parent_scope_symbols
            = parent_scope.symbols ();

          auto p = parent_scope_symbols.find (name);

          if (p != parent_scope_symbols.end ())
            {
              found = true;
              symbol_record parent_scope_sr = p->second;

              frame_offset = parent_scope_sr.frame_offset () + count;

              data_offset = parent_scope_sr.data_offset ();

              break;
            }
          else
            {
              count++;
              parent_scope = parent_scope.parent_scope ();
            }
        }
    }
  else
    {
      const std::map<std::string, symbol_record>& eval_scope_symbols
        = eval_scope.symbols ();

      std::string name = script_sr.name ();

      auto p = eval_scope_symbols.find (name);

      symbol_record eval_scope_sr;

      if (p != eval_scope_symbols.end ())
        {
          found = true;
          eval_scope_sr = p->second;

          // The +1 is for going from the script frame to the eval
          // frame.  Only one access_link should need to be followed.

          frame_offset = eval_scope_sr.frame_offset () + 1;

          data_offset = eval_scope_sr.data_offset ();
        }
    }

  return found;
}

bool script_stack_frame::get_val_offsets (const symbol_record& sym,
    std::size_t& frame_offset,
    std::size_t& data_offset) const
{
  data_offset = sym.data_offset ();
  frame_offset = sym.frame_offset ();

  if (frame_offset == 0)
    {
      // An out of range data_offset value here means that we have a
      // symbol that was not originally in the script.  But this
      // function is called in places where we can't insert a new
      // symbol, so we fail and it is up to the caller to decide what
      // to do.

      if (data_offset >= size ())
        return get_val_offsets_internal (sym, frame_offset, data_offset);

      // Use frame and value offsets stored in this stack frame,
      // indexed by data_offset from the symbol_record to find the
      // values.  These offsets were determined by
      // script_stack_frame::set_script_offsets when this script was
      // invoked.

      frame_offset = m_lexical_frame_offsets.at (data_offset);

      if (frame_offset == 0)
        {
          // If the frame offset stored in m_lexical_frame_offsets is
          // zero, then the data offset in the evaluation scope has
          // not been determined so try to do that now.  The symbol
          // may have been added by eval and without calling
          // resize_and_update_script_offsets.

          return get_val_offsets_internal (sym, frame_offset, data_offset);
        }

      data_offset = m_value_offsets.at (data_offset);
    }
  else
    {
      // If frame_offset is not zero, then then we must have a symbol
      // that was not originally in the script.  The values should
      // have been determined by the script_stack_frame::lookup function.
    }

  return true;
}

void script_stack_frame::get_val_offsets_with_insert (const symbol_record& sym,
    std::size_t& frame_offset,
    std::size_t& data_offset)
{
  data_offset = sym.data_offset ();
  frame_offset = sym.frame_offset ();

  if (frame_offset == 0)
    {
      if (data_offset >= size ())
        {
          // If the data_offset is out of range, then we must have a
          // symbol that was not originally in the script.  Resize and
          // update the offsets.

          resize_and_update_script_offsets (sym);
        }

      // Use frame and value offsets stored in this stack frame,
      // indexed by data_offset from the symbol_record to find the
      // values.  These offsets were determined by
      // script_stack_frame::set_script_offsets when this script was
      // invoked.

      frame_offset = m_lexical_frame_offsets.at (data_offset);

      if (frame_offset == 0)
        {
          // If the frame offset stored in m_lexical_frame_offsets is
          // zero, then the data offset in the evaluation scope has
          // not been determined so try to do that now.  The symbol
          // may have been added by eval and without calling
          // resize_and_update_script_offsets.

          // We don't need to resize here.  That case is handled above.

          // FIXME: We should be able to avoid creating the map object
          // and the looping in the set_scripts_offsets_internal
          // function.  Can we do that without (or with minimal) code
          // duplication?

          std::map<std::string, symbol_record> tmp_symbols;
          tmp_symbols[sym.name ()] = sym;
          set_script_offsets_internal (tmp_symbols);

          // set_script_offsets_internal may have modified
          // m_lexical_frame_offsets and m_value_offsets.

          frame_offset = m_lexical_frame_offsets.at (data_offset);
        }

      data_offset = m_value_offsets.at (data_offset);
    }
  else
    {
      // If frame_offset is not zero, then then we must have a symbol
      // that was not originally in the script.  The values were
      // determined by the script_stack_frame::lookup function.
    }
}

stack_frame::scope_flags
script_stack_frame::scope_flag (const symbol_record& sym) const
{
  std::size_t frame_offset;
  std::size_t data_offset;

  bool found = get_val_offsets (sym, frame_offset, data_offset);

  // It can't be global or persistent, so call it local.
  if (! found)
    return LOCAL;

  // Follow frame_offset access links to stack frame that holds
  // the value.

  const stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (! frame)
    error ("internal error: invalid access link in function call stack");

  if (data_offset >= frame->size ())
    return LOCAL;

  return frame->get_scope_flag (data_offset);
}

octave_value script_stack_frame::varval (const symbol_record& sym) const
{
  std::size_t frame_offset;
  std::size_t data_offset;

  bool found = get_val_offsets (sym, frame_offset, data_offset);

  if (! found)
    return octave_value ();

  // Follow frame_offset access links to stack frame that holds
  // the value.

  const stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (! frame)
    error ("internal error: invalid access link in function call stack");

  if (data_offset >= frame->size ())
    return octave_value ();

  switch (frame->get_scope_flag (data_offset))
    {
    case LOCAL:
      return frame->varval (data_offset);

    case PERSISTENT:
      {
        symbol_scope scope = frame->get_scope ();

        return scope.persistent_varval (data_offset);
      }

    case GLOBAL:
      return m_evaluator.global_varval (sym.name ());
    }

  error ("internal error: invalid switch case");
}

octave_value& script_stack_frame::varref (const symbol_record& sym, bool deref_refs)
{
  std::size_t frame_offset;
  std::size_t data_offset;
  get_val_offsets_with_insert (sym, frame_offset, data_offset);

  // Follow frame_offset access links to stack frame that holds
  // the value.

  stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (data_offset >= frame->size ())
    frame->resize (data_offset+1);

  switch (frame->get_scope_flag (data_offset))
    {
    case LOCAL:
      return frame->varref (data_offset, deref_refs);

    case PERSISTENT:
      {
        symbol_scope scope = frame->get_scope ();

        return scope.persistent_varref (data_offset);
      }

    case GLOBAL:
      return m_evaluator.global_varref (sym.name ());
    }

  error ("internal error: invalid switch case");
}

void script_stack_frame::mark_scope (const symbol_record& sym,
                                     scope_flags flag)
{
  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    resize_and_update_script_offsets (sym);

  // Redirection to evaluation context for the script.

  std::size_t frame_offset = m_lexical_frame_offsets.at (data_offset);
  data_offset = m_value_offsets.at (data_offset);

  if (frame_offset > 1)
    error ("variables must be made PERSISTENT or GLOBAL in the first scope in which they are used");

  std::shared_ptr<stack_frame> frame = access_link ();

  if (data_offset >= frame->size ())
    frame->resize (data_offset+1);

  frame->set_scope_flag (data_offset, flag);
}

void script_stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [script_stack_frame] (" << this << ") --" << std::endl;
  stack_frame::display (follow);

  os << "script: " << m_script->name ()
     << " (" << m_script->type_name () << ")" << std::endl;

  os << "lexical_offsets: " << m_lexical_frame_offsets.size ()
     << " elements:";

  for (std::size_t i = 0; i < m_lexical_frame_offsets.size (); i++)
    os << "  " << m_lexical_frame_offsets.at (i);
  os << std::endl;

  os << "value_offsets: " << m_value_offsets.size () << " elements:";
  for (std::size_t i = 0; i < m_value_offsets.size (); i++)
    os << "  " << m_value_offsets.at (i);
  os << std::endl;

  display_scope (os, get_scope ());
}

void script_stack_frame::accept (stack_frame_walker& sfw)
{
  sfw.visit_script_stack_frame (*this);
}

void base_value_stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [base_value_stack_frame] (" << this << ") --" << std::endl;
  stack_frame::display (follow);

  os << "values: " << m_values.size ()
     << " elements (idx, scope flag, type):" << std::endl;

  for (std::size_t i = 0; i < m_values.size (); i++)
    {
      os << "  (" << i << ", " << m_flags.at (i) << ", ";

      octave_value val = varval (i);

      os << (val.is_defined () ? val.type_name () : " UNDEFINED") << ")"
         << std::endl;
    }
}

// If this is a nested scope, set access_link to nearest parent
// stack frame that corresponds to the lexical parent of this scope.

std::shared_ptr<stack_frame>
user_fcn_stack_frame::get_access_link (octave_user_function *fcn,
                                       const std::shared_ptr<stack_frame>& static_link)
{
  std::shared_ptr<stack_frame> alink;

  symbol_scope fcn_scope = fcn->scope ();

  if (fcn_scope.is_nested ())
    {
      if (! static_link)
        error ("internal call stack error (invalid static link)");

      symbol_scope caller_scope = static_link->get_scope ();

      int nesting_depth = fcn_scope.nesting_depth ();
      int caller_nesting_depth = caller_scope.nesting_depth ();

      if (caller_nesting_depth < nesting_depth)
        {
          // FIXME: do we need to ensure that the called
          // function is a child of the caller?  Does it hurt
          // to panic_unless this condition, at least for now?

          alink = static_link;
        }
      else
        {
          // FIXME: do we need to check that the parent of the
          // called function is also a parent of the caller?
          // Does it hurt to panic_unless this condition, at least
          // for now?

          int links_to_follow = caller_nesting_depth - nesting_depth + 1;

          alink = static_link;

          while (alink && --links_to_follow >= 0)
            alink = alink->access_link ();

          if (! alink)
            error ("internal function nesting error (invalid access link)");
        }
    }

  return alink;
}

void user_fcn_stack_frame::clear_values ()
{
  symbol_scope fcn_scope = m_fcn->scope ();

  const std::list<symbol_record>& symbols = fcn_scope.symbol_list ();

  if (size () == 0)
    return;

  for (const auto& sym : symbols)
    {
      std::size_t frame_offset = sym.frame_offset ();

      if (frame_offset > 0)
        continue;

      std::size_t data_offset = sym.data_offset ();

      if (data_offset >= size ())
        continue;

      if (get_scope_flag (data_offset) == LOCAL)
        {
          octave_value& ref = m_values.at (data_offset);

          if (ref.get_count () == 1)
            {
              ref.call_object_destructor ();
              ref = octave_value ();
            }
        }
    }
}

unwind_protect *user_fcn_stack_frame::unwind_protect_frame ()
{
  if (! m_unwind_protect_frame)
    m_unwind_protect_frame = new unwind_protect ();

  return m_unwind_protect_frame;
}

symbol_record user_fcn_stack_frame::lookup_symbol (const std::string& name) const
{
  const stack_frame *frame = this;

  while (frame)
    {
      symbol_scope scope = frame->get_scope ();

      symbol_record sym = scope.lookup_symbol (name);

      if (sym)
        return sym;

      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  return symbol_record ();
}

symbol_record user_fcn_stack_frame::insert_symbol (const std::string& name)
{
  // If the symbols is already in the immediate scope, there is
  // nothing more to do.

  symbol_scope scope = get_scope ();

  symbol_record sym = scope.lookup_symbol (name);

  if (sym)
    return sym;

  // FIXME: This needs some thought... We may need to add a symbol to
  // a static workspace, but the symbol can never be defined as a
  // variable.  This currently works by tagging the added symbol as
  // "added_static".  Aside from the bad name, this doesn't seem like
  // the best solution.  Maybe scopes should have a separate set of
  // symbols that may only be defined as functions?

  // Insert the symbol in the current scope.  This is not possible for
  // anonymous functions, nested functions, or functions that contain
  // nested functions (their scopes will all be marked static).

  //    if (scope.is_static ())
  //      error ("can not add variable '%s' to a static workspace",
  //             name.c_str ());

  // At this point, non-local references are not possible so we only
  // need to look in the current scope and insert there.  This
  // operation should never fail.

  sym = scope.find_symbol (name);

  panic_unless (sym.is_valid ());

  return sym;
}

stack_frame::scope_flags
user_fcn_stack_frame::scope_flag (const symbol_record& sym) const
{
  std::size_t frame_offset = sym.frame_offset ();
  std::size_t data_offset = sym.data_offset ();

  // Follow frame_offset access links to stack frame that holds
  // the value.

  const stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (! frame)
    error ("internal error: invalid access link in function call stack");

  if (data_offset >= frame->size ())
    return LOCAL;

  return frame->get_scope_flag (data_offset);
}

octave_value user_fcn_stack_frame::varval (const symbol_record& sym) const
{
  std::size_t frame_offset = sym.frame_offset ();
  std::size_t data_offset = sym.data_offset ();

  // Follow frame_offset access links to stack frame that holds
  // the value.

  const stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (! frame)
    error ("internal error: invalid access link in function call stack");

  if (data_offset >= frame->size ())
    return octave_value ();

  switch (frame->get_scope_flag (data_offset))
    {
    case LOCAL:
      return frame->varval (data_offset);

    case PERSISTENT:
      {
        symbol_scope scope = frame->get_scope ();

        return scope.persistent_varval (data_offset);
      }

    case GLOBAL:
      return m_evaluator.global_varval (sym.name ());
    }

  error ("internal error: invalid switch case");
}

octave_value& user_fcn_stack_frame::varref (const symbol_record& sym, bool deref_refs)
{
  std::size_t frame_offset = sym.frame_offset ();
  std::size_t data_offset = sym.data_offset ();

  // Follow frame_offset access links to stack frame that holds
  // the value.

  stack_frame *frame = this;

  for (std::size_t i = 0; i < frame_offset; i++)
    {
      std::shared_ptr<stack_frame> nxt = frame->access_link ();
      frame = nxt.get ();
    }

  if (data_offset >= frame->size ())
    frame->resize (data_offset+1);

  switch (frame->get_scope_flag (data_offset))
    {
    case LOCAL:
      return frame->varref (data_offset, deref_refs);

    case PERSISTENT:
      {
        symbol_scope scope = frame->get_scope ();

        return scope.persistent_varref (data_offset);
      }

    case GLOBAL:
      return m_evaluator.global_varref (sym.name ());
    }

  error ("internal error: invalid switch case");
}

void user_fcn_stack_frame::mark_scope (const symbol_record& sym, scope_flags flag)
{
  std::size_t frame_offset = sym.frame_offset ();

  if (frame_offset > 0 && (flag == PERSISTENT || flag == GLOBAL))
    error ("variables must be made PERSISTENT or GLOBAL in the first scope in which they are used");

  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    resize (data_offset+1);

  set_scope_flag (data_offset, flag);
}

void bytecode_fcn_stack_frame::display (bool) const
{
  std::ostream& os = octave_stdout;

  os << "-- [bytecode_fcn_stack_frame] (" << this << ") --" << std::endl;

  os << "fcn: " << m_fcn->name ()
     << " (" << m_fcn->type_name () << ")" << std::endl;

  display_scope (os, get_scope ());
}

void user_fcn_stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [user_fcn_stack_frame] (" << this << ") --" << std::endl;
  base_value_stack_frame::display (follow);

  os << "fcn: " << m_fcn->name ()
     << " (" << m_fcn->type_name () << ")" << std::endl;

  display_scope (os, get_scope ());
}

void user_fcn_stack_frame::accept (stack_frame_walker& sfw)
{
  sfw.visit_user_fcn_stack_frame (*this);
}

void user_fcn_stack_frame::break_closure_cycles (const std::shared_ptr<stack_frame>& frame)
{
  for (auto& val : m_values)
    val.break_closure_cycles (frame);

  if (m_access_link)
    m_access_link->break_closure_cycles (frame);
}

symbol_record scope_stack_frame::insert_symbol (const std::string& name)
{
  // There is no access link for scope frames, so there is no other
  // frame to search in and the offset must be zero.

  symbol_record sym = m_scope.lookup_symbol (name);

  if (sym)
    return sym;

  // If the symbol is not found, insert it.  We only need to search in
  // the local scope object.  This operation should never fail.

  sym = m_scope.find_symbol (name);

  panic_unless (sym.is_valid ());

  return sym;
}

stack_frame::scope_flags
scope_stack_frame::scope_flag (const symbol_record& sym) const
{
  // There is no access link for scope frames, so the frame
  // offset must be zero.

  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    return LOCAL;

  return get_scope_flag (data_offset);
}

octave_value scope_stack_frame::varval (const symbol_record& sym) const
{
  // There is no access link for scope frames, so the frame
  // offset must be zero.

  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    return octave_value ();

  switch (get_scope_flag (data_offset))
    {
    case LOCAL:
      {
        octave_value ov = m_values.at (data_offset);
        if (ov.is_ref ())
          return ov.ref_rep ()->deref ();
        else
          return ov;
      }
    case PERSISTENT:
      return m_scope.persistent_varval (data_offset);

    case GLOBAL:
      return m_evaluator.global_varval (sym.name ());
    }

  error ("internal error: invalid switch case");
}

octave_value& scope_stack_frame::varref (const symbol_record& sym, bool deref_refs)
{
  // There is no access link for scope frames, so the frame
  // offset must be zero.

  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    resize (data_offset+1);

  switch (get_scope_flag (data_offset))
    {
    case LOCAL:
      {
        octave_value &ov = m_values.at (data_offset);
        if (deref_refs && ov.is_ref ())
          return ov.ref_rep ()->ref ();
        else
          return ov;
      }
    case PERSISTENT:
      return m_scope.persistent_varref (data_offset);

    case GLOBAL:
      return m_evaluator.global_varref (sym.name ());
    }

  error ("internal error: invalid switch case");
}

void scope_stack_frame::mark_scope (const symbol_record& sym,
                                    scope_flags flag)
{
  // There is no access link for scope frames, so the frame
  // offset must be zero.

  std::size_t data_offset = sym.data_offset ();

  if (data_offset >= size ())
    resize (data_offset+1);

  set_scope_flag (data_offset, flag);
}

void scope_stack_frame::display (bool follow) const
{
  std::ostream& os = octave_stdout;

  os << "-- [scope_stack_frame] (" << this << ") --" << std::endl;
  base_value_stack_frame::display (follow);

  display_scope (os, m_scope);
}

void scope_stack_frame::accept (stack_frame_walker& sfw)
{
  sfw.visit_scope_stack_frame (*this);
}

void bytecode_fcn_stack_frame::accept (stack_frame_walker& sfw)
{
  sfw.visit_bytecode_fcn_stack_frame (*this);
}


OCTAVE_END_NAMESPACE(octave)
