////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

#include "defun.h"
#include "event-manager.h"
#include "interpreter.h"
#include "oct-time.h"
#include "ov-struct.h"
#include "pager.h"
#include "profiler.h"

OCTAVE_BEGIN_NAMESPACE(octave)

profiler::stats::stats (void)
  : m_time (0.0), m_calls (0), m_recursive (false),
    m_parents (), m_children ()
{ }

octave_value
profiler::stats::function_set_value (const function_set& list)
{
  const octave_idx_type n = list.size ();

  RowVector retval (n);
  octave_idx_type i = 0;
  for (const auto& nm : list)
    retval(i++) = nm;

  return retval;
}

profiler::tree_node::tree_node (tree_node *p, octave_idx_type f)
  : m_parent (p), m_fcn_id (f), m_children (), m_time (0.0), m_calls (0)
{ }

profiler::tree_node::~tree_node ()
{
  for (auto& idx_tnode : m_children)
    delete idx_tnode.second;
}

profiler::tree_node *
profiler::tree_node::enter (octave_idx_type fcn)
{
  tree_node *retval;

  child_map::iterator pos = m_children.find (fcn);
  if (pos == m_children.end ())
    {
      retval = new tree_node (this, fcn);
      m_children[fcn] = retval;
    }
  else
    retval = pos->second;

  ++retval->m_calls;
  return retval;
}

profiler::tree_node *
profiler::tree_node::exit (octave_idx_type /* fcn */)
{
  // FIXME: These panic_unless statements don't make sense if profile() is
  //  called from within a function hierarchy to begin with.  See bug #39587.
  //  panic_unless (m_parent);
  //  panic_unless (m_fcn_id == fcn);

  return m_parent;
}

void
profiler::tree_node::build_flat (flat_profile& data) const
{
  // If this is not the top-level node,
  // update profile entry for this function.
  if (m_fcn_id != 0)
    {
      stats& entry = data[m_fcn_id - 1];

      entry.m_time += m_time;
      entry.m_calls += m_calls;

      panic_unless (m_parent);
      if (m_parent->m_fcn_id != 0)
        {
          entry.m_parents.insert (m_parent->m_fcn_id);
          data[m_parent->m_fcn_id - 1].m_children.insert (m_fcn_id);
        }

      if (! entry.m_recursive)
        for (const tree_node *i = m_parent; i; i = i->m_parent)
          if (i->m_fcn_id == m_fcn_id)
            {
              entry.m_recursive = true;
              break;
            }
    }

  // Recurse on children.
  for (const auto& idx_tnode : m_children)
    idx_tnode.second->build_flat (data);
}

octave_value
profiler::tree_node::get_hierarchical (double *total) const
{
  // Note that we don't generate the entry just for this node, but
  // rather a struct-array with entries for all children.  This way, the
  // top-node (for which we don't want a real entry) generates already
  // the final hierarchical profile data.

  const octave_idx_type n = m_children.size ();

  Cell rv_indices (n, 1);
  Cell rv_times (n, 1);
  Cell rv_totals (n, 1);
  Cell rv_calls (n, 1);
  Cell rv_children (n, 1);

  octave_idx_type i = 0;
  for (const auto& idx_tnode : m_children)
    {
      const tree_node& entry = *idx_tnode.second;
      double child_total = entry.m_time;

      rv_indices(i) = octave_value (idx_tnode.first);
      rv_times(i) = octave_value (entry.m_time);
      rv_calls(i) = octave_value (entry.m_calls);
      rv_children(i) = entry.get_hierarchical (&child_total);
      rv_totals(i) = octave_value (child_total);

      if (total)
        *total += child_total;

      ++i;
    }

  octave_map retval;

  retval.assign ("Index", rv_indices);
  retval.assign ("SelfTime", rv_times);
  retval.assign ("TotalTime", rv_totals);
  retval.assign ("NumCalls", rv_calls);
  retval.assign ("Children", rv_children);

  return retval;
}

profiler::profiler (void)
  : m_known_functions (), m_fcn_index (),
    m_enabled (false), m_call_tree (new tree_node (nullptr, 0)),
    m_active_fcn (nullptr), m_last_time (-1.0)
{ }

profiler::~profiler (void)
{
  delete m_call_tree;
}

void
profiler::set_active (bool value)
{
  m_enabled = value;
}

void
profiler::enter_function (const std::string& fcn)
{
  // The enter class will check and only call us if the profiler is active.
  panic_unless (enabled ());
  panic_unless (m_call_tree);

  // If there is already an active function, add to its time before
  // pushing the new one.
  if (m_active_fcn && m_active_fcn != m_call_tree)
    add_current_time ();

  // Map the function's name to its index.
  octave_idx_type fcn_idx;
  fcn_index_map::iterator pos = m_fcn_index.find (fcn);
  if (pos == m_fcn_index.end ())
    {
      m_known_functions.push_back (fcn);
      fcn_idx = m_known_functions.size ();
      m_fcn_index[fcn] = fcn_idx;
    }
  else
    fcn_idx = pos->second;

  if (! m_active_fcn)
    m_active_fcn = m_call_tree;

  m_active_fcn = m_active_fcn->enter (fcn_idx);

  m_last_time = query_time ();

}

void
profiler::exit_function (const std::string& fcn)
{
  if (m_active_fcn)
    {
      panic_unless (m_call_tree);
      // FIXME: This panic_unless statements doesn't make sense if profile()
      //        is called from within a function hierarchy to begin with.
      //        See bug #39587.
      // panic_unless (m_active_fcn != m_call_tree);

      // Usually, if we are disabled this function is not even called.  But
      // the call disabling the profiler is an exception.  So also check here
      // and only record the time if enabled.
      if (enabled ())
        add_current_time ();

      fcn_index_map::iterator pos = m_fcn_index.find (fcn);
      // FIXME: This panic_unless statements doesn't make sense if profile()
      //        is called from within a function hierarchy to begin with.
      //        See bug #39587.
      // panic_unless (pos != m_fcn_index.end ());
      m_active_fcn = m_active_fcn->exit (pos->second);

      // If this was an "inner call", we resume executing the parent function
      // up the stack.  So note the start-time for this!
      m_last_time = query_time ();
    }
}

void
profiler::reset (void)
{
  if (enabled ())
    error ("profile: can't reset active profiler");

  m_known_functions.clear ();
  m_fcn_index.clear ();

  if (m_call_tree)
    {
      delete m_call_tree;
      m_call_tree = new tree_node (nullptr, 0);
      m_active_fcn = nullptr;
    }

  m_last_time = -1.0;
}

octave_value
profiler::get_flat (void) const
{
  octave_value retval;

  const octave_idx_type n = m_known_functions.size ();

  flat_profile flat (n);

  if (m_call_tree)
    {
      m_call_tree->build_flat (flat);

      Cell rv_names (n, 1);
      Cell rv_times (n, 1);
      Cell rv_calls (n, 1);
      Cell rv_recursive (n, 1);
      Cell rv_parents (n, 1);
      Cell rv_children (n, 1);

      for (octave_idx_type i = 0; i != n; ++i)
        {
          rv_names(i) = octave_value (m_known_functions[i]);
          rv_times(i) = octave_value (flat[i].m_time);
          rv_calls(i) = octave_value (flat[i].m_calls);
          rv_recursive(i) = octave_value (flat[i].m_recursive);
          rv_parents(i) = stats::function_set_value (flat[i].m_parents);
          rv_children(i) = stats::function_set_value (flat[i].m_children);
        }

      octave_map m;

      m.assign ("FunctionName", rv_names);
      m.assign ("TotalTime", rv_times);
      m.assign ("NumCalls", rv_calls);
      m.assign ("IsRecursive", rv_recursive);
      m.assign ("Parents", rv_parents);
      m.assign ("Children", rv_children);

      retval = m;
    }
  else
    {
      static const char *fn[] =
      {
        "FunctionName",
        "TotalTime",
        "NumCalls",
        "IsRecursive",
        "Parents",
        "Children",
        nullptr
      };

      static octave_map m (dim_vector (0, 1), string_vector (fn));

      retval = m;
    }

  return retval;
}

octave_value
profiler::get_hierarchical (void) const
{
  octave_value retval;

  if (m_call_tree)
    retval = m_call_tree->get_hierarchical ();
  else
    {
      static const char *fn[] =
      {
        "Index",
        "SelfTime",
        "NumCalls",
        "Children",
        nullptr
      };

      static octave_map m (dim_vector (0, 1), string_vector (fn));

      retval = m;
    }

  return retval;
}

double
profiler::query_time (void) const
{
  sys::time now;

  // FIXME: is this volatile declaration really needed?
  // See bug #34210 for additional details.
  volatile double dnow = now.double_value ();

  return dnow;
}

void
profiler::add_current_time (void)
{
  if (m_active_fcn)
    {
      const double t = query_time ();

      m_active_fcn->add_time (t - m_last_time);
    }
}

// Enable or disable the profiler data collection.
DEFMETHOD (__profiler_enable__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{state} =} __profiler_enable__ ()
Undocumented internal function.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  profiler& profiler = interp.get_profiler ();

  if (nargin == 1)
    {
      profiler.set_active (args(0).bool_value ());

      std::string status = "off";
      if (args(0).bool_value ())
        status = "on";

      event_manager& evmgr = interp.get_event_manager ();
      evmgr.gui_status_update ("profiler", status);  // tell GUI
    }

  return ovl (profiler.enabled ());
}

// Clear all collected profiling data.
DEFMETHOD (__profiler_reset__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __profiler_reset__ ()
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  profiler& profiler = interp.get_profiler ();

  profiler.reset ();

  return ovl ();
}

// Query the timings collected by the profiler.
DEFMETHOD (__profiler_data__, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{data} =} __profiler_data__ ()
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  profiler& profiler = interp.get_profiler ();

  if (nargout > 1)
    return ovl (profiler.get_flat (), profiler.get_hierarchical ());
  else
    return ovl (profiler.get_flat ());
}

OCTAVE_END_NAMESPACE(octave)
