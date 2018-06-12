/*

Copyright (C) 1996-2018 John W. Eaton

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "oct-shlib.h"

#include "call-stack.h"
#include "defaults.h"
#include "dynamic-ld.h"
#include "error.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "ov-mex-fcn.h"
#include "ov.h"
#include "ovl.h"
#include "profiler.h"
#include "unwind-prot.h"


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_mex_function,
                                     "mex function", "mex function");

octave_mex_function::octave_mex_function
  (void *fptr, bool fmex, const octave::dynamic_library& shl,
   const std::string& nm)
  : octave_function (nm), m_mex_fcn_ptr (fptr), m_exit_fcn_ptr (nullptr),
    m_is_fmex (fmex), m_sh_lib (shl)
{
  mark_fcn_file_up_to_date (time_parsed ());

  std::string file_name = fcn_file_name ();

  std::string oct_file_dir = octave::config::oct_file_dir ();
  m_is_system_fcn_file
    = (! file_name.empty ()
       && oct_file_dir == file_name.substr (0, oct_file_dir.length ()));
}

octave_mex_function::~octave_mex_function (void)
{
  if (m_exit_fcn_ptr)
    (*m_exit_fcn_ptr) ();

  octave::dynamic_loader& dyn_loader
    = octave::__get_dynamic_loader__ ("~octave_mex_function");

  dyn_loader.remove_mex (my_name, m_sh_lib);
}

std::string
octave_mex_function::fcn_file_name (void) const
{
  return m_sh_lib.file_name ();
}

octave::sys::time
octave_mex_function::time_parsed (void) const
{
  return m_sh_lib.time_loaded ();
}

// FIXME: shouldn't this declaration be a header file somewhere?
extern octave_value_list
call_mex (octave_mex_function& curr_mex_fcn, const octave_value_list& args,
          int nargout);

octave_value_list
octave_mex_function::call (octave::tree_evaluator& tw, int nargout,
                           const octave_value_list& args)
{
  octave_value_list retval;

  if (args.has_magic_colon ())
    error ("invalid use of colon in function argument list");

  octave::unwind_protect frame;

  octave::call_stack& cs
    = octave::__get_call_stack__ ("octave_mex_function::call");

  cs.push (this);

  frame.add_method (cs, &octave::call_stack::pop);

  octave::profiler& profiler = tw.get_profiler ();

  octave::profiler::enter<octave_mex_function> block (profiler, *this);

  retval = call_mex (*this, args, nargout);

  return retval;
}
