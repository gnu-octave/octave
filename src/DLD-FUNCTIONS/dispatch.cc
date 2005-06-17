/*

Copyright (C) 2001 John W. Eaton and Paul Kienzle

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <list>
#include <map>
#include <string>

#include "defun-dld.h"
#include "ov.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"
#include "pager.h"
#include "parse.h"
#include "symtab.h"
#include "variables.h"

// XXX FIXME XXX should be using a map from type_id->name, rather
// than type_name->name

template class std::map<std::string,std::string>;

typedef std::map<std::string,std::string> Table;

class
octave_dispatch : public octave_function
{
public:

  // XXX FIXME XXX need to handle doc strings of dispatched functions, for
  // example, by appending "for <f>(<type>,...) see <name>" for each
  // time dispatch(f,type,name) is called.
  octave_dispatch (const std::string &nm)
    : octave_function (nm, "Overloaded function"), tab (), base (nm),
      has_alias (false)
  { }

  // XXX FIXME XXX if we get deleted, we should restore the original
  // symbol_record from base before dying.
  ~octave_dispatch (void) { }

  bool is_builtin_function (void) const { return true; }

  octave_function *function_value (bool) { return this; }

  octave_value do_index_op (const octave_value_list&, int)
  {
    error ("dispatch: do_index_op");
    return octave_value ();
  }

  octave_value subsref (const std::string&,
			const std::list<octave_value_list>&)
  {
    error ("dispatch: subsref (str, list)");
    panic_impossible ();
    return octave_value ();
  }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout);

  octave_value_list do_multi_index_op (int, const octave_value_list&);

  void add (const std::string t, const std::string n);

  void clear (const std::string t);

  void print (std::ostream& os, bool pr_as_read=false) const;

private:

  Table tab;
  std::string base;
  bool has_alias;

  octave_dispatch (void) 
    : octave_function (), tab (), base (), has_alias (false) { }

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

  DECLARE_OCTAVE_ALLOCATOR
};

DEFINE_OCTAVE_ALLOCATOR (octave_dispatch);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_dispatch,
				     "overloaded function", "function");

void 
octave_dispatch::add (const std::string t, const std::string n)
{ 
  if (tab.count (t) > 0 && tab[t] != n)
    warning ("replacing %s(%s,...)->%s with %s",
	     base.c_str (), t.c_str (), tab[t].c_str (), n.c_str ());

  tab[t] = n;

  if (t == "any")
    has_alias = true;
}

void
octave_dispatch::clear (const std::string t)
{
  tab.erase (t); 

  if (t == "any")
    has_alias = false;
}

octave_value_list
octave_dispatch::subsref (const std::string& type,
			  const std::list<octave_value_list>& idx,
			  int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      retval = do_multi_index_op (nargout, idx.front ());
      break;

    case '{':
    case '.':
      {
	const std::string nm = type_name ();
	error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  if (idx.size () > 1)
    retval = retval(0).next_subsref (type, idx);

  return retval;
}

static octave_function*
builtin (const std::string& base)
{
  octave_function *fcn = 0;

  // Check if we are overriding a builtin function.  This is the
  // case if builtin is protected.
  symbol_record *builtin = fbi_sym_tab->lookup ("builtin:" + base, 0);

  if (! builtin)
    error ("builtin record has gone missing");

  if (error_state)
    return fcn;

  if (builtin->is_read_only ())
    {
      // builtin is read only, so checking for updates is pointless
      if (builtin->is_function ())
        fcn = builtin->def().function_value ();
      else
	error ("builtin %s is not a function", base.c_str ());
    }
  else
    {
      // Check that builtin is up to date.
 
      // Don't try to fight octave's function name handling
      // mechanism.  Instead, move dispatch record out of the way,
      // and restore the builtin to its original name.
      symbol_record *dispatch = fbi_sym_tab->lookup (base, 0);
      if (! dispatch)
	error ("dispatch record has gone missing");

      dispatch->unprotect ();

      fbi_sym_tab->rename (base, "dispatch:" + base);

      fbi_sym_tab->rename ("builtin:" + base, base);

      // check for updates to builtin function; ignore errors that
      // appear (they interfere with renaming), and remove the updated
      // name from the current symbol table.  XXX FIXME XXX check that
      // updating a function updates it in all contexts --- it may be
      // that it is updated only in the current symbol table, and not
      // the caller.  I believe this won't be a problem because the
      // caller will go through the same logic and end up with the
      // newer version.
      fcn = is_valid_function (base, "dispatch", 1);
      int cache_error = error_state;
      error_state = 0;
      curr_sym_tab->clear_function (base);

      // Move the builtin function out of the way and restore the
      // dispatch fuction.
      // XXX FIXME XXX what if builtin wants to protect itself?
      symbol_record *found = fbi_sym_tab->lookup (base, 0);
      bool readonly = found->is_read_only ();
      found->unprotect ();
      fbi_sym_tab->rename (base, "builtin:" + base);
      fbi_sym_tab->rename ("dispatch:" + base, base);
      if (readonly)
	found->protect ();
      dispatch->protect ();

      // remember if there were any errors.
      error_state = cache_error;
    }

  return fcn;
}

static bool
any_arg_is_magic_colon (const octave_value_list& args)
{
  int nargin = args.length ();

  for (int i = 0; i < nargin; i++)
    if (args(i).is_magic_colon ())
      return true;

  return false;
}

octave_value_list
octave_dispatch::do_multi_index_op (int nargout, const octave_value_list& args)
{
  octave_value_list retval;

  if (error_state) return retval;

  if (any_arg_is_magic_colon (args))
    {
      ::error ("invalid use of colon in function argument list");
      return retval;
    }

  // If more than one argument, check if argument template matches any
  // overloaded functions.  Also provide a catch-all '*' type to provide
  // single level pseudo rename and replace functionality.
  if (args.length () > 0 && tab.count (args(0).type_name ()) > 0)
    retval = feval (tab[args(0).type_name()], args, nargout);
  else if (has_alias)
    retval = feval (tab["any"], args, nargout);
  else
    {
      octave_function *fcn = builtin (base);
      if (! error_state && fcn)
        retval = fcn->do_multi_index_op (nargout, args);
    }

  return retval;
}

void 
octave_dispatch::print (std::ostream& os, bool) const
{
  os << "Overloaded function " << base << std::endl;

  for (Table::const_iterator it = tab.begin (); it != tab.end (); it++)
    os << base << "(" << it->first << ",...)->" 
       << it->second << "(" << it->first << ",...)"
       << std::endl;
}

DEFUN_DLD (builtin, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@dots{}]} builtin (@var{f}, @dots{})\n\
Call the base function @var{f} even if @var{f} is overloaded to\n\
some other function for the given type signature.\n\
@end deftypefn\n\
@seealso{dispatch}")
{
  octave_value_list retval; 

  int nargin = args.length ();

  if (nargin > 0)
    {
      const std::string name (args(0).string_value ());
 
      if (error_state)
	return retval;

      symbol_record *sr = fbi_sym_tab->lookup (name, 0);

      if (sr)
	{
	  if (sr->def().type_id () == octave_dispatch::static_type_id ())
	    {
	      octave_function *fcn = builtin (name);

	      if (!error_state && fcn)
		retval = fcn->do_multi_index_op (nargout,
						 args.splice (0, 1, retval));
	    }
	  else
	    retval = feval (name, args, nargout);
	}
      else
	error ("builtin: lookup for symbol `%s' failed", name.c_str ());
    }
  else
    print_usage ("builtin");

  return retval;
}

static void
dispatch_record (const std::string &f, const std::string &n, 
		 const std::string &t)
{
  // find the base function in the symbol table, loading it if it
  // is not already there; if it is already a dispatch, then bonus

  symbol_record *sr = fbi_sym_tab->lookup (f, true);

  if (sr->def().type_id () != octave_dispatch::static_type_id ())
    {
      // Preserve mark_as_command status
      bool iscommand = sr->is_command ();

      // Not an overloaded name, so if only display or clear then we are done
      if (t.empty ())
	return;

      // sr is the base symbol; rename it to keep it safe.  When we need
      // it we will rename it back again.
      if (sr->is_read_only ()) 
        {
          sr->unprotect ();
          fbi_sym_tab->rename (f, "builtin:" + f);
  	  sr = fbi_sym_tab->lookup (f, true);
          sr->protect ();
	}
      else 
        fbi_sym_tab->rename (f, "builtin:" + f);

      // It would be good to hide the builtin:XXX name, but since the
      // new XXX name in the symbol table is set to BUILTIN_FUNCTION,
      // things don't work quite the way we would like.
      // sr->hide ();

      // Problem:  when a function is first called a new record
      // is created for it in the current symbol table, so calling
      // dispatch on a function that has already been called, we
      // should also clear it from all existing symbol tables.
      // This is too much work, so we will only do it for the
      // top level symbol table.  We can't use the clear_function() 
      // method, because it won't clear builtin functions.  Instead 
      // we check if the symbol is a function and clear it then.  This
      // won't properly clear shadowed functions, or functions in
      // other namespaces (such as the current, if called from a
      // function).
      symbol_record *local = top_level_sym_tab->lookup (f, false);
      if (local && local->is_function ())
	local->clear ();

      // Build a new dispatch object based on the function definition
      octave_dispatch *dispatch = new octave_dispatch (f);
  
      // Create a symbol record for the dispatch object.
      sr = fbi_sym_tab->lookup (f, true);
      sr->unprotect ();
      sr->define (octave_value (dispatch), symbol_record::BUILTIN_FUNCTION); 
      // std::cout << "iscommand('"<<f<<"')=" << iscommand << std::endl;
      if (iscommand)
	sr->mark_as_command();
      sr->document ("\n\n@noindent\nOverloaded function:\n");
      sr->make_eternal (); // XXX FIXME XXX why??
      sr->mark_as_static ();
      sr->protect ();
    }

  // clear/replace/extend the map with the new type-function pair
  const octave_dispatch& rep
    = reinterpret_cast<const octave_dispatch&> (sr->def().get_rep ());

  if (t.empty ())
    // XXX FIXME XXX should return the list if nargout > 1
    rep.print (octave_stdout);
  else if (n.empty ())
    {
      // XXX FIXME XXX should we eliminate the dispatch function if
      // there are no more elements?
      // XXX FIXME XXX should clear the " $t:\w+" from the help string.
      // XXX FIXME XXX -- seems bad to cast away const here...
      octave_dispatch& xrep = const_cast<octave_dispatch&> (rep);

      xrep.clear (t);
    }
  else
    {
      // XXX FIXME XXX -- seems bad to cast away const here...
      octave_dispatch& xrep = const_cast<octave_dispatch&> (rep);

      xrep.add (t, n);

      if (! sr->help().empty ())
	sr->document (sr->help() + "\n" + n + " (" + t + ", ...)\n");
    }
}

DEFUN_DLD (dispatch, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dispatch (@var{f}, @var{r}, @var{type})\n\
\n\
Replace the function @var{f} with a dispatch so that function @var{r}\n\
is called when @var{f} is called with the first argument of the named\n\
@var{type}. If the type is @var{any} then call @var{r} if no other type\n\
matches.  The original function @var{f} is accessible using\n\
@code{builtin (@var{f}, @dots{}).\n\
\n\
If @var{r} is omitted, clear dispatch function associated with @var{type}.\n\
\n\
If both @var{r} and @var{type} are omitted, list dispatch functions\n\
for @var{f}\n\
@end deftypefn\n\
@seealso{builtin}") 
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    {
      print_usage ("dispatch");
      return retval;
    }

  std::string f, t, n;
  if (nargin > 0)
    f = args(0).string_value ();

  if (nargin == 2)
    t = args(1).string_value ();
  else if (nargin > 2)
    {
      n = args(1).string_value ();
      t = args(2).string_value ();
    }

  if (error_state)
    return retval;
  
  static bool register_type = true;

  // register dispatch function type if you have not already done so
  if (register_type)
    {
      octave_dispatch::register_type ();
      register_type = false;
      fbi_sym_tab->lookup("dispatch")->mark_as_static ();
    }

  dispatch_record (f, n, t);

  return retval;
}

/*

%!test # builtin function replacement
%! dispatch('sin','length','string')
%! assert(sin('abc'),3)
%! assert(sin(0),0,10*eps); 
%!test # 'any' function
%! dispatch('sin','exp','any')
%! assert(sin(0),1,eps);
%! assert(sin('abc'),3);
%!test # 'builtin' function
%! assert(builtin('sin',0),0,eps);
%! builtin('eval','x=1;');
%! assert(x,1);
%!test # clear function mapping
%! dispatch('sin','string')
%! dispatch('sin','any')
%! assert(sin(0),0,10*eps);
%!test # oct-file replacement
%! dispatch('fft','length','string')
%! assert(fft([1,1]),[2,0]);
%! assert(fft('abc'),3)
%! dispatch('fft','string');
%!test # m-file replacement
%! dispatch('hamming','length','string')
%! assert(hamming(1),1)
%! assert(hamming('abc'),3)
%! dispatch('hamming','string')

%!test # override preloaded builtin
%! evalin('base','cos(1);');
%! dispatch('cos','length','string')
%! evalin('base',"assert(cos('abc'),3)");
%! evalin('base',"assert(cos(0),1,eps)");
%! dispatch('cos','string')
%!test # override pre-loaded oct-file
%! evalin('base','qr(1);');
%! dispatch('qr','length','string')
%! evalin('base',"assert(qr('abc'),3)");
%! evalin('base',"assert(qr(1),1)");
%! dispatch('qr','string');
%!test # override pre-loaded m-file
%! evalin('base','hanning(1);');
%! dispatch('hanning','length','string')
%! evalin('base','assert(hanning("abc"),3)');
%! evalin('base','assert(hanning(1),1)');
%! dispatch('hanning','string');

XXX FIXME XXX I would rather not create dispatch_x/dispatch_y
in the current directory!  I don't want them installed accidentally.

%!test # replace base m-file
%! system("echo 'function a=dispatch_x(a)'>dispatch_x.m");
%! dispatch('dispatch_x','length','string')
%! assert(dispatch_x(3),3)
%! assert(dispatch_x('a'),1)
%! pause(1);
%! system("echo 'function a=dispatch_x(a),++a;'>dispatch_x.m");
%! assert(dispatch_x(3),4)
%! assert(dispatch_x('a'),1)
%!test 
%! system("rm dispatch_x.m");

%!test # replace dispatch m-file
%! system("echo 'function a=dispatch_y(a)'>dispatch_y.m");
%! dispatch('hello','dispatch_y','complex scalar')
%! assert(hello(3i),3i)
%! pause(1);
%! system("echo 'function a=dispatch_y(a),++a;'>dispatch_y.m");
%! assert(hello(3i),1+3i)
%!test 
%! system("rm dispatch_y.m");

XXX FIXME XXX add tests for preservation of mark_as_command status.

*/
