/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

// Author: John W. Eaton.
// HDF5 support by Steven G. Johnson <stevenj@alum.mit.edu>
// Matlab v5 support by James R. Van Zandt <jrv@vanzandt.mv.com>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cstring>
#include <cctype>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>

#ifdef HAVE_HDF5
#include <hdf5.h>
#endif

#include "byte-swap.h"
#include "data-conv.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "lo-sstream.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "load-save.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-cell.h"
#include "pager.h"
#include "pt-exp.h"
#include "symtab.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"
#include "dMatrix.h"

#ifdef HAVE_HDF5
#include "ls-hdf5.h"
#endif
#include "ls-mat-ascii.h"
#include "ls-mat4.h"
#include "ls-mat5.h"
#include "ls-oct-ascii.h"
#include "ls-oct-binary.h"

// Write octave-core file if Octave crashes or is killed by a signal.
static bool Vcrash_dumps_octave_core;

// The maximum amount of memory (in kilobytes) that we will attempt to
// write to the Octave core file.
static double Voctave_core_file_limit;

// The name of the Octave core file.
static std::string Voctave_core_file_name;

// The default output format.  May be one of "binary", "text",
// "mat-binary", or "hdf5".
static std::string Vdefault_save_format;

// The output format for Octave core files.  May be one of "binary",
// "text", "mat-binary", or "hdf5".
static std::string Voctave_core_file_format;

// The format string for the comment line at the top of text-format
// save files.  Passed to strftime.  Should begin with `#' and contain
// no newline characters.
static std::string Vsave_header_format_string;

// XXX FIXME XXX -- shouldn't this be implemented in terms of other
// functions that are already available?

// Install a variable with name NAME and the value specified TC in the
// symbol table.  If FORCE is TRUE, replace any existing definition
// for NAME.  If GLOBAL is TRUE, make the variable global.
//
// Assumes TC is defined.

static void
install_loaded_variable (int force, const std::string& name,
			 const octave_value& val,
			 int global, const std::string& doc)
{
  // Is there already a symbol by this name?  If so, what is it?

  symbol_record *lsr = curr_sym_tab->lookup (name);

  bool is_undefined = true;
  bool is_variable = false;
  bool is_function = false;
  bool is_global = false;

  if (lsr)
    {
      is_undefined = ! lsr->is_defined ();
      is_variable = lsr->is_variable ();
      is_function = lsr->is_function ();
      is_global = lsr->is_linked_to_global ();
    }

  symbol_record *sr = 0;

  if (global)
    {
      if (is_global || is_undefined)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: global variable name `%s' exists",
		       name.c_str ());
	      warning ("use `load -force' to overwrite");
	    }
	}
      else if (is_function)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: `%s' is currently a function in this scope",
		       name.c_str ());
	      warning ("`load -force' will load variable and hide function");
	    }
	}
      else if (is_variable)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: local variable name `%s' exists",
		       name.c_str ());
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	error ("load: unable to load data for unknown symbol type");
    }
  else
    {
      if (is_global)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: global variable name `%s' exists",
		       name.c_str ());
	      warning ("use `load -force' to overwrite");
	    }
	}
      else if (is_function)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: `%s' is currently a function in this scope",
		       name.c_str ());
	      warning ("`load -force' will load variable and hide function");
	    }
	}
      else if (is_variable || is_undefined)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: local variable name `%s' exists",
		       name.c_str ());
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	error ("load: unable to load data for unknown symbol type");
    }

  if (sr)
    {
      sr->define (val);
      sr->document (doc);
      return;
    }
  else
    error ("load: unable to load variable `%s'", name.c_str ());

  return;
}

// Return TRUE if NAME matches one of the given globbing PATTERNS.

static bool
matches_patterns (const string_vector& patterns, int pat_idx,
		  int num_pat, const std::string& name)
{
  for (int i = pat_idx; i < num_pat; i++)
    {
      glob_match pattern (patterns[i]);

      if (pattern.match (name))
	return true;
    }

  return false;
}

int
read_binary_file_header (std::istream& is, bool& swap,
			 oct_mach_info::float_format& flt_fmt, bool quiet)
{
  const int magic_len = 10;
  char magic[magic_len+1];
  is.read (X_CAST (char *, magic), magic_len);
  magic[magic_len] = '\0';

  if (strncmp (magic, "Octave-1-L", magic_len) == 0)
    swap = oct_mach_info::words_big_endian ();
  else if (strncmp (magic, "Octave-1-B", magic_len) == 0)
    swap = ! oct_mach_info::words_big_endian ();
  else
    {
      if (! quiet)
	error ("load: can't read binary file");
      return -1;
    }
	
  char tmp = 0;
  is.read (X_CAST (char *, &tmp), 1);

  flt_fmt = mopt_digit_to_float_format (tmp);

  if (flt_fmt == oct_mach_info::flt_fmt_unknown)
    {
      if (! quiet)
        error ("load: unrecognized binary format!");

      return -1;
    }

  return 0;
}

static load_save_format
get_file_format (const std::string& fname, const std::string& orig_fname)
{
  load_save_format retval = LS_UNKNOWN;

#ifdef HAVE_HDF5
  // check this before we open the file
  if (H5Fis_hdf5 (fname.c_str ()) > 0)
    return LS_HDF5;
#endif /* HAVE_HDF5 */

  std::ifstream file (fname.c_str ());

  if (! file)
    {
      error ("load: couldn't open input file `%s'", orig_fname.c_str ());
      return retval;
    }

  oct_mach_info::float_format flt_fmt = oct_mach_info::flt_fmt_unknown;

  bool swap = false;

  if (read_binary_file_header (file, swap, flt_fmt, true) == 0)
    retval = LS_BINARY;
  else
    {
      file.seekg (0, std::ios::beg);

      FOUR_BYTE_INT mopt, nr, nc, imag, len;

      int err = read_mat_file_header (file, swap, mopt, nr, nc, imag, len, 1);

      if (! err)
	retval = LS_MAT_BINARY;
      else
	{
	  file.clear ();
	  file.seekg (0, std::ios::beg);

	  err = read_mat5_binary_file_header (file, swap, true);

	  if (! err)
  	    {
	      file.clear ();
	      file.seekg (0, std::ios::beg);
	      retval = LS_MAT5_BINARY;
  	    }
  	  else
  	    {
	      file.clear ();
	      file.seekg (0, std::ios::beg);

	      std::string tmp = extract_keyword (file, "name");

	      if (! tmp.empty ())
		retval = LS_ASCII;
	      else
		{
		  // Try reading the file as numbers only, determining the
		  // number of rows and columns from the data.  We don't
		  // even bother to check to see if the first item in the
		  // file is a number, so that get_complete_line() can
		  // skip any comments that might appear at the top of the
		  // file.
		  
		  retval = LS_MAT_ASCII;
		}
	    }
	}
    }

  file.close ();

  if (retval == LS_UNKNOWN)
    error ("load: unable to determine file format for `%s'",
	   orig_fname.c_str ());

  return retval;
}

octave_value
do_load (std::istream& stream, const std::string& orig_fname, bool force,
	 load_save_format format, oct_mach_info::float_format flt_fmt,
	 bool list_only, bool swap, bool verbose,
	 const string_vector& argv, int argv_idx, int argc, int nargout)
{
  octave_value retval;

  Octave_map retstruct;

  OSSTREAM output_buf;

  int count = 0;

  for (;;)
    {
      bool global = false;
      octave_value tc;

      std::string name;
      std::string doc;

      switch (format)
	{
	case LS_ASCII:
	  name = read_ascii_data (stream, orig_fname, global, tc, count);
	  break;

	case LS_BINARY:
	  name = read_binary_data (stream, swap, flt_fmt, orig_fname,
				   global, tc, doc);
	  break;

	case LS_MAT_ASCII:
	  name = read_mat_ascii_data (stream, orig_fname, tc);
	  break;

	case LS_MAT_BINARY:
	  name = read_mat_binary_data (stream, orig_fname, tc);
	  break;

#ifdef HAVE_HDF5
	case LS_HDF5:
	  name = read_hdf5_data (stream, orig_fname, global, tc, doc);
	  break;
#endif /* HAVE_HDF5 */

	case LS_MAT5_BINARY:
	  name = read_mat5_binary_element (stream, orig_fname, swap,
					   global, tc);
	  break;

	default:
	  gripe_unrecognized_data_fmt ("load");
	  break;
	}

      if (error_state || stream.eof () || name.empty ())
	break;
      else if (! error_state && ! name.empty ())
	{
	  if (tc.is_defined ())
	    {
	      if (format == LS_MAT_ASCII && argv_idx < argc)
		warning ("load: loaded ASCII file `%s' -- ignoring extra args",
			 orig_fname.c_str ());

	      if (format == LS_MAT_ASCII
		  || argv_idx == argc
		  || matches_patterns (argv, argv_idx, argc, name))
		{
		  count++;
		  if (list_only)
		    {
		      if (verbose)
			{
			  if (count == 1)
			    output_buf
			      << "type               rows   cols   name\n"
			      << "====               ====   ====   ====\n";

			  output_buf
			    << std::setiosflags (std::ios::left)
			    << std::setw (16) << tc.type_name () . c_str ()
			    << std::setiosflags (std::ios::right)
			    << std::setw (7) << tc.rows ()
			    << std::setw (7) << tc.columns ()
			    << "   ";
			}
		      output_buf << name << "\n";
		    }
		  else
		    {
		      if (nargout == 1)
			{
			  if (format == LS_MAT_ASCII)
			    retval = tc;
			  else
			    retstruct.assign (name, tc);
			}
		      else
			install_loaded_variable (force, name, tc, global, doc);
		    }
		}

	      // Only attempt to read one item from a headless text file.

	      if (format == LS_MAT_ASCII)
		break;
	    }
	  else
	    error ("load: unable to load variable `%s'", name.c_str ());
	}
      else
	{
	  if (count == 0)
	    error ("load: are you sure `%s' is an Octave data file?",
		   orig_fname.c_str ());

	  break;
	}
    }

  if (list_only && count)
    {
      output_buf << OSSTREAM_ENDS;
      std::string msg = OSSTREAM_STR (output_buf);
      OSSTREAM_FREEZE (output_buf);

      if (nargout > 0)
	retval = msg;
      else
	octave_stdout << msg;
    }
  else if (! retstruct.empty ())
    retval = retstruct;

  return retval;
}

// HDF5 load/save documentation is included in the Octave manual
// regardless, but if HDF5 is not linked in we also include a
// sentence noting this, so the user understands that the features
// aren't available.  Define a macro for this sentence:

#ifdef HAVE_HDF5
#define HAVE_HDF5_HELP_STRING ""
#else /* ! HAVE_HDF5 */
#define HAVE_HDF5_HELP_STRING "\n\
HDF5 load and save are not available, as this Octave executable was\n\
not linked with the HDF5 library."
#endif /* ! HAVE HDF5 */

DEFCMD (load, args, nargout,
  "-*- texinfo -*-\n\
@deffn {Command} load options file v1 v2 @dots{}\n\
Load the named variables from the file @var{file}.  As with @code{save},\n\
you may specify a list of variables and @code{load} will only extract\n\
those variables with names that match.  For example, to restore the\n\
variables saved in the file @file{data}, use the command\n\
\n\
@example\n\
load data\n\
@end example\n\
\n\
If a variable that is not marked as global is loaded from a file when a\n\
global symbol with the same name already exists, it is loaded in the\n\
global symbol table.  Also, if a variable is marked as global in a file\n\
and a local symbol exists, the local symbol is moved to the global\n\
symbol table and given the value from the file.  Since it seems that\n\
both of these cases are likely to be the result of some sort of error,\n\
they will generate warnings.\n\
\n\
If invoked with a single output argument, Octave returns data instead\n\
of inserting variables in the symbol table.  If the data file contains\n\
only numbers (TAB- or space-delimited columns), a matrix of values is\n\
returned.  Otherwise, @code{load} returns a structure with members\n\
 corresponding to the names of the variables in the file.\n\
\n\
The @code{load} command can read data stored in Octave's text and\n\
binary formats, and @sc{Matlab}'s binary format.  It will automatically\n\
detect the type of file and do conversion from different floating point\n\
formats (currently only IEEE big and little endian, though other formats\n\
may added in the future).\n\
\n\
Valid options for @code{load} are listed in the following table.\n\
\n\
@table @code\n\
@item -force\n\
The @samp{-force} option is accepted but ignored for backward\n\
compatiability. Octave now overwrites variables currently in memeory with\n\
the same name as those found in the file.\n\
\n\
@item -ascii\n\
Force Octave to assume the file is in Octave's text format.\n\
\n\
@strong{WARNING: the meaning of this option will change in a future\n\
version of Octave to be compatible with @sc{Matlab}.  To keep the\n\
meaning of your code the same across this change, use the @code{-text}\n\
option instead.}\n\
\n\
@item -binary\n\
Force Octave to assume the file is in Octave's binary format.\n\
\n\
@item -mat\n\
@itemx -mat-binary\n\
Force Octave to assume the file is in @sc{Matlab}'s version 6 binary\n\
format.\n\
\n\
@item -V4\n\
@itemx -v4\n\
@itemx -4\n\
@itemx -mat4-binary\n\
Force Octave to assume the file is in the binary format written by\n\
@sc{Matlab} version 4.\n\
\n\
@item -hdf5\n\
Force Octave to assume the file is in HDF5 format.\n\
(HDF5 is a free, portable binary format developed by the National\n\
Center for Supercomputing Applications at the University of Illinois.)\n\
Note that Octave can read HDF5 files not created by itself, but may\n\
skip some datasets in formats that it cannot support.\n"

HAVE_HDF5_HELP_STRING

"\n\
@item -import\n\
The @samp{-import} is accepted but ignored for backward compatiability.\n\
Octave can now support multi-dimensional HDF data and automatically\n\
modifies variable names if they are invalid Octave identifiers.\n\
\n\
@item -text\n\
Force Octave to assume the file is in Octave's text format.\n\
@end table\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("load");

  if (error_state)
    return retval;

  // It isn't necessary to have the default load format stored in a
  // user preference variable since we can determine the type of file
  // as we are reading.

  load_save_format format = LS_UNKNOWN;

  bool force = true;
  bool list_only = false;
  bool verbose = false;

  int i;
  for (i = 1; i < argc; i++)
    {
      if (argv[i] == "-force" || argv[i] == "-f")
	{
	  // Silently ignore this
	  // warning ("load: -force ignored");
	}
      else if (argv[i] == "-list" || argv[i] == "-l")
	{
	  list_only = true;
	}
      else if (argv[i] == "-verbose" || argv[i] == "-v")
	{
	  verbose = true;
	}
      else if (argv[i] == "-ascii" || argv[i] == "-a")
	{
	  warning ("the meaning of this option will change in a future");
	  warning ("version of Octave to be compatible with @sc{Matlab}.");
	  warning ("To keep the meaning of your code the same across");
	  warning ("this change, use the -text option instead.");

	  format = LS_ASCII;
	}
      else if (argv[i] == "-binary" || argv[i] == "-b")
	{
	  format = LS_BINARY;
	}
      else if (argv[i] == "-mat-binary" || argv[i] == "-mat" || argv[i] == "-m")
	{
	  format = LS_MAT5_BINARY;
	}
      else if (argv[i] == "-mat4-binary" || argv[i] == "-V4"
	       || argv[i] == "-v4" || argv[i] == "-4")
	{
	  format = LS_MAT_BINARY;
	}
      else if (argv[i] == "-hdf5" || argv[i] == "-h")
	{
#ifdef HAVE_HDF5
	  format = LS_HDF5;
#else /* ! HAVE_HDF5 */
	  error ("load: octave executable was not linked with HDF5 library");
	  return retval;
#endif /* ! HAVE_HDF5 */
	}
      else if (argv[i] == "-import" || argv[i] == "-i")
	{
	  warning ("load: -import ignored");
	}
      else if (argv[i] == "-text" || argv[i] == "-t")
	{
	  format = LS_ASCII;
	}
      else
	break;
    }

  if (i == argc)
    {
      print_usage ("load");
      return retval;
    }

  std::string orig_fname = argv[i];

  oct_mach_info::float_format flt_fmt = oct_mach_info::flt_fmt_unknown;

  bool swap = false;

  if (argv[i] == "-")
    {
      i++;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
	error ("load: cannot read HDF5 format from stdin");
      else
#endif /* HAVE_HDF5 */
      if (format != LS_UNKNOWN)
	{
	  // XXX FIXME XXX -- if we have already seen EOF on a
	  // previous call, how do we fix up the state of std::cin so
	  // that we can get additional input?  I'm afraid that we
	  // can't fix this using std::cin only.

	  retval = do_load (std::cin, orig_fname, force, format, flt_fmt,
			    list_only, swap, verbose, argv, i, argc,
			    nargout);
	}
      else
	error ("load: must specify file format if reading from stdin");
    }
  else
    {
      std::string fname = file_ops::tilde_expand (argv[i]);

      // Check if file exists, if it doesn't then also check with a 
      // .mat extension
      std::ifstream file_exist (fname.c_str ());
      if (file_exist)
	file_exist.close ();
      else
	{
	  fname.append (".mat");
	  std::ifstream file_mat_exist (fname.c_str ());
	  if (file_mat_exist)
	    file_mat_exist.close ();
	  else
	    {
	      error ("load: nonexistent file: `%s'", orig_fname.c_str ());
	      return retval;
	    }
	}

      if (format == LS_UNKNOWN)
	format = get_file_format (fname, orig_fname);

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
	{
	  i++;

	  hdf5_ifstream hdf5_file (fname.c_str ());

	  if (hdf5_file.file_id >= 0)
	    {
	      retval = do_load (hdf5_file, orig_fname, force, format,
				flt_fmt, list_only, swap, verbose,
				argv, i, argc, nargout);

	      hdf5_file.close ();
	    }
	  else
	    error ("load: couldn't open input file `%s'",
		   orig_fname.c_str ());
	}
      else
#endif /* HAVE_HDF5 */
	// don't insert any statements here; the "else" above has to
	// go with the "if" below!!!!!
      if (format != LS_UNKNOWN)
	{
	  i++;

	  std::ios::openmode mode = std::ios::in;

	  if (format == LS_BINARY
#ifdef HAVE_HDF5
	      || format == LS_HDF5
#endif
	      || format == LS_MAT_BINARY
	      || format == LS_MAT5_BINARY)
	    mode |= std::ios::binary;

	  std::ifstream file (fname.c_str (), mode);

	  if (file)
	    {
	      if (format == LS_BINARY)
		{
		  if (read_binary_file_header (file, swap, flt_fmt) < 0)
		    {
		      file.close ();
		      return retval;
		    }
		}
	      else if (format == LS_MAT5_BINARY)
		{
		  if (read_mat5_binary_file_header (file, swap, false) < 0)
		    {
		      file.close ();
		      return retval;
		    }
		}

	      retval = do_load (file, orig_fname, force, format,
				flt_fmt, list_only, swap, verbose,
				argv, i, argc, nargout);
	      file.close ();
	    }
	  else
	    error ("load: couldn't open input file `%s'",
		   orig_fname.c_str ());
	}
    }

  return retval;
}

// Return TRUE if PATTERN has any special globbing chars in it.

static bool
glob_pattern_p (const std::string& pattern)
{
  int open = 0;

  int len = pattern.length ();

  for (int i = 0; i < len; i++)
    {
      char c = pattern[i];

      switch (c)
	{
	case '?':
	case '*':
	  return true;

	case '[':	// Only accept an open brace if there is a close
	  open++;	// brace to match it.  Bracket expressions must be
	  continue;	// complete, according to Posix.2

	case ']':
	  if (open)
	    return true;
	  continue;

	case '\\':
	  if (i == len - 1)
	    return false;

	default:
	  continue;
	}
    }

  return false;
}

static void
do_save (std::ostream& os, const octave_value& tc,
	 const std::string& name, const std::string& help,
	 int global, load_save_format fmt, bool save_as_floats,
	 bool& infnan_warned)
{
  switch (fmt)
    {
    case LS_ASCII:
      save_ascii_data (os, tc, name, infnan_warned, false, global, 0);
      break;

    case LS_BINARY:
      save_binary_data (os, tc, name, help, global, save_as_floats);
      break;

    case LS_MAT_BINARY:
      save_mat_binary_data (os, tc, name);
      break;

#ifdef HAVE_HDF5
    case LS_HDF5:
      save_hdf5_data (os, tc, name, help, global, save_as_floats);
      break;
#endif /* HAVE_HDF5 */

    case LS_MAT5_BINARY:
      save_mat5_binary_element (os, tc, name, global, save_as_floats);
      break;

    default:
      gripe_unrecognized_data_fmt ("save");
      break;
    }
}

// Save the info from SR on stream OS in the format specified by FMT.

void
do_save (std::ostream& os, symbol_record *sr, load_save_format fmt,
	 bool save_as_floats, bool& infnan_warned)
{
  if (! sr->is_variable ())
    {
      error ("save: can only save variables, not functions");
      return;
    }

  octave_value tc = sr->def ();

  if (tc.is_defined ())
    {
      std::string name = sr->name ();
      std::string help = sr->help ();

      int global = sr->is_linked_to_global ();

      do_save (os, tc, name, help, global, fmt, save_as_floats,
	       infnan_warned);
    }
}

// Save variables with names matching PATTERN on stream OS in the
// format specified by FMT.  If SAVE_BUILTINS is TRUE, also save
// builtin variables with names that match PATTERN.

static int
save_vars (std::ostream& os, const std::string& pattern, bool save_builtins,
	   load_save_format fmt, bool save_as_floats)
{
  Array<symbol_record *> vars = curr_sym_tab->glob
    (pattern, symbol_record::USER_VARIABLE, SYMTAB_ALL_SCOPES);

  int saved = vars.length ();

  bool infnan_warned = false;

  for (int i = 0; i < saved; i++)
    {
      do_save (os, vars(i), fmt, save_as_floats, infnan_warned);

      if (error_state)
	break;
    }

  if (! error_state && save_builtins)
    {
      vars = fbi_sym_tab->glob
	(pattern, symbol_record::BUILTIN_VARIABLE, SYMTAB_ALL_SCOPES);

      int count = vars.length ();

      saved += count;

      for (int i = 0; i < count; i++)
	{
	  do_save (os, vars(i), fmt, save_as_floats, infnan_warned);

	  if (error_state)
	    break;
	}
    }

  return saved;
}

static load_save_format
get_save_format (const std::string& fmt,
		 load_save_format fallback_format = LS_ASCII)
{
  load_save_format retval = fallback_format;

  if (fmt == "binary")
    retval = LS_BINARY;
  else if (fmt == "mat-binary" || fmt =="mat_binary")
    retval = LS_MAT5_BINARY;
  else if (fmt == "mat4-binary" || fmt =="mat4_binary")
    retval = LS_MAT_BINARY;
#ifdef HAVE_HDF5
  else if (fmt == "hdf5")
    retval = LS_HDF5;
#endif /* HAVE_HDF5 */
      
  return retval;
}

void
write_header (std::ostream& os, load_save_format format)
{
  switch (format)
    {
    case LS_BINARY:
      {
	os << (oct_mach_info::words_big_endian ()
	       ? "Octave-1-B" : "Octave-1-L");

	oct_mach_info::float_format flt_fmt =
	  oct_mach_info::native_float_format ();

	char tmp = (char) float_format_to_mopt_digit (flt_fmt);

	os.write (X_CAST (char *, &tmp), 1);
      }
      break;

    case LS_MAT5_BINARY:
      {
	char const * versionmagic;
	TWO_BYTE_INT number = *(TWO_BYTE_INT *)"\x00\x01";
	struct tm bdt;
	time_t now;
	char headertext[128];

	time (&now);
	bdt = *gmtime (&now);
	memset (headertext, ' ', 124);
	// ISO 8601 format date
	strftime (headertext, 124, "MATLAB 5.0 MAT-file, written by Octave "
		 OCTAVE_VERSION ", %Y-%m-%d %T UTC", &bdt);

	// The first pair of bytes give the version of the MAT file
	// format.  The second pair of bytes form a magic number which
	// signals a MAT file.  MAT file data are always written in
	// native byte order.  The order of the bytes in the second
	// pair indicates whether the file was written by a big- or
	// little-endian machine.  However, the version number is
	// written in the *opposite* byte order from everything else!
	if (number == 1)
	  versionmagic = "\x01\x00\x4d\x49"; // this machine is big endian
	else
	  versionmagic = "\x00\x01\x49\x4d"; // this machine is little endian

	memcpy (headertext+124, versionmagic, 4);
	os.write (headertext, 128);
      }

      break;

#ifdef HAVE_HDF5
    case LS_HDF5:
#endif /* HAVE_HDF5 */
    case LS_ASCII:
      {
	octave_localtime now;

	std::string comment_string = now.strftime (Vsave_header_format_string);

	if (! comment_string.empty ())
	  {
#ifdef HAVE_HDF5
	    if (format == LS_HDF5)
	      {
		hdf5_ofstream& hs = (hdf5_ofstream&) os;
		H5Gset_comment (hs.file_id, "/", comment_string.c_str ());
	      }
	    else
#endif /* HAVE_HDF5 */
	      os << comment_string << "\n";
	  }
      }
    break;

    default:
      break;
    }
}

static void
save_vars (const string_vector& argv, int argv_idx, int argc,
	   std::ostream& os, bool save_builtins, load_save_format fmt,
	   bool save_as_floats, bool write_header_info)
{
  if (write_header_info)
    write_header (os, fmt);

  if (argv_idx == argc)
    {
      save_vars (os, "*", save_builtins, fmt, save_as_floats);
    }
  else
    {
      for (int i = argv_idx; i < argc; i++)
	{
	  if (! save_vars (os, argv[i], save_builtins, fmt, save_as_floats))
	    {
	      warning ("save: no such variable `%s'", argv[i].c_str ());
	    }
	}
    }
}

void
dump_octave_core (std::ostream& os, const char *fname, load_save_format fmt)
{
  write_header (os, fmt);

  Array<symbol_record *> vars = curr_sym_tab->glob
    ("*", symbol_record::USER_VARIABLE, SYMTAB_ALL_SCOPES);

  int num_to_save = vars.length ();

  bool infnan_warned = false;

  double save_mem_size = 0;

  for (int i = 0; i < num_to_save; i++)
    {
      symbol_record *sr = vars(i);

      if (sr->is_variable ())
	{
	  octave_value tc = sr->def ();

	  if (tc.is_defined ())
	    {
	      double tc_size = tc.byte_size () / 1024;

	      // XXX FIXME XXX -- maybe we should try to throw out hte
	      // largest first...

	      if (Voctave_core_file_limit < 0
		  || save_mem_size + tc_size < Voctave_core_file_limit)
		{
		  save_mem_size += tc_size;

		  std::string name = sr->name ();
		  std::string help = sr->help ();

		  int global = sr->is_linked_to_global ();

		  do_save (os, tc, name, help, global, fmt, false,
			   infnan_warned);

		  if (error_state)
		    break;
		}
	    }
	}
    }

  message (0, "save to `%s' complete", fname);
}

void
dump_octave_core (void)
{
  if (Vcrash_dumps_octave_core)
    {
      // XXX FIXME XXX -- should choose better file name?

      const char *fname = Voctave_core_file_name.c_str ();

      message (0, "attempting to save variables to `%s'...", fname);

      load_save_format format
	= get_save_format (Voctave_core_file_format, LS_BINARY);

      std::ios::openmode mode = std::ios::out|std::ios::trunc;

      if (format == LS_BINARY
#ifdef HAVE_HDF5
	  || format == LS_HDF5
#endif
	  || format == LS_MAT_BINARY
	  || format == LS_MAT5_BINARY)
	mode |= std::ios::binary;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
	{
	  hdf5_ofstream file (fname);

	  if (file.file_id >= 0)
	    {
	      dump_octave_core (file, fname, format);

	      file.close ();
	    }
	  else
	    warning ("unable to open `%s' for writing...", fname);
	}
      else
#endif /* HAVE_HDF5 */
	// don't insert any commands here!  The open brace below must
	// go with the else above!
	{
	  std::ofstream file (fname, mode);
	  
	  if (file)
	    {
	      dump_octave_core (file, fname, format);

	      file.close ();
	    }
	  else
	    warning ("unable to open `%s' for writing...", fname);
	}
    }
}

DEFCMD (save, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} save options file v1 v2 @dots{}\n\
Save the named variables @var{v1}, @var{v2}, @dots{} in the file\n\
@var{file}.  The special filename @samp{-} can be used to write the\n\
output to your terminal.  If no variable names are listed, Octave saves\n\
all the variables in the current scope.  Valid options for the\n\
@code{save} command are listed in the following table.  Options that\n\
modify the output format override the format specified by the built-in\n\
variable @code{default_save_format}.\n\
\n\
@table @code\n\
@item -ascii\n\
Save the data in Octave's text data format.\n\
\n\
@strong{WARNING: the meaning of this option will change in a future\n\
version of Octave to be compatible with @sc{Matlab}.  To keep the\n\
meaning of your code the same across this change, use the @code{-text}\n\
option instead.}\n\
\n\
@item -binary\n\
Save the data in Octave's binary data format.\n\
\n\
@item -float-binary\n\
Save the data in Octave's binary data format but only using single\n\
precision.  You should use this format only if you know that all the\n\
values to be saved can be represented in single precision.\n\
\n\
@item -mat\n\
@itemx -mat-binary\n\
Save the data in @sc{Matlab}'s binary data format.\n\
\n\
@item -V4\n\
@itemx -v4\n\
@itemx -4\n\
@itemx -mat4-binary\n\
Save the data in the binary format written by @sc{Matlab} version 4.\n\
\n\
@item -hdf5\n\
Save the data in HDF5 format.\n\
(HDF5 is a free, portable binary format developed by the National\n\
Center for Supercomputing Applications at the University of Illinois.)\n"

HAVE_HDF5_HELP_STRING

"\n\
@item -float-hdf5\n\
Save the data in HDF5 format but only using single precision.\n\
You should use this format only if you know that all the\n\
values to be saved can be represented in single precision.\n\
\n\
@item -save-builtins\n\
Force Octave to save the values of built-in variables too.  By default,\n\
Octave does not save built-in variables.\n\
@end table\n\
\n\
The list of variables to save may include wildcard patterns containing\n\
the following special characters:\n\
@table @code\n\
@item ?\n\
Match any single character.\n\
\n\
@item *\n\
Match zero or more characters.\n\
\n\
@item [ @var{list} ]\n\
Match the list of characters specified by @var{list}.  If the first\n\
character is @code{!} or @code{^}, match all characters except those\n\
specified by @var{list}.  For example, the pattern @samp{[a-zA-Z]} will\n\
match all lower and upper case alphabetic characters. \n\
\n\
@item -text\n\
Save the data in Octave's text data format.\n\
@end table\n\
\n\
Except when using the @sc{Matlab} binary data file format, saving global\n\
variables also saves the global status of the variable, so that if it is\n\
restored at a later time using @samp{load}, it will be restored as a\n\
global variable.\n\
\n\
The command\n\
\n\
@example\n\
save -binary data a b*\n\
@end example\n\
\n\
@noindent\n\
saves the variable @samp{a} and all variables beginning with @samp{b} to\n\
the file @file{data} in Octave's binary format.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("save");

  if (error_state)
    return retval;

  // Here is where we would get the default save format if it were
  // stored in a user preference variable.

  bool save_builtins = false;

  bool save_as_floats = false;

  load_save_format format = get_save_format (Vdefault_save_format);

  bool append = false;

  int i;
  for (i = 1; i < argc; i++)
    {
      if (argv[i] == "-append")
	{
	  append = true;
	}
      else if (argv[i] == "-ascii" || argv[i] == "-a")
	{
	  warning ("the meaning of this option will change in a future");
	  warning ("version of Octave to be compatible with @sc{Matlab}.");
	  warning ("To keep the meaning of your code the same across");
	  warning ("this change, use the -text option instead.");

	  format = LS_ASCII;
	}
      else if (argv[i] == "-text" || argv[i] == "-t")
	{
	  format = LS_ASCII;
	}
      else if (argv[i] == "-binary" || argv[i] == "-b")
	{
	  format = LS_BINARY;
	}
      else if (argv[i] == "-hdf5" || argv[i] == "-h")
	{
#ifdef HAVE_HDF5
	  format = LS_HDF5;
#else /* ! HAVE_HDF5 */
	  error ("save: octave executable was not linked with HDF5 library");
	  return retval;
#endif /* ! HAVE_HDF5 */
	}
      else if (argv[i] == "-mat-binary" || argv[i] == "-mat" || argv[i] == "-m")
	{
	  format = LS_MAT5_BINARY;
	}
      else if (argv[i] == "-mat4-binary" || argv[i] == "-V4"
	       || argv[i] == "-v4" || argv[i] == "-4")
	{
	  format = LS_MAT_BINARY;
	}
      else if (argv[i] == "-float-binary" || argv[i] == "-f")
	{
	  format = LS_BINARY;
	  save_as_floats = true;
	}
      else if (argv[i] == "-float-hdf5")
	{
#ifdef HAVE_HDF5
	  format = LS_HDF5;
	  save_as_floats = true;
#else /* ! HAVE_HDF5 */
	  error ("save: octave executable was not linked with HDF5 library");
	  return retval;
#endif /* ! HAVE_HDF5 */
	}
      else if (argv[i] == "-save-builtins")
	{
	  save_builtins = true;
	}
      else
	break;
    }

  if (i == argc)
    {
      print_usage ("save");
      return retval;
    }

  if (save_as_floats && format == LS_ASCII)
    {
      error ("save: cannot specify both -ascii and -float-binary");
      return retval;
    }

  if (argv[i] == "-")
    {
      i++;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
        error ("save: cannot write HDF5 format to stdout");
      else
#endif /* HAVE_HDF5 */
	// don't insert any commands here!  the brace below must go
	// with the "else" above!
	{
	  // XXX FIXME XXX -- should things intended for the screen end up
	  // in a octave_value (string)?
	  
	  save_vars (argv, i, argc, octave_stdout, save_builtins, format,
		     save_as_floats, true);
	}
    }

  // Guard against things like `save a*', which are probably mistakes...

  else if (i == argc - 1 && glob_pattern_p (argv[i]))
    {
      print_usage ("save");
      return retval;
    }
  else
    {
      std::string fname = file_ops::tilde_expand (argv[i]);

      i++;

      std::ios::openmode mode = std::ios::out;

      if (format == LS_BINARY
#ifdef HAVE_HDF5
	  || format == LS_HDF5
#endif
	  || format == LS_MAT_BINARY
	  || format == LS_MAT5_BINARY)
	mode |= std::ios::binary;

      mode |= append ? std::ios::ate : std::ios::trunc;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
	{
	  hdf5_ofstream hdf5_file (fname.c_str ());

	  if (hdf5_file.file_id >= 0)
	    {
	      save_vars (argv, i, argc, hdf5_file, save_builtins, format,
			 save_as_floats, true);

	      hdf5_file.close ();
	  }
	else
	  {
	    error ("save: couldn't open output file `%s'", fname.c_str ());
	    return retval;
	  }
	}
      else
#endif /* HAVE_HDF5 */
	// don't insert any statements here!  The brace below must go
	// with the "else" above!
	{
	  std::ofstream file (fname.c_str (), mode);
	  
	  if (file)
	    {
	      bool write_header_info
		= ((file.rdbuf ())->pubseekoff (0, std::ios::cur)
		   == static_cast<std::streampos> (0));
	      
	      save_vars (argv, i, argc, file, save_builtins, format,
			 save_as_floats, write_header_info);
	    }
	  else
	    {
	      error ("save: couldn't open output file `%s'", fname.c_str ());
	      return retval;
	    }
	}
    }

  return retval;
}

static int
crash_dumps_octave_core (void)
{
  Vcrash_dumps_octave_core = check_preference ("crash_dumps_octave_core");

  return 0;
}

static int
default_save_format (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("default_save_format");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("default_save_format");
      status = -1;
    }
  else
    Vdefault_save_format = s;

  return status;
}

static int
octave_core_file_limit (void)
{
  double val;

  if (builtin_real_scalar_variable ("octave_core_file_limit", val))
    {
      Voctave_core_file_limit = val;
      return 0;
    }
  else
    gripe_invalid_value_specified ("octave_core_file_limit");

  return -1;
}

static int
octave_core_file_name (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("octave_core_file_name");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("octave_core_file_name");
      status = -1;
    }
  else
    Voctave_core_file_name = s;

  return status;
}

static int
octave_core_file_format (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("octave_core_file_format");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("octave_core_file_format");
      status = -1;
    }
  else
    Voctave_core_file_format = s;

  return status;
}

static std::string
default_save_header_format (void)
{
  return
    std::string ("# Created by Octave " OCTAVE_VERSION
		 ", %a %b %d %H:%M:%S %Y %Z <")
    + octave_env::get_user_name ()
    + std::string ("@")
    + octave_env::get_host_name ()
    + std::string (">");
}

static int
save_header_format_string (void)
{
  int status = 0;

  octave_value v = builtin_any_variable ("save_header_format_string");

  if (v.is_string ())
    Vsave_header_format_string = v.string_value ();
  else
    {
      gripe_invalid_value_specified ("save_header_format_string");
      status = -1;
    }

  return status;
}

void
symbols_of_load_save (void)
{
  DEFVAR (crash_dumps_octave_core, true, crash_dumps_octave_core,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} crash_dumps_octave_core\n\
If this variable is set to a nonzero value, Octave tries to save all\n\
current variables the the file \"octave-core\" if it crashes or receives a\n\
hangup, terminate or similar signal.  The default value is 1.\n\
@seealso{octave_core_file_limit, octave_core_file_name, and octave_core_file_format}\n\
@end defvr");

  DEFVAR (default_save_format, "ascii", default_save_format,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} default_save_format\n\
This variable specifies the default format for the @code{save} command.\n\
It should have one of the following values: @code{\"ascii\"},\n\
@code{\"binary\"}, @code{float-binary}, or @code{\"mat-binary\"}.  The\n\
initial default save format is Octave's text format.\n\
@end defvr");

  DEFVAR (octave_core_file_limit, -1.0, octave_core_file_limit,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} octave_core_file_limit\n\
The maximum amount of memory (in kilobytes) of the top-level workspace\n\
that Octave will attempt to write when saving data to the\n\
@var{octave_core_file_name}.  If @var{octave_core_file_format} is a\n\
binary format, then @var{octave_core_file_limit} will be approximately\n\
the maximum size of the file.  If a text file format is used, then the\n\
file could be much larger than the limit.\n\
The default value is -1 (unlimited)\n\
@seealso{crash_dumps_octave_core, octave_core_file_name, and octave_core_file_format}\n\
@end defvr");

  DEFVAR (octave_core_file_name, "octave-core", octave_core_file_name,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} octave_core_file_name\n\
The name of the file used for saving data from the top-level workspace\n\
when Octave aborts.  The default value is @code{\"octave-core\"}\n\
@seealso{crash_dumps_octave_core, octave_core_file_name, and octave_core_file_format}\n\
@end defvr");

  DEFVAR (octave_core_file_format, "binary", octave_core_file_format,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} octave_core_file_format\n\
If Octave aborts, it attempts to save the contents of the top-level\n\
workspace in a file using this format.  The value of\n\
@code{octave_core_file_format} should have one of the following values:\n\
@code{\"ascii\"}, @code{\"binary\"}, @code{float-binary}, or\n\
@code{\"mat-binary\"}.  The default value is Octave's binary format.\n\
@seealso{crash_dumps_octave_core, octave_core_file_name, and octave_core_file_limit}\n\
@end defvr");

  DEFVAR (save_header_format_string, default_save_header_format (),
	  save_header_format_string,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} save_header_format_string\n\
This variable specifies the the format string for the comment line\n\
that is written at the beginning of text-format data files saved by\n\
Octave.  The format string is passed to @code{strftime} and should\n\
begin with the character @samp{#} and contain no newline characters.\n\
If the value of @code{save_header_format_string} is the empty string,\n\
the header comment is omitted from text-format data files.  The\n\
default value is\n\
\n\
@example\n\
\"# Created by Octave VERSION, %a %b %d %H:%M:%S %Y %Z <USER@@HOST>\"\n\
@end example\n\
@seealso{strftime}\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
