// file-io.cc                                             -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

// Written by John C. Campbell <jcc@che.utexas.edu>.

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <DLList.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <strstream.h>
#include <ctype.h>

#include "dMatrix.h"

#include "statdefs.h"
#include "file-io.h"
#include "input.h"
#include "octave-hist.h"
#include "tree-const.h"
#include "error.h"
#include "utils.h"
#include "pager.h"
#include "sysdep.h"
#include "mappers.h"

// keeps a count of how many files are open and in the file list
static int file_count = 0;

// keeps a count of args sent to printf or scanf
static int fmt_arg_count = 0;

class file_info
{
 public:
  file_info (void);
  file_info (int num, const char *nm, FILE *t, const char *md);
  file_info (const file_info& f);

  file_info& operator = (const file_info& f);

  ~file_info (void);

  int number (void) const;
  const char *name (void) const;
  FILE *fptr (void) const;
  const char *mode (void) const;

  int eof (void) const;
  int error (void) const;

 private:
  int file_number;
  char *file_name;
  FILE *file_fptr;
  char *file_mode;
};

file_info::file_info (void)
{
  file_number = -1;
  file_name = (char *) NULL;
  file_fptr = (FILE *) NULL;
  file_mode = (char *) NULL;
}

file_info::file_info (int n, const char *nm, FILE *t, const char *md)
{
  file_number = n;
  file_name = strsave (nm);
  file_fptr = t;
  file_mode = strsave (md);
}

file_info::file_info (const file_info& f)
{
  file_number = f.file_number;
  file_name = strsave (f.file_name);
  file_fptr = f.file_fptr;
  file_mode = strsave (f.file_mode);
}

file_info&
file_info::operator = (const file_info& f)
{
  if (this != & f)
    {
      file_number = f.file_number;
      delete [] file_name;
      file_name = strsave (f.file_name);
      file_fptr = f.file_fptr;
      delete [] file_mode;
      file_mode = strsave (f.file_mode);
    }
  return *this;
}

file_info::~file_info (void)
{
  delete [] file_name;
  delete [] file_mode;
}

int
file_info::number (void) const
{
  return file_number;
}

const char *
file_info::name (void) const
{
  return file_name;
}

FILE *
file_info::fptr (void) const
{
  return file_fptr;
}

const char *
file_info::mode (void) const
{
  return file_mode;
}

// double linked list containing relevant information about open files
static DLList <file_info> file_list;

void
initialize_file_io (void)
{
  file_info octave_stdin (0, "stdin", stdin, "r");
  file_info octave_stdout (1, "stdout", stdout, "w");
  file_info octave_stderr (2, "stderr", stderr, "w");

  file_list.append (octave_stdin);
  file_list.append (octave_stdout);
  file_list.append (octave_stderr);

  file_count = 3;
}

/*
 * Given a file name or number, return a pointer to the corresponding
 * open file.  If the file has not already been opened, return NULL.
 */
Pix
return_valid_file (const tree_constant& arg)
{
  if (arg.is_string_type ())
    {
      Pix p = file_list.first ();
      file_info file;
      for (int i = 0; i < file_count; i++)
	{
	  char *file_name = arg.string_value ();
	  file = file_list (p);
	  if (strcmp (file.name (), file_name) == 0)
	    return p;
	  file_list.next (p);
	}
    }
  else if (arg.is_scalar_type ())
    {
      double file_num = arg.double_value ();
      if ((double) NINT (file_num) != file_num)
	error ("file number not an integer value");
      else
	{
	  Pix p = file_list.first ();
	  file_info file;
	  for (int i = 0; i < file_count; i++)
	    {
	      file = file_list (p);
	      if (file.number () == file_num)
		return p;
	      file_list.next (p);
	    }
	  error ("no file with that number");
	}
      }
    else
      error ("inapproriate file specifier");

  return (Pix) NULL;
}

static Pix 
fopen_file_for_user (const tree_constant& arg, const char *mode,
		     const char *warn_for)
{
  char *file_name = arg.string_value ();

  FILE *file_ptr = fopen (file_name, mode);
  if (file_ptr != (FILE *) NULL)
    {
      file_info file (++file_count, file_name, file_ptr, mode);
      file_list.append (file);
      
      Pix p = file_list.first ();
      file_info file_from_list;
      
      for (int i = 0; i < file_count; i++)
	{
	  file_from_list = file_list (p);
	  if (strcmp (file_from_list.name (), file_name) == 0)
	    return p;
	  file_list.next (p);
	}
    }

  error ("%s: unable to open file `%s'", warn_for, file_name);

  return (Pix) NULL;
}

static Pix
file_io_get_file (const tree_constant arg, const char *mode,
		  const char *warn_for)
{
  Pix p = return_valid_file (arg);

  if (p == (Pix) NULL)
    {
      if (arg.is_string_type ())
	{
	  char *name = arg.string_value ();

	  struct stat buffer;
	  int status = stat (name, &buffer);

	  if (status == 0)
	    {
	      if ((buffer.st_mode & S_IFREG) == S_IFREG)
		p = fopen_file_for_user (arg, mode, warn_for);
	      else
		error ("%s: invalid file type", warn_for);
	    }
	  else if (status < 0 && *mode != 'r')
	    p = fopen_file_for_user (arg, mode, warn_for);
	  else
	    error ("%s: can't stat file `%s'", warn_for, name);
	}
      else
	error ("%s: invalid file specifier", warn_for);
    }

  return p;
}

Octave_object
fclose_internal (const Octave_object& args)
{
  Octave_object retval;

  Pix p = return_valid_file (args(1));

  if (p == (Pix) NULL)
    return retval;

  file_info file = file_list (p);

  if (file.number () < 3)
    {
      warning ("fclose: can't close stdin, stdout, or stderr!");
      return retval;
    }

  int success = fclose (file.fptr ());
  file_list.del (p);
  file_count--;

  retval.resize (1);
  if (success == 0)
    retval(0) = tree_constant (1.0); // succeeded
  else
    {
      error ("fclose: error on closing file");
      retval(0) = tree_constant (0.0); // failed
    }

  return retval;
}

Octave_object
fflush_internal (const Octave_object& args)
{
  Octave_object retval;

  Pix p = return_valid_file (args(1));

  if (p == (Pix) NULL)
    return retval;

  file_info file = file_list (p);

  if (strcmp (file.mode (), "r") == 0)
    {
      warning ("can't flush an input stream");
      return retval;
    }

  int success = 0;

  if (file.number () == 1)
    flush_output_to_pager ();
  else
    success = fflush (file.fptr ());

  retval.resize (1);
  if (success == 0)
    retval(0) = tree_constant (1.0); // succeeded
  else
    {
      error ("fflush: write error");
      retval(0) = tree_constant (0.0); // failed
    }

  return retval;
}

static int
valid_mode (const char *mode)
{
  if (mode != (char *) NULL)
    {
      char m = mode[0];
      if (m == 'r' || m == 'w' || m == 'a')
	{
	  m = mode[1];
	  return (m == '\0' || (m == '+' && mode[2] == '\0'));
	}
    }
  return 0;
}

Octave_object
fgets_internal (const Octave_object& args, int nargout)
{
  Octave_object retval;

  Pix p = file_io_get_file (args(1), "r", "fgets");
  
  if (p == (Pix) NULL)
    return retval;

  int length = 0;
  if (args(2).is_scalar_type ())
    {
      length = (int) args(2).double_value ();
      if ((double) NINT (length) != length)
	{
	  error ("fgets: length not an integer value");
	  return retval;
	}
    }

  file_info file = file_list (p);

  char string[length+1];
  char *success = fgets (string, length+1, file.fptr ());

  if (success == (char *) NULL)
    {
      retval.resize (1);
      retval(0) = tree_constant (-1.0);
      return retval;
    }

  if (nargout == 2)
    {
      retval.resize (2);
      retval(1) = tree_constant ((double) strlen (string));
    }
  else
    retval.resize (1);

  retval(0) = tree_constant (string);

  return retval;
}

Octave_object
fopen_internal (const Octave_object& args)
{
  Octave_object retval;
  Pix p;

  if (! args(1).is_string_type ())
    {
      error ("fopen: file name must be a string");
      return retval;
    }

  p = return_valid_file (args(1));

  if (p != (Pix) NULL)
    {
      file_info file = file_list (p);

      retval.resize (1);
      retval(0) = tree_constant ((double) file.number ());

      return retval;
    }

  if (! args(2).is_string_type ())
    {
      error ("fopen: file mode must be a string");
      return retval;
    }

  char *name = args(1).string_value ();
  char *mode = args(2).string_value ();

  if (! valid_mode (mode))
    {
      error ("fopen: invalid mode");
      return retval;
    }

  struct stat buffer;
  if (stat (name, &buffer) == 0 && (buffer.st_mode & S_IFDIR) == S_IFDIR)
    {
      error ("fopen: can't open directory");
      return retval;
    }

  FILE *file_ptr = fopen (name, mode);

  if (file_ptr == (FILE *) NULL)
    {
      error ("fopen: unable to open file `%s'", name);
      return retval;
    }

  int number = file_count++;

  file_info file (number, name, file_ptr, mode);
  file_list.append (file);

  retval.resize (1);
  retval(0) = tree_constant ((double) number);

  return retval;
}

Octave_object
freport_internal (void)
{
  Octave_object retval;
  Pix p = file_list.first ();

  ostrstream output_buf;

  output_buf << "\n number  mode  name\n\n";
  for (int i = 0; i < file_count; i++)
    {
      file_info file = file_list (p);
      output_buf.form ("%7d%6s  %s\n", file.number (), file.mode (),
		       file.name ());
      file_list.next (p);
    }

  output_buf << "\n" << ends;
  maybe_page_output (output_buf);

  return retval;
}

Octave_object
frewind_internal (const Octave_object& args)
{
  Octave_object retval;

  Pix p = file_io_get_file (args(1), "a+", "frewind");

  if (p != (Pix) NULL)
    {
      file_info file = file_list (p);
      rewind (file.fptr ());
    }

  return retval;
}

Octave_object
fseek_internal (const Octave_object& args)
{
  Octave_object retval;

  int nargin = args.length ();

  Pix p = file_io_get_file (args(1), "a+", "fseek");

  if (p == (Pix) NULL)
    return retval;

  long origin = SEEK_SET;
  long offset = 0;
  if (args(2).is_scalar_type ())
    {
      offset = (long) args(2).double_value ();
      if ((double) NINT (offset) != offset)
	{
	  error ("fseek: offset not an integer value");
	  return retval;
	}
    }

  if (nargin == 4 && args(3).is_scalar_type ())
    {
      origin = (long) args(3).double_value ();
      if (origin == -1)
	origin = SEEK_CUR;
      else if (origin == -2)
	origin = SEEK_END;
      else
	{
	  if ((double) NINT (origin) != origin)
	    {
	      error ("fseek: origin not an integer value");
	      return retval;
	    }
	}
    }

  file_info file = file_list (p);
  int success = fseek (file.fptr (), offset, origin);
  retval.resize (1);

  if (success == 0)
    retval(0) = tree_constant (1.0); // succeeded
  else
    {
      error ("fseek: file error");
      retval(0) = tree_constant (0.0); // failed
    }

  return retval;
}

Octave_object
ftell_internal (const Octave_object& args)
{
  Octave_object retval;

  Pix p = file_io_get_file (args(1), "a+", "ftell");

  if (p != (Pix) NULL)
    {
      file_info file = file_list (p);
      long offset = ftell (file.fptr ());
      retval.resize (1);
      retval(0) = tree_constant ((double) offset);

      if (offset == -1L)
	error ("ftell: write error");
    }

  return retval;
}

void
close_files (void)
{
  Pix p = file_list.first ();

  for (int i = 0; i < file_count; i++)
    {
      file_info file = file_list (p);
      if (i > 2)   // do not close stdin, stdout, stderr!
	{
	  int success = fclose (file.fptr ());
	  if (success != 0)
	    error ("closing %s", file.name ());
	}
      file_list.del (p);
    }
}

static int
process_printf_format (const char *s, const Octave_object& args,
		       ostrstream& sb, const char *type)
{
  ostrstream fmt;

  int nargin = args.length ();

  fmt << "%";  // do_printf() already blew past this one...

  tree_constant_rep::constant_type arg_type;

  int chars_from_fmt_str = 0;

 again:
  switch (*s)
    {
    case '+': case '-': case ' ': case '0': case '#':
      chars_from_fmt_str++;
      fmt << *s++;
      goto again;

    case '\0':
      goto invalid_format;

    default:
      break;
    }

  if (*s == '*')
    {
      if (fmt_arg_count >= nargin)
	{
	  error ("%s: not enough arguments", type);
	  return -1;
	}

      if (args(fmt_arg_count).const_type ()
	  != tree_constant_rep::scalar_constant)
	{
	  error ("%s: `*' must be replaced by an integer", type);
	  return -1;
	}

      fmt << NINT (args(fmt_arg_count++).double_value ());
      s++;
      chars_from_fmt_str++;
    }
  else
    {
      while (*s != '\0' && isdigit (*s))
	{
	  chars_from_fmt_str++;
	  fmt << *s++;
	}
    }

  if (*s == '\0')
    goto invalid_format;

  if (*s == '.')
    {
      chars_from_fmt_str++;
      fmt << *s++;
    }

  if (*s == '*')
    {
      if (*(s-1) == '*')
	goto invalid_format;

      if (fmt_arg_count >= nargin)
	{
	  error ("%s: not enough arguments", type);
	  return -1;
	}

      if (args(fmt_arg_count).const_type ()
	  != tree_constant_rep::scalar_constant)
	{
	  error ("%s: `*' must be replaced by an integer", type);
	  return -1;
	}

      fmt << NINT (args(fmt_arg_count++).double_value ());
      s++;
      chars_from_fmt_str++;
    }
  else
    {
      while (*s != '\0' && isdigit (*s))
	{
	  chars_from_fmt_str++;
	  fmt << *s++;
	}
    }

  if (*s == '\0')
    goto invalid_format;

  if (*s != '\0' && (*s == 'h' || *s == 'l' || *s == 'L'))
    {
      chars_from_fmt_str++;
      fmt << *s++;
    }

  if (*s == '\0')
    goto invalid_format;

  if (fmt_arg_count >= nargin)
    {
      error ("%s: not enough arguments", type);
      return -1;
    }

  arg_type = args(fmt_arg_count).const_type ();

  switch (*s)
    {
    case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':

      if (arg_type != tree_constant_rep::scalar_constant)
	goto invalid_conversion;
      else
	{
	  chars_from_fmt_str++;
	  fmt << *s << ends;
	  double d = args(fmt_arg_count++).double_value ();
	  if ((int) d != d)
	    goto invalid_conversion;
	  else
	    {
	      char *s = fmt.str ();
	      sb.form (s, (int) d);
	      delete [] s;
	      return chars_from_fmt_str;
	    }
	}

    case 'e': case 'E': case 'f': case 'g': case 'G':

      if (arg_type != tree_constant_rep::scalar_constant)
	goto invalid_conversion;
      else
	{
	  chars_from_fmt_str++;
	  fmt << *s << ends;
	  char *s = fmt.str ();
	  sb.form (s, args(fmt_arg_count++).double_value ());
	  delete [] s;
	  return chars_from_fmt_str;
	}

    case 's':

      if (arg_type != tree_constant_rep::string_constant)
	goto invalid_conversion;
      else
	{
	  chars_from_fmt_str++;
	  fmt << *s << ends;
	  char *s = fmt.str ();
	  sb.form (s, args(fmt_arg_count++).string_value ());
	  delete [] s;
	  return chars_from_fmt_str;
	}

    case 'c':

      if (arg_type != tree_constant_rep::string_constant)
	goto invalid_conversion;
      else
	{
	  chars_from_fmt_str++;
	  fmt << *s << ends;
	  char *str = args(fmt_arg_count++).string_value ();
	  if (strlen (str) != 1)
	    goto invalid_conversion;
	  else
	    {
	      char *s = fmt.str ();
	      sb.form (s, *str);
	      delete [] s;
	      return chars_from_fmt_str;
	    }
	}

    default:
      goto invalid_format;
   }

 invalid_conversion:
  error ("%s: invalid conversion", type);
  return -1;

 invalid_format:
  error ("%s: invalid format", type);
  return -1;
}


Octave_object
do_printf (const char *type, const Octave_object& args, int nargout)
{
  Octave_object retval;
  fmt_arg_count = 1;
  char *fmt;
  file_info file;

  if (strcmp (type, "fprintf") == 0)
    {
      if (args(2).is_string_type ())
	{
	  fmt = args(2).string_value ();
	  fmt_arg_count++;
	}
      else
	{
	  error ("%s: format must be a string", type);
	  return retval;
	}

      Pix p = file_io_get_file (args(1), "a+", type);

      if (p == (Pix) NULL)
	return retval;

      file = file_list (p);

      if (file.mode () == "r")
	{
	  error ("%s: file is read only", type);
	  return retval;
	}

      fmt = args(2).string_value ();

      fmt_arg_count++;
    }
  else if (args(1).is_string_type ())
    {
      fmt = args(1).string_value ();
      fmt_arg_count++;
    }
  else
    {
      error ("%s: invalid format string", type);
      return retval;
    }

// Scan fmt for % escapes and print out the arguments.

  ostrstream output_buf;

  char *ptr = fmt;

  for (;;)
    {
      char c;
      while ((c = *ptr++) != '\0' && c != '%')
	output_buf << c;

      if (c == '\0')
	break;

      if (*ptr == '%')
	{
	  ptr++;
	  output_buf << c;
	  continue;
	}

// We must be looking at a format specifier.  Extract it or fail.


      int status = process_printf_format (ptr, args, output_buf, type);

      if (status < 0)
	return retval;

      ptr += status;
    }

  output_buf << ends;
  if (strcmp (type, "printf") == 0
      || (strcmp (type, "fprintf") == 0 && file.number () == 1))
    {
      maybe_page_output (output_buf);
    }
  else if (strcmp (type, "fprintf") == 0)
    {
      char *msg = output_buf.str ();
      int success = fputs (msg, file.fptr ());
      if (success == EOF)
	warning ("%s: unknown failure writing to file", type);
      delete [] msg;
    }
  else if (strcmp (type, "sprintf") == 0)
    {
      retval.resize (1);
      char *msg = output_buf.str ();
      retval(0) = tree_constant (msg);
      delete [] msg;
    }

  return retval;
}

static int
process_scanf_format (const char *s, ostrstream& fmt,
		      const char *type, int nargout, FILE* fptr,
		      Octave_object& values)
{
  fmt << "%";

  int chars_from_fmt_str = 0;
  int store_value = 1;
  int string_width = -1;
  int success = 1;

  if (*s == '*')
    {
      store_value = 0;
      s++;
      chars_from_fmt_str++;
    }

  if (isdigit (*s))
    {
      ostrstream str_number;
      while (*s != '\0' && isdigit (*s))
	{
	  chars_from_fmt_str++;
	  str_number << *s;
	  fmt << *s++;
	}
      str_number << ends;
      char *number = str_number.str ();
      string_width = atoi (number);
      delete [] number;
    }

  if (*s == '\0')
    goto invalid_format;

  if (*s != '\0' && (*s == 'h' || *s == 'l' || *s == 'L'))
    {
      chars_from_fmt_str++;
      s++;
    }

  if (*s == '\0')
    goto invalid_format;

// Even if we don't have a place to store them, attempt to convert
// everything specified by the format string.

  if (fmt_arg_count >= (nargout ? nargout : 1))
    store_value = 0;

  switch (*s)
    {
    case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':
      {
	chars_from_fmt_str++;
	fmt << *s << ends;
	int temp;
	char *str = fmt.str ();
	success = fscanf (fptr, str, &temp);
	delete [] str;
	if (success > 0 && store_value)
	  values(fmt_arg_count++) = tree_constant ((double) temp);
      }
      break;
    case 'e': case 'E': case 'f': case 'g': case 'G':
      {
	chars_from_fmt_str++;
	fmt << 'l' << *s << ends;
	double temp;
	char *str = fmt.str ();
	success = fscanf (fptr, str, &temp);
	delete [] str;
	if (success > 0 && store_value)
	  values(fmt_arg_count++) = tree_constant (temp);
      }
      break;
    case 's':
      {
	if (string_width < 1)
	  {
	    string_width = 0;
	    long original_position = ftell (fptr);
	    int c;

	    while ((c = getc (fptr)) != EOF
		   && (c == ' ' || c == '\n' || c != '\t'))
	      ; // Don't count leading whitespace.

	    if (c != EOF)
	      string_width++;

	    for (;;)
	      {
		c = getc (fptr);
		if (c != EOF && c != ' ' && c != '\n' && c != '\t')
		  string_width++;
		else
		  break;
	      }

	    fseek (fptr, original_position, SEEK_SET);
	  }
	chars_from_fmt_str++;
	char temp[string_width+1];
	fmt << *s << ends;
	char *str = fmt.str ();
	success = fscanf (fptr, str, temp);
	delete [] str;
	if (success && store_value)
	  values(fmt_arg_count++) = tree_constant (temp);
      }
      break;
    case 'c':
      {
	if (string_width < 1)
	  string_width = 1;
	chars_from_fmt_str++;
	char temp[string_width+1];
	memset (temp, '\0', string_width+1);
	fmt << *s << ends;
	char *str = fmt.str ();
	success = fscanf (fptr, str, temp);
	delete [] str;
	temp[string_width] = '\0';
	if (success > 0 && store_value)
	  values(fmt_arg_count++) = tree_constant (temp);
      }
      break;
    default:
      goto invalid_format;
    }

  if (success > 0)
    return chars_from_fmt_str;
  else if (success == 0)
    warning ("%s: invalid conversion", type);
  else if (success == EOF)
    {
      if (strcmp (type, "fscanf") == 0)
	warning ("%s: end of file reached before final conversion", type);
      else if (strcmp (type, "sscanf") == 0)
	warning ("%s: end of string reached before final conversion", type);
      else if (strcmp (type, "scanf") == 0)
	warning ("%s: end of input reached before final conversion", type);
    }
  else
    {
    invalid_format:
      warning ("%s: invalid format", type);
    }

  return -1;
}

Octave_object
do_scanf (const char *type, const Octave_object& args, int nargout)
{
  Octave_object retval;
  char *scanf_fmt = (char *) NULL;
  char *tmp_file = (char *) NULL;
  int tmp_file_open = 0;
  FILE *fptr = (FILE *) NULL;
  file_info file;

  fmt_arg_count = 0;

  if (strcmp (type, "scanf") != 0)
    {
      if (args(2).is_string_type ())
	scanf_fmt = args(2).string_value ();
      else
	{
	  error ("%s: format must be a string", type);
	  return retval;
	}
    }

  int doing_fscanf = (strcmp (type, "fscanf") == 0);

  if (doing_fscanf)
    {
      Pix p = file_io_get_file (args(1), "r", type);

      if (p == (Pix) NULL)
	return retval;

      file = file_list (p);

      if (strcmp (file.mode (), "w") == 0 || strcmp (file.mode (), "a") == 0)
	{
	  error ("%s: this file is opened for writing only", type);
	  return retval;
	}

      fptr = file.fptr ();
    }

  if ((fptr == (FILE *) NULL && args(1).is_string_type ())
      || (doing_fscanf && file.number () == 0))
    {
      char *string;

      if (strcmp (type, "scanf") == 0)
	scanf_fmt = args(1).string_value ();

      if (strcmp (type, "scanf") == 0
	  || (doing_fscanf && file.number () == 0))
	{
	  string = gnu_readline ("");
	  if (string && *string)
	    maybe_save_history (string);
	}
      else
	string = args(1).string_value ();

      tmp_file = tmpnam ((char *) NULL);

      fptr = fopen (tmp_file, "w+");
      if (fptr == (FILE *) NULL)
	{
	  error ("%s: error opening temporary file", type);
	  return retval;
	}
      tmp_file_open = 1;
      unlink (tmp_file);

      if (string == (char *) NULL)
	panic_impossible ();

      int success = fputs (string, fptr);
      fflush (fptr);
      rewind (fptr);

      if (success < 0)
	{
	  error ("%s: trouble writing temporary file", type);
	  fclose (fptr);
	  return retval;
	}
    }
  else if (! doing_fscanf)
    {
      error ("%s: first argument must be a string", type);
      return retval;
    }

// Scan scanf_fmt for % escapes and assign the arguments.

  retval.resize (nargout ? nargout : 1);

  char *ptr = scanf_fmt;

  for (;;)
    {
      ostrstream fmt;
      char c;
      while ((c = *ptr++) != '\0' && c != '%')
	fmt << c;

      if (c == '\0')
	break;

      if (*ptr == '%')
	{
	  ptr++;
	  fmt << c;
	  continue;
	}

// We must be looking at a format specifier.  Extract it or fail.

      int status = process_scanf_format (ptr, fmt, type, nargout,
					 fptr, retval);

      if (status < 0)
	{
	  if (fmt_arg_count == 0)
	    retval.resize (0);
	  break;
	}

      ptr += status;
    }

  if (tmp_file_open)
    fclose (fptr);

  return retval;
}

/*
 * Find out how many elements are left to read.
 */
static long
num_items_remaining (FILE *fptr, char *type)
{
  size_t size;

  if (strcasecmp (type, "uchar") == 0)
    size = sizeof (u_char);
  else if (strcasecmp (type, "char") == 0)
    size = sizeof (char);
  else if (strcasecmp (type, "short") == 0)
    size = sizeof (short);
  else if (strcasecmp (type, "ushort") == 0)
    size = sizeof (u_short);
  else if (strcasecmp (type, "int") == 0)
    size = sizeof (int);
  else if (strcasecmp (type, "uint") == 0)
    size = sizeof (u_int);
  else if (strcasecmp (type, "long") == 0)
    size = sizeof (long);
  else if (strcasecmp (type, "ulong") == 0)
    size = sizeof (u_long);
  else if (strcasecmp (type, "float") == 0)
    size = sizeof (float);
  else if (strcasecmp (type, "double") == 0)
    size = sizeof (double);
  else
    return 0;

  long curr_pos = ftell (fptr);

  fseek (fptr, 0, SEEK_END);
  long end_of_file = ftell (fptr);

  fseek (fptr, curr_pos, SEEK_SET);

  long len = end_of_file - curr_pos;

  return len / size;
}

/*
 * Read binary data from a file.
 *
 *   [data, count] = fread (fid, size, 'precision')
 *
 *     fid       : the file id from fopen
 *     size      : the size of the matrix or vector or scaler to read
 *
 *                 n	  : reads n elements of a column vector
 *                 inf	  : reads to the end of file (default)
 *                 [m, n] : reads enough elements to fill the matrix
 *                          the number of columns can be inf
 *
 *     precision : type of the element.  Can be:
 *
 *                 char, uchar, schar, short, ushort, int, uint,
 *                 long, ulong, float, double
 *
 *                 Default  is uchar.
 *
 *     data	 : output data
 *     count	 : number of elements read
 */
Octave_object
fread_internal (const Octave_object& args, int nargout)
{
  Octave_object retval;

  int nargin = args.length ();

  Pix p = file_io_get_file (args(1), "r", "fread");

  if (p == (Pix) NULL)
    return retval;

// Get type and number of bytes per element to read.
  char *prec = "uchar";
  if (nargin > 3)
    {
      if (args(3).is_string_type ())
	prec = args(3).string_value ();
      else
	{
	  error ("fread: precision must be a specified as a string");
	  return retval;
	}
    }

// Get file info.

  file_info file = file_list (p);

  FILE *fptr = file.fptr ();

// Set up matrix to read into.  If specified in arguments use that
// number, otherwise read everyting left in file.

  double dnr = 0.0;
  double dnc = 0.0;
  int nr;
  int nc;

  if (nargin > 2)
    {
      if (args(2).is_scalar_type ())
	{
	  tree_constant tmpa = args(2).make_numeric ();
	  dnr = tmpa.double_value ();
	  dnc = 1.0;
	}
      else if (args(2).is_matrix_type ())
	{
	  ColumnVector tmp = args(2).to_vector ();

	  if (tmp.length () == 2)
	    {
	      dnr = tmp.elem (0);
	      dnc = tmp.elem (1);
	    }
	  else
	    {
	      error ("fread: invalid size specification\n");
	      return retval;
	    }
	}

      if ((xisinf (dnr)) && (xisinf (dnc)))
	{
	  error ("fread: number of rows and columns cannot both be infinite");
	  return retval;
	}

      if (xisinf (dnr))
	{
	  nc = NINT (dnc);
	  int n = num_items_remaining (fptr, prec);
	  nr = n / nc;
	  if (n > nr * nc)
	    nr++;
	}
      else if (xisinf (dnc))
	{
	  nr = NINT (dnr);
	  int n = num_items_remaining (fptr, prec);
	  nc = n / nr;
	  if (n > nc * nr)
	    nc++;
	}
      else
	{
	  nr = NINT (dnr);
	  nc = NINT (dnc);
	}
    }
  else
    {
// No size parameter, read what's left of the file.
      nc = 1;
      int n = num_items_remaining (fptr, prec);
      nr = n / nc;
      if (n > nr * nc)
	nr++;
    }

  Matrix m (nr, nc, octave_NaN);

// Read data.

  int count = m.read (fptr, prec);

  if (nargout > 1)
    {
      retval.resize (2);
      retval(1) = tree_constant ((double) count);
    }
  else
    retval.resize (1);

  retval(0) = tree_constant (m);

  return retval;
}

/*
 * Write binary data to a file.
 *
 *   count = fwrite (fid, data, 'precision')
 *
 *    fid	: file id from fopen
 *    Data	: data to be written
 *    precision	: type of output element.  Can be:
 *
 *                char, uchar, schar, short, ushort, int, uint,
 *                long, float, double
 *
 *                 Default is uchar.
 *
 *    count     : the number of elements written
 */
Octave_object
fwrite_internal (const Octave_object& args, int nargout)
{
  Octave_object retval;

  int nargin = args.length ();

  Pix p = file_io_get_file (args(1), "a+", "fwrite");

  if (p == (Pix) NULL)
    return retval;

// Get type and number of bytes per element to read.
  char *prec = "uchar";
  if (nargin > 3)
    {
      if (args(3).is_string_type ())
	prec = args(3).string_value ();
      else
	{
	  error ("fwrite: precision must be a specified as a string");
	  return retval;
	}
    }

  file_info file = file_list (p);

  Matrix m = args(2).to_matrix ();

  int count = m.write (file.fptr (), prec);

  retval.resize (1);
  retval(0) = tree_constant ((double) count);

  return retval;
}

/*
 * Check for an EOF condition on a file opened by fopen.
 *
 *   eof = feof (fid)
 *
 *     fid : file id from fopen
 *     eof : non zero for an end of file condition
 */
Octave_object
feof_internal (const Octave_object& args, int nargout)
{
  Octave_object retval;

// Get file info.
  Pix p = return_valid_file (args(1));

  if (p == (Pix) NULL)
    return retval;

  file_info file = file_list (p);

  retval.resize (1);
  retval(0) = tree_constant (feof (file.fptr ()));

  return retval;
}

/*
 * Check for an error condition on a file opened by fopen.
 *
 *   [message, errnum] = ferror (fid)
 *
 *     fid     : file id from fopen
 *     message : system error message
 *     errnum  : error number
 */
Octave_object
ferror_internal (const Octave_object& args, int nargout)
{
  Octave_object retval;

// Get file info.
  Pix p = return_valid_file (args(1));

  if (p == (Pix) NULL)
    return retval;

  file_info file = file_list (p);

  int ierr = ferror (file.fptr ());

  if (nargout > 1)
    {
      retval.resize (2);
      retval(1) = tree_constant ((double) ierr);
    }
  else
    retval.resize (1);

  retval(0) = tree_constant (strsave (strerror (ierr)));

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
