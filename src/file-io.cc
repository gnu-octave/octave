// file-io.cc                                             -*- C++ -*-
/*

Copyright (C) 1993 John W. Eaton

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

#ifdef __GNUG__
#pragma implementation
#endif

#include <DLList.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <strstream.h>
#include <ctype.h>

#include "statdefs.h"
#include "file-io.h"
#include "input.h"
#include "octave-hist.h"
#include "tree-const.h"
#include "error.h"
#include "utils.h"
#include "pager.h"

// keeps a count of how many files are open and in the file list
static int file_count = 0;

// keeps a count of args sent to printf or scanf
static int fmt_arg_count = 0;

class File_info
{
 public:
  File_info (void);
  File_info (int num, char *nm, FILE *t, char *md);
  File_info (const File_info& f);

  File_info& operator = (const File_info& f);

  ~File_info (void);

  int number (void) const;
  char *name (void) const;
  FILE *fptr (void) const;
  char *mode (void) const;

 private:
  int _number;
  char *_name;
  FILE *_fptr;
  char *_mode;
};

File_info::File_info (void)
{
  _number = -1;
  _name = (char *) NULL;
  _fptr = (FILE *) NULL;
  _mode = (char *) NULL;
}

File_info::File_info (const File_info& f)
{
  _number = f._number;
  _name = strsave (f._name);
  _fptr = f._fptr;
  _mode = strsave (f._mode);
}

File_info&
File_info::operator = (const File_info& f)
{
  _number = f._number;
  _name = strsave (f._name);
  _fptr = f._fptr;
  _mode = strsave (f._mode);

  return *this;
}

File_info::~File_info (void)
{
  delete [] _name;
  delete [] _mode;
}

File_info::File_info (int n, char *nm, FILE *t, char *md)
{
  _number = n;
  _name = strsave (nm);
  _fptr = t;
  _mode = strsave (md);
}

int
File_info::number (void) const
{
  return _number;
}

char *
File_info::name (void) const
{
  return _name;
}

FILE *
File_info::fptr (void) const
{
  return _fptr;
}

char *
File_info::mode (void) const
{
  return _mode;
}


// double linked list containing relevant information about open files
static DLList <File_info> file_list;

void
initialize_file_io ()
{
  File_info _stdin (0, "stdin", stdin, "r");
  File_info _stdout (1, "stdout", stdout, "w");
  File_info _stderr (2, "stderr", stderr, "w");

  file_list.append (_stdin);
  file_list.append (_stdout);
  file_list.append (_stderr);

  file_count = 3;
}

Pix
return_valid_file (tree_constant& arg)
{
  if (arg.is_string_type ())
    {
      Pix p = file_list.first ();
      File_info file;
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
	  File_info file;
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
fopen_file_for_user (tree_constant& arg, char *mode)
{
  char *file_name = arg.string_value ();

  FILE *file_ptr = fopen (file_name, mode);
  if (file_ptr != (FILE *) NULL)
    {
      File_info file (++file_count, file_name, file_ptr, mode);
      file_list.append (file);
      
      Pix p = file_list.first ();
      File_info file_from_list;
      
      for (int i = 0; i < file_count; i++)
	{
	  file_from_list = file_list (p);
	  if (strcmp (file_from_list.name (), file_name) == 0)
	    return p;
	  file_list.next (p);
	}
    }

  error ("problems automatically opening file for user");
  return (Pix) NULL;
}


tree_constant *
fclose_internal (tree_constant *args)
{
  tree_constant *retval = NULL_TREE_CONST;

  Pix p = return_valid_file (args[1]);

  if (p == (Pix) NULL)
    return retval;

  File_info file = file_list (p);

  if (file.number () < 3)
    {
      warning ("fclose: can't close stdin, stdout, or stderr!");
      return retval;
    }

  int success = fclose (file.fptr ());
  file_list.del (p);
  file_count--;

  retval = new tree_constant[2];
  if (success == 0)
    retval[0] = tree_constant (1.0); // succeeded
  else
    {
      error ("fclose: error on closing file");
      retval[0] = tree_constant (0.0); // failed
    }

  return retval;
}

tree_constant *
fflush_internal (tree_constant *args)
{
  tree_constant *retval = NULL_TREE_CONST;

  Pix p = return_valid_file (args[1]);

  if (p == (Pix) NULL)
    return retval;

  File_info file = file_list (p);

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

  retval = new tree_constant[2];
  if (success == 0)
    retval[0] = tree_constant (1.0); // succeeded
  else
    {
      error ("fflush: write error");
      retval[0] = tree_constant (0.0); // failed
    }

  return retval;
}

static int
valid_mode (char *mode)
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

tree_constant *
fgets_internal (tree_constant *args, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  Pix p = return_valid_file (args[1]);
  
  if (p == (Pix) NULL)
    {
      if (args[1].is_string_type ())
	{
	  struct stat buffer;
	  char *name = args[1].string_value ();
	  if (stat (name, &buffer) == 0
	      && (buffer.st_mode & S_IFREG) == S_IFREG)
	    {
	      p = fopen_file_for_user (args[1], "r");
	    }
	  else
	    {
	      error ("fgets: file dosen't exist");
	      return retval;
	    }
	}
      else
	return retval;
    }
  
  int length = 0;

  if (args[2].is_scalar_type ())
    {
      length = (int) args[2].double_value ();
      if ((double) NINT (length) != length)
	{
	  error ("fgets: length not an integer value");
	  return retval;
	}
    }

  char string[length+1];
  File_info file = file_list (p);
  char *success = fgets (string, length+1, file.fptr ());

  if (success == (char *) NULL)
    {
      retval = new tree_constant[2];
      retval[0] = tree_constant (-1.0);
      return retval;
    }

  if (nargout == 2)
    {
      retval = new tree_constant[3];
      retval[1] = tree_constant ((double) strlen (string));
    }
  else
    retval = new tree_constant[2];

  retval[0] = tree_constant (string);

  return retval;
}

tree_constant *
fopen_internal (tree_constant *args)
{
  tree_constant *retval = NULL_TREE_CONST;
  Pix p;

  if (! args[1].is_string_type ())
    {
      error ("fopen: file name must be a string");
      return retval;
    }

  p = return_valid_file (args[1]);

  if (p != (Pix) NULL)
    {
      File_info file = file_list (p);

      retval = new tree_constant[2];
      retval[0] = tree_constant ((double) file.number ());

      return retval;
    }

  if (! args[2].is_string_type ())
    {
      error ("fopen: mode must be a string");
      return retval;
    }

  char *name = args[1].string_value ();
  char *mode = args[2].string_value ();

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
      error ("fopen: file does not exist");
      return retval;
    }

  int number = file_count++;

  File_info file (number, name, file_ptr, mode);
  file_list.append (file);

  retval = new tree_constant[2];
  retval[0] = tree_constant ((double) number);

  return retval;
}

tree_constant *
freport_internal ()
{
  tree_constant *retval = NULL_TREE_CONST;
  Pix p = file_list.first ();

  ostrstream output_buf;

  output_buf << "\n number  mode  name\n\n";
  for (int i = 0; i < file_count; i++)
    {
      File_info file = file_list (p);
      output_buf.form ("%7d%6s  %s\n", file.number (), file.mode (),
		       file.name ());
      file_list.next (p);
    }

  output_buf << "\n" << ends;
  maybe_page_output (output_buf);

  return retval;
}

tree_constant *
frewind_internal (tree_constant *args)
{
  tree_constant *retval = NULL_TREE_CONST;

  Pix p = return_valid_file (args[1]);
  if (p == (Pix) NULL)
    p = fopen_file_for_user (args[1], "a+");   

  File_info file = file_list (p);
  rewind (file.fptr ());

  return retval;
}

tree_constant *
fseek_internal (tree_constant *args, int nargin)
{
  tree_constant *retval = NULL_TREE_CONST;

  Pix p = return_valid_file (args[1]);

  if (p == (Pix) NULL)
    p = fopen_file_for_user (args[1], "a+");

  long origin = SEEK_SET;
  long offset = 0;
  if (args[2].is_scalar_type ())
    {
      offset = (long) args[2].double_value ();
      if ((double) NINT (offset) != offset)
	{
	  error ("fseek: offset not an integer value");
	  return retval;
	}
    }

  if (nargin == 4 && args[3].is_scalar_type ())
    {
      origin = (long) args[3].double_value ();
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

  File_info file = file_list (p);
  int success = fseek (file.fptr (), offset, origin);
  retval = new tree_constant[2];

  if (success == 0)
    retval[0] = tree_constant (1.0); // succeeded
  else
    {
      error ("fseek: file error");
      retval[0] = tree_constant (0.0); // failed
    }

  return retval;
}

tree_constant *
ftell_internal (tree_constant *args)
{
  tree_constant *retval = NULL_TREE_CONST;
  Pix p = return_valid_file (args[1]);

  if (p == (Pix) NULL)
    p = fopen_file_for_user (args[1], "a+");

  File_info file = file_list (p);
  long offset = ftell (file.fptr ());
  retval = new tree_constant[2];
  retval[0] = tree_constant ((double) offset);

  if (offset == -1L)
    error ("ftell: write error");

  return retval;
}

void
close_files ()
{
  Pix p = file_list.first ();

  for (int i = 0; i < file_count; i++)
    {
      File_info file = file_list (p);
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
process_printf_format (char *s, tree_constant *args, ostrstream& sb,
		       char *type, int nargin)
{
  ostrstream fmt;

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
	  message (type, "not enough arguments");
	  return -1;
	}

      if (args[fmt_arg_count].const_type ()
	  != tree_constant_rep::scalar_constant)
	{
	  message (type, "`*' must be replaced by an integer");
	  return -1;
	}

      fmt << NINT (args[fmt_arg_count++].double_value ());
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
	  message (type, "not enough arguments");
	  return -1;
	}

      if (args[fmt_arg_count].const_type ()
	  != tree_constant_rep::scalar_constant)
	{
	  message (type, "`*' must be replaced by an integer");
	  return -1;
	}

      fmt << NINT (args[fmt_arg_count++].double_value ());
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
      message (type, "not enough arguments");
      return -1;
    }

  arg_type = args[fmt_arg_count].const_type ();

  switch (*s)
    {
    case 'd': case 'i': case 'o': case 'u': case 'x': case 'X':

      if (arg_type != tree_constant_rep::scalar_constant)
	goto invalid_conversion;
      else
	{
	  chars_from_fmt_str++;
	  fmt << *s << ends;
	  double d = args[fmt_arg_count++].double_value ();
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
	  sb.form (s, args[fmt_arg_count++].double_value ());
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
	  sb.form (s, args[fmt_arg_count++].string_value ());
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
	  char *str = args[fmt_arg_count++].string_value ();
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
  message (type, "invalid conversion");
  return -1;

 invalid_format:
  message (type, "invalid format");
  return -1;
}


tree_constant *
do_printf (char *type, tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  fmt_arg_count = 1;
  char *fmt;
  File_info file;

  if (strcmp (type, "fprintf") == 0)
    {
      Pix p;

      if (args[2].is_string_type ())
	{
	  fmt = args[2].string_value ();
	  fmt_arg_count++;
	}
      else
	{
	  error ("%s: format must be a string", type);
	  return retval;
	}

      if (args[1].is_scalar_type ())
	{
	  p = return_valid_file (args[1]);
	  if (p == (Pix) NULL)
	    return retval;
	}
      else if (args[1].is_string_type ())
	{
	  p = return_valid_file (args[1]);
	  if (p == (Pix) NULL)
	    p = fopen_file_for_user (args[1], "a+");
	}
      else
	  {
	    error ("%s: illegal file specifier", type);
	    return retval;
	  }

      file = file_list (p);
      if (file.mode () == "r")
	{
	  error ("%s: file is read only", type);
	  return retval;
	}
      fmt = args[2].string_value ();
      fmt_arg_count++;
    }
  else if (args[1].is_string_type ())
    {
      fmt = args[1].string_value ();
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


      int status = process_printf_format (ptr, args, output_buf, type,
					  nargin);

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
	error ("%s: writing to file", type);
      delete [] msg;
    }
  else if (strcmp (type, "sprintf") == 0)
    {
      retval = new tree_constant [2];
      char *msg = output_buf.str ();
      retval[0] = tree_constant (msg);
      delete [] msg;
    }

  return retval;
}

static int
process_scanf_format (char *s, tree_constant *args, ostrstream& fmt,
		      char *type, int nargout, FILE* fptr,
		      tree_constant *values)
{
  fmt << "%";

  tree_constant_rep::constant_type arg_type;

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

  if (fmt_arg_count >= nargout && store_value)
    {
      message (type, "not enough arguments");
      return -1;
    }

  arg_type = args[fmt_arg_count].const_type ();

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
	  values[fmt_arg_count++] = tree_constant ((double) temp);
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
	  values[fmt_arg_count++] = tree_constant (temp);
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
	  values[fmt_arg_count++] = tree_constant (temp);
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
	  values[fmt_arg_count++] = tree_constant (temp);
      }
      break;
    default:
      goto invalid_format;
    }

  if (success > 0 || (success == 0 && store_value == 0))
    return chars_from_fmt_str;

  if (success == 0)
    message (type, "invalid conversion");
  else if (success == EOF)
    {
      if (strcmp (type, "fscanf") == 0)
	message (type, "end of file reached before final conversion");
      else if (strcmp (type, "sscanf") == 0)
	message (type, "end of string reached before final conversion");
      else if (strcmp (type, "scanf") == 0)
	message (type, "end of input reached before final conversion");
    }
  else
    {
    invalid_format:
      message (type, "invalid format");
    }

  return -1;
}

tree_constant *
do_scanf (char *type, tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  char *scanf_fmt = (char *) NULL;
  char *tmp_file = (char *) NULL;
  int tmp_file_open = 0;
  FILE *fptr = (FILE *) NULL;
  File_info file;

  fmt_arg_count = 0;

  if (strcmp (type, "scanf") != 0)
    {
      if ( args[2].is_string_type ())
	scanf_fmt = args[2].string_value ();
      else
	{
	  error ("%s: format must be a string", type);
	  return retval;
	}
    }

  int doing_fscanf = (strcmp (type, "fscanf") == 0);

  if (doing_fscanf)
    {
      Pix p;
      if (args[1].is_scalar_type ()
	  || args[1].is_string_type ())
	{
	  p = return_valid_file (args[1]);
	  if (p == (Pix) NULL)
	    return retval;
	}
      else
	{
	  error ("%s: illegal file specifier", type);
	  return retval;
	}

      file = file_list (p);

      if (strcmp (file.mode (), "w") == 0 || strcmp (file.mode (), "a") == 0)
	{
	  error ("%s: this file is opened for writing only", type);
	  return retval;
	}

      fptr = file.fptr ();
    }

  if (args[1].is_string_type () || (doing_fscanf && file.number () == 0))
    {
      char *string;

      if (strcmp (type, "scanf") == 0)
	scanf_fmt = args[1].string_value ();

      if (strcmp (type, "scanf") == 0
	  || (doing_fscanf && file.number () == 0))
	{
	  string = gnu_readline ("");
	  if (string && *string)
	    maybe_save_history (string);
	}
      else
	string = args[1].string_value ();

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

  retval = new tree_constant[nargout+1];

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

      int status = process_scanf_format (ptr, args, fmt, type,
					 nargout, fptr, retval);

      if (status < 0)
	{
	  if (fmt_arg_count == 0)
	    {
	      delete [] retval;
	      retval = NULL_TREE_CONST;
	    }
	  break;
	}

      ptr += status;
    }

  if (tmp_file_open)
    fclose (fptr);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
