// utils.cc                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

/*

The 11 functions listed below were adapted from a similar functions
from GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991
Free Software Foundation, Inc.

  polite_directory_format  absolute_pathname
  absolute_program         base_pathname
  read_octal               sub_append_string
  decode_prompt_string     pathname_backup
  make_absolute            get_working_directory
  change_to_directory

*/

#ifdef __GNUG__
#pragma implementation
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/param.h>
#include <setjmp.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <limits.h>
#include <iostream.h>
#include <strstream.h>
#include <fstream.h>
#include <dirent.h>

#define NLENGTH(dirent) (strlen((dirent)->d_name))

extern "C"
{
#if defined (HAVE_TERMIOS_H)
#include <termios.h>
#elif defined (HAVE_TERMIO_H)
#include <termio.h>
#elif defined (HAVE_SGTTY_H)
#include <sgtty.h>
#else
LOSE! LOSE!
#endif

extern int ioctl (int, int, ...);
char *tilde_expand (char *s); /* From readline's tilde.c */
}

#include "SLStack.h"

#include "statdefs.h"
#include "procstream.h"
#include "user-prefs.h"
#include "variables.h"
#include "error.h"
#include "utils.h"
#include "input.h"
#include "octave.h"
#include "mappers.h"
#include "version.h"
#include "defaults.h"
#include "tree-const.h"
#include "unwind-prot.h"
#include "octave-hist.h"

#ifndef STDIN_FILENO
#define STDIN_FILENO 1
#endif

// Top level context (?)
extern jmp_buf toplevel;

// Pipe to gnuplot.
static oprocstream plot_stream;

// Non-zero means follow symbolic links that point to directories just
// as if they are real directories.
static int follow_symbolic_links = 1;

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

// The size that strings change by.
#ifndef DEFAULT_ARRAY_SIZE
#define DEFAULT_ARRAY_SIZE 512
#endif

// The growth rate for the prompt string.
#ifndef PROMPT_GROWTH
#define PROMPT_GROWTH 50
#endif

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

// Where to find the site-wide configuration file
#ifndef OCTAVE_HOME
#define OCTAVE_HOME "/usr/local"
#endif

// Temp storage for a path.
static char tdir[MAXPATHLEN];

// List of files to delete when we exit or crash.
static SLStack <char *> tmp_files;

/*
 * Save a string.
 */
char *
strsave (const char *s)
{
  if (s == (char *) NULL)
    return (char *) NULL;

  int len = strlen (s);
  char *tmp = new char [len+1];
  tmp = strcpy (tmp, s);
  return tmp;
}

/*
 * Concatenate two strings.
 */
char *
strconcat (const char *s, const char *t)
{
  int len = strlen (s) + strlen (t);
  char *tmp = new char [len+1];
  strcpy (tmp, s);
  return strcat (tmp, t);
}

/*
 * Throw away input until a given character is read.
 */
void
discard_until (istream& stream, char character)
{
  int c;
  for (;;)
    {
      stream >> c;
      if (c == EOF || c == character)
	break;
    }
  if (c != EOF)
    stream.putback ((char) c);
} 

void
check_dimensions (int& nr, int& nc, const char *warnfor)
{
  if (nr < 0 || nc < 0)
    {
      if (user_pref.treat_neg_dim_as_zero)
	nr = nc = 0;
      else
	error ("%s: can't create a matrix with negative dimensions",
	       warnfor);
    }
}

/*
 * Set terminal in raw mode.  From less-177.
 *
 * Change terminal to "raw mode", or restore to "normal" mode.
 * "Raw mode" means 
 *	1. An outstanding read will complete on receipt of a single keystroke.
 *	2. Input is not echoed.  
 *	3. On output, \n is mapped to \r\n.
 *	4. \t is NOT expanded into spaces.
 *	5. Signal-causing characters such as ctrl-C (interrupt),
 *	   etc. are NOT disabled.
 * It doesn't matter whether an input \n is mapped to \r, or vice versa.
 */
void
raw_mode (int on)
{
  static int curr_on = 0;

  int tty_fd = STDIN_FILENO;
  if (! isatty (tty_fd))
    {
      if (interactive || forced_interactive)
	error ("stdin is not a tty!");
      return;
    }

  if (on == curr_on)
    return;

#if defined (HAVE_TERMIOS_H)
  {
    struct termios s;
    static struct termios save_term;

    if (on)
      {
// Get terminal modes.

	tcgetattr (tty_fd, &s);

// Save modes and set certain variables dependent on modes.

	save_term = s;
//	ospeed = s.c_cflag & CBAUD;
//	erase_char = s.c_cc[VERASE];
//	kill_char = s.c_cc[VKILL];

// Set the modes to the way we want them.

	s.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
	s.c_oflag |=  (OPOST|ONLCR);
	s.c_oflag &= ~(OCRNL|ONOCR|ONLRET);
	s.c_cc[VMIN] = 1;
	s.c_cc[VTIME] = 0;
      }      
    else
      {
// Restore saved modes.
	s = save_term;
      }
    tcsetattr (tty_fd, TCSAFLUSH, &s);
  }
#elif defined (HAVE_TERMIO_H)
  {
    struct termio s;
    static struct termio save_term;

    if (on)
      {
// Get terminal modes.

	ioctl (tty_fd, TCGETA, &s);

// Save modes and set certain variables dependent on modes.

	save_term = s;
//	ospeed = s.c_cflag & CBAUD;
//	erase_char = s.c_cc[VERASE];
//	kill_char = s.c_cc[VKILL];

// Set the modes to the way we want them.

	s.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
	s.c_oflag |=  (OPOST|ONLCR);
	s.c_oflag &= ~(OCRNL|ONOCR|ONLRET);
	s.c_cc[VMIN] = 1;
	s.c_cc[VTIME] = 0;
      }      
    else
      {
// Restore saved modes.
	s = save_term;
      }
    ioctl (tty_fd, TCSETAW, &s);
  }
#elif defined (HAVE_SGTTY_H)
  {
    struct sgttyb s;
    static struct sgttyb save_term;

    if (on)
      {
// Get terminal modes.

	ioctl (tty_fd, TIOCGETP, &s);

// Save modes and set certain variables dependent on modes.

	save_term = s;
//	ospeed = s.sg_ospeed;
//	erase_char = s.sg_erase;
//	kill_char = s.sg_kill;

// Set the modes to the way we want them.

	s.sg_flags |= CBREAK;
	s.sg_flags &= ~(ECHO);
      } 
    else
      {
// Restore saved modes.
	s = save_term;
      }
    ioctl (tty_fd, TIOCSETN, &s);
  }
#else
LOSE! LOSE!
#endif

  curr_on = on;
}

/*
 * Read one character from the terminal.
 */
int
kbhit (void)
{
  int c;
  raw_mode (1);
  c = cin.get ();
  raw_mode (0);
  return c;
}

char **
pathstring_to_vector (char *pathstring)
{
  static char **path = (char **) NULL;

  if (pathstring != (char *) NULL)
    {
      int nelem = 0;
      pathstring = strsave (pathstring);
      if (*pathstring != '\0')
	{
	  nelem++;
	  char *ptr = pathstring;
	  while (*ptr != '\0')
	    {
	      if (*ptr == ':')
		nelem++;
	      ptr++;
	    }
	}

      delete [] path;
      path = new char * [nelem+1];
      path[nelem] = (char *) NULL;

      int i = 0;
      char *ptr = pathstring;
      while (i < nelem)
	{
	  char *end = strchr (ptr, ':');
	  if (end != (char *) NULL)
	    *end = '\0';
	  char *result = tilde_expand (ptr);
	  path[i] = strsave (result);
	  free (result);
	  ptr = end + 1;
	  i++;
	}

      delete [] pathstring;
    }

  return path;
}

static char *
octave_home (void)
{
  static char *home =  (char *) NULL;
  delete [] home;
  char *oh = getenv ("OCTAVE_HOME");
  if (oh != (char *) NULL)
    home = strsave (oh);
  else
    home = strsave (OCTAVE_HOME);
  return home;
}

static char *
octave_lib_dir (void)
{
  static char *ol = (char *) NULL;
  delete [] ol;
  char *oh = octave_home ();
  char *tmp = strconcat (oh, "/lib/octave/");
  ol = strconcat (tmp, version_string);
  return ol;
}

static char *
octave_info_dir (void)
{
  static char *oi = (char *) NULL;
  delete [] oi;
  char *oh = octave_home ();
  oi = strconcat (oh, "/info/");
  return oi;
}

char *
default_path (void)
{
  static char *pathstring = (char *) NULL;
  delete [] pathstring;
  char *oct_path = getenv ("OCTAVE_PATH");
  if (oct_path != (char *) NULL)
    pathstring = strsave (oct_path);
  else
    {
      char *libdir = octave_lib_dir ();
      pathstring = strconcat (".:", libdir);
    }
  return pathstring;
}

char *
default_info_file (void)
{
  static char *info_file_string = (char *) NULL;
  delete [] info_file_string;
  char *oct_info_file = getenv ("OCTAVE_INFO_FILE");
  if (oct_info_file != (char *) NULL)
    info_file_string = strsave (oct_info_file);
  else
    {
      char *infodir = octave_info_dir ();
      info_file_string = strconcat (infodir, "octave.info");
    }
  return info_file_string;
}

char *
get_site_defaults (void)
{
  static char *sd = (char *) NULL;
  delete [] sd;
  char *libdir = octave_lib_dir ();
  sd = strconcat (libdir, "/octaverc");
  return sd;
}

char *
default_pager (void)
{
  static char *pager_binary = (char *) NULL;
  delete [] pager_binary;
  char *pgr = getenv ("PAGER");
  if (pgr != (char *) NULL)
    pager_binary = strsave (pgr);
  else
#ifdef DEFAULT_PAGER
    pager_binary = strsave (DEFAULT_PAGER);
#else
    pager_binary = strsave ("");
#endif

  return pager_binary;
}

/*
 * See if the given file is in the path.
 */
char *
file_in_path (const char *name, const char *suffix)
{
  char *nm = strconcat ("/", name);
  char *tmp = nm;
  if (suffix != (char *) NULL)
    {
      nm = strconcat (tmp, suffix);
      delete [] tmp;
    }

  if (!the_current_working_directory)
    get_working_directory ("file_in_path");

  char **path = pathstring_to_vector (user_pref.loadpath);

  char **ptr = path;
  if (ptr != (char **) NULL)
    {
      while (*ptr != (char *) NULL)
	{
	  char *tmp_p = strconcat (*ptr, nm);
	  char *p = make_absolute (tmp_p, the_current_working_directory);
	  delete [] tmp_p;
	  ifstream in_file (p);
	  if (in_file)
	    {
	      in_file.close ();
	      delete [] nm;
	      return p;
	    }
	  delete [] p;
	  ptr++;
	}
    }

  delete [] nm;
  return (char *) NULL;
}

/*
 * See if there is an M-file in the path.  If so, return the full path
 * to the file.
 */
char *
m_file_in_path (const char *name)
{
  return file_in_path (name, ".m");
}

/*
 * Return a pretty pathname.  If the first part of the pathname is the
 * same as $HOME, then replace that with `~'.
 */
char *
polite_directory_format (char *name)
{
  int l = home_directory ? strlen (home_directory) : 0;

  if (l > 1 && strncmp (home_directory, name, l) == 0
      && (!name[l] || name[l] == '/'))
    {
      strcpy (tdir + 1, name + l);
      tdir[0] = '~';
      return (tdir);
    }
  else
    return name;
}

/*
 * Return 1 if STRING contains an absolute pathname, else 0.
 */
int
absolute_pathname (const char *string)
{
  if (!string || !*string)
    return 0;

  if (*string == '/')
    return 1;

  if (*string++ == '.')
    {
      if ((!*string) || *string == '/')
	return 1;

      if (*string++ == '.')
	if (!*string || *string == '/')
	  return 1;
    }
  return 0;
}

/*
 * Return 1 if STRING is an absolute program name; it is absolute if
 * it contains any slashes.  This is used to decide whether or not to
 * look up through $PATH.
 */
int
absolute_program (const char *string)
{
  return (strchr (string, '/') != (char *)NULL);
}

/*
 * Return the `basename' of the pathname in STRING (the stuff after
 * the last '/').  If STRING is not a full pathname, simply return it.
 */
char *
base_pathname (char *string)
{
  char *p = strrchr (string, '/');

  if (!absolute_pathname (string))
    return (string);

  if (p)
    return (++p);
  else
    return (string);
}

/*
 * Return the octal number parsed from STRING, or -1 to indicate that
 * the string contained a bad number.
 */
int
read_octal (const char *string)
{
  int result = 0;
  int digits = 0;

  while (*string && *string >= '0' && *string < '8')
    {
      digits++;
      result = (result * 8) + *string++ - '0';
    }

  if (!digits || result > 0777 || *string)
    result = -1;

  return result;
}

/*
 * Append SOURCE to TARGET at INDEX.  SIZE is the current amount of
 * space allocated to TARGET.  SOURCE can be NULL, in which case
 * nothing happens.  Gets rid of SOURCE by free ()ing it.  Returns
 * TARGET in case the location has changed.
 */
char *
sub_append_string (char *source, char *target, int *index, int *size)
{
  if (source)
    {
      while ((int)strlen (source) >= (int)(*size - *index))
	{
	  char *tmp = new char [*size += DEFAULT_ARRAY_SIZE];
	  strcpy (tmp, target);
	  delete [] target;
	  target = tmp;
	}

      strcat (target, source);
      *index += strlen (source);

      delete [] source;
    }
  return target;
}

/*
 * Return a string which will be printed as a prompt.  The string may
 * contain special characters which are decoded as follows: 
 *   
 *	\t	the time
 *	\d	the date
 *	\n	CRLF
 *	\s	the name of the shell (program)
 *	\w	the current working directory
 *	\W	the last element of PWD
 *	\u	your username
 *	\h	the hostname
 *	\#	the command number of this command
 *	\!	the history number of this command
 *	\$	a $ or a # if you are root
 *	\<octal> character code in octal
 *	\\	a backslash
 */
char *
decode_prompt_string (const char *string)
{
  int result_size = PROMPT_GROWTH;
  int result_index = 0;
  char *result = new char [PROMPT_GROWTH];
  int c;
  char *temp = (char *)NULL;

  result[0] = 0;
  while (c = *string++)
    {
      if (c == '\\')
	{
	  c = *string;

	  switch (c)
	    {
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	      {
		char octal_string[4];
		int n;

		strncpy (octal_string, string, 3);
		octal_string[3] = '\0';

		n = read_octal (octal_string);

		temp = strsave ("\\");
		if (n != -1)
		  {
		    string += 3;
		    temp[0] = n;
		  }

		c = 0;
		goto add_string;
	      }
	  
	    case 't':
	    case 'd':
	      /* Make the current time/date into a string. */
	      {
		time_t the_time = time (0);
		char *ttemp = ctime (&the_time);
		temp = strsave (ttemp);

		if (c == 't')
		  {
		    strcpy (temp, temp + 11);
		    temp[8] = '\0';
		  }
		else
		  temp[10] = '\0';

		goto add_string;
	      }

	    case 'n':
	      if (!no_line_editing)
		temp = strsave ("\r\n");
	      else
		temp = strsave ("\n");
	      goto add_string;

	    case 's':
	      {
		temp = base_pathname (prog_name);
		temp = strsave (temp);
		goto add_string;
	      }
	
	    case 'w':
	    case 'W':
	      {
		char t_string[MAXPATHLEN];
#define EFFICIENT
#ifdef EFFICIENT

// Use the value of PWD because it is much more effecient.

		temp = user_pref.pwd;

		if (!temp)
		  getcwd (t_string, MAXPATHLEN);
		else
		  strcpy (t_string, temp);
#else
		getcwd (t_string, MAXPATHLEN);
#endif	/* EFFICIENT */

		if (c == 'W')
		  {
		    char *dir = strrchr (t_string, '/');
		    if (dir && dir != t_string)
		      strcpy (t_string, dir + 1);
		    temp = strsave (t_string);
		  }
		else
		  temp = strsave (polite_directory_format (t_string));
		goto add_string;
	      }
      
	    case 'u':
	      {
		temp = strsave (user_name);

		goto add_string;
	      }

	    case 'h':
	      {
		char *t_string;

		temp = strsave (host_name);
		if (t_string = strchr (temp, '.'))
		  *t_string = '\0';
		
		goto add_string;
	      }

	    case '#':
	      {
		char number_buffer[20];
		sprintf (number_buffer, "%d", current_command_number);
		temp = strsave (number_buffer);
		goto add_string;
	      }

	    case '!':
	      {
		char number_buffer[20];
		int num = current_history_number ();
		if (num > 0)
                  sprintf (number_buffer, "%d", num);
		else
		  strcpy (number_buffer, "!");
		temp = strsave (number_buffer);
		goto add_string;
	      }

	    case '$':
	      temp = strsave (geteuid () == 0 ? "#" : "$");
	      goto add_string;

	    case '\\':
	      temp = strsave ("\\");
	      goto add_string;

	    default:
	      temp = strsave ("\\ ");
	      temp[1] = c;

	    add_string:
	      if (c)
		string++;
	      result =
		(char *)sub_append_string (temp, result,
					   &result_index, &result_size);
	      temp = (char *)NULL; /* Free ()'ed in sub_append_string (). */
	      result[result_index] = '\0';
	      break;
	    }
	}
      else
	{
	  while (3 + result_index > result_size)
	    {
	      char *tmp = new char [result_size += PROMPT_GROWTH];
	      strcpy (tmp, result);
	      delete [] result;
	      result = tmp;
	    }
	  result[result_index++] = c;
	  result[result_index] = '\0';
	}
    }

#if 0
  /* I don't really think that this is a good idea.  Do you? */
  if (!find_variable ("NO_PROMPT_VARS"))
    {
      WORD_LIST *expand_string (), *list;
      char *string_list ();

      list = expand_string (result, 1);
      free (result);
      result = string_list (list);
      dispose_words (list);
    }
#endif

  return result;
}

/*
 * Remove the last N directories from PATH.  Do not PATH blank.
 * PATH must contain enough space for MAXPATHLEN characters.
 */
void
pathname_backup (char *path, int n)
{
  register char *p;

  if (!*path)
    return;

  p = path + (strlen (path) - 1);

  while (n--)
    {
      while (*p == '/' && p != path)
	p--;

      while (*p != '/' && p != path)
	p--;

      *++p = '\0';
    }
}

/*
 * Turn STRING (a pathname) into an absolute pathname, assuming that
 * DOT_PATH contains the symbolic location of '.'.  This always
 * returns a new string, even if STRING was an absolute pathname to
 * begin with.
 */
char *
make_absolute (const char *string, const char *dot_path)
{
  static char current_path[MAXPATHLEN];
  register char *cp;

  if (!dot_path || *string == '/')
    return strsave (string);

  strcpy (current_path, dot_path);

  if (!current_path[0])
    strcpy (current_path, "./");

  cp = current_path + (strlen (current_path) - 1);

  if (*cp++ != '/')
    *cp++ = '/';

  *cp = '\0';

  while (*string)
    {
      if (*string == '.')
	{
	  if (!string[1])
	    return strsave (current_path);

	  if (string[1] == '/')
	    {
	      string += 2;
	      continue;
	    }

	  if (string[1] == '.' && (string[2] == '/' || !string[2]))
	    {
	      string += 2;

	      if (*string)
		string++;

	      pathname_backup (current_path, 1);
	      cp = current_path + strlen (current_path);
	      continue;
	    }
	}

      while (*string && *string != '/')
	*cp++ = *string++;

      if (*string)
	*cp++ = *string++;

      *cp = '\0';
    }
  return strsave (current_path);
}

/*
 * Return a consed string which is the current working directory.
 * FOR_WHOM is the name of the caller for error printing.
 */ 
char *
get_working_directory (const char *for_whom)
{
  if (!follow_symbolic_links)
    {
      if (the_current_working_directory)
	delete [] the_current_working_directory;

      the_current_working_directory = (char *)NULL;
    }

  if (!the_current_working_directory)
    {
      char *directory;

      the_current_working_directory = new char [MAXPATHLEN];
      directory = getcwd (the_current_working_directory, MAXPATHLEN);
      if (!directory)
	{
	  message (for_whom, the_current_working_directory);
	  delete [] the_current_working_directory;
	  the_current_working_directory = (char *)NULL;
	  return (char *)NULL;
	}
    }

  return the_current_working_directory;
}

/*
 * Do the work of changing to the directory NEWDIR.  Handle symbolic
 * link following, etc.
 */ 
int
change_to_directory (const char *newdir)
{
  char *t;

  if (follow_symbolic_links)
    {
      if (!the_current_working_directory)
	get_working_directory ("cd_links");

      if (the_current_working_directory)
	t = make_absolute (newdir, the_current_working_directory);
      else
	t = strsave (newdir);

      /* Get rid of trailing `/'. */
      {
	register int len_t = strlen (t);
	if (len_t > 1)
	  {
	    --len_t;
	    if (t[len_t] == '/')
	      t[len_t] = '\0';
	  }
      }

      if (chdir (t) < 0)
	{
	  delete [] t;
	  return 0;
	}

      if (the_current_working_directory)
	strcpy (the_current_working_directory, t);

      delete [] t;
      return 1;
    }
  else
    {
      if (chdir (newdir) < 0)
	return 0;
      else
	return 1;
    }
}

/*
 * Has file `A' been modified after time `T'?
 *
 * case:
 *
 *   a newer than t         returns    1
 *   a older than t         returns    0
 *   stat on a fails        returns   -1
 */
int
is_newer (const char *fa, time_t t)
{
  struct stat fa_sb;
  register int fa_stat;
  register int status = 0;

  fa_stat = stat (fa, &fa_sb);
  if (fa_stat != 0)
    status = -1;

  if (status != 0)
    return status;

  return (fa_sb.st_mtime > t);
}

/*
 * Return to the main command loop in octave.cc.
 */
void
jump_to_top_level (void)
{
  run_all_unwind_protects ();

  longjmp (toplevel, 1);
}

/*
 * Gag.
 */
char *
s_plural (int i)
{
  static char *empty = "";
  static char *s = "s";
  return i == 1 ? empty : s;
}

char *
es_plural (int i)
{
  static char *empty = "";
  static char *es = "es";
  return i == 1 ? es : empty;
}

char *
save_in_tmp_file (tree_constant& t, int ndim = 2, int parametric = 0)
{
  char *name = strsave (tmpnam ((char *) NULL));
  if (name != (char *) NULL)
    {
      ofstream file (name);
      if (file)
	{
	  switch (ndim)
	    {
	    case 2:
	      t.save (file);
	      break;
	    case 3:
	      t.save_three_d (file, parametric);
	      break;
	    default:
	      panic_impossible ();
	      break;
	    }
	}
      else
	{
	  error ("couldn't open temporary output file `%s'", name);
	  delete [] name;
	  name = (char *) NULL;
	}
    }
  return name;
}

void
mark_for_deletion (const char *filename)
{
  char *tmp = strsave (filename);
  tmp_files.push (tmp);
}

void
cleanup_tmp_files (void)
{
  while (! tmp_files.empty ())
    {
      char *filename = tmp_files.pop ();
      unlink (filename);
      delete [] filename;
    }
}

int
send_to_plot_stream (const char *cmd)
{
// From sighandlers.cc:
  extern int pipe_handler_error_count;

  static int initialized = 0;

  if (! plot_stream.is_open ())
    {
      char *plot_prog = user_pref.gnuplot_binary;
      if (plot_prog != (char *) NULL)
	{
	  plot_stream.open (plot_prog);
	  if (! plot_stream.is_open ())
	    {
	      warning ("plot: unable to open pipe to `%s'",
		       plot_prog);

	      if (strcmp (plot_prog, "gnuplot") != 0)
		{
		  message ("plot", "trying again with `gnuplot'");
		  goto last_chance;
		}
	    }
	}
      else
	{
	last_chance:

	  plot_stream.open ("gnuplot");

	  if (! plot_stream.is_open ())
	    {
	      error ("plot: unable to open pipe to `%s'", plot_prog);
	      return -1;
	    }
	}
    }

  if (! initialized)
    {
      initialized = 1;
      plot_stream << "set data style lines\n";
    }

  plot_stream << cmd;
  plot_stream.flush ();
  pipe_handler_error_count = 0;

  return 0;
}

void
close_plot_stream (void)
{
  if (plot_stream.is_open ())
    plot_stream.close ();
}

int
almost_match (const char *std, const char *s, int min_match_len = 1)
{
  int stdlen = strlen (std);
  int slen = strlen (s);

  return (slen <= stdlen
	  && slen >= min_match_len
	  && strncmp (std, s, slen) == 0);
}

char **
get_m_file_names (int& num, const char *dir, int no_suffix)
{
  static int num_max = 256;
  char **retval = new char * [num_max];
  int i = 0;

  DIR *dirp = opendir (dir);
  if (dirp != (DIR *) NULL)
    {
      struct dirent *entry;
      while ((entry = readdir (dirp)) != (struct dirent *) NULL)
	{
	  int len = NLENGTH (entry);
	  if (len > 2
	      && entry->d_name[len-2] == '.'
	      && entry->d_name[len-1] == 'm')
	    {
	      retval[i] = strsave (entry->d_name);
	      if (no_suffix)
		retval[i][len-2] = '\0';

	      i++;

	      if (i == num_max - 1)
		{
		  num_max += 256;
		  char **tmp = new char * [num_max];
		  for (int j = 0; j < i; j++)
		    tmp[j] = retval[j];

		  retval = tmp;
		}
	    }
	}
      closedir (dirp);
    }

  retval[i] = (char *) NULL;
  num = i;

  return retval;
}

char **
get_m_file_names (int& num, int no_suffix)
{
  static int num_max = 1024;
  char **retval = new char * [num_max];
  int i = 0;

  char **path = pathstring_to_vector (user_pref.loadpath);

  char **ptr = path;
  if (ptr != (char **) NULL)
    {
      while (*ptr != (char *) NULL)
	{
	  int tmp_num;
	  char **names = get_m_file_names (tmp_num, *ptr, no_suffix);

	  if (i + tmp_num >= num_max - 1)
	    {
	      num_max += 1024;
	      char **tmp = new char * [num_max];
	      for (int j = 0; j < i; j++)
		tmp[j] = retval[j];

	      retval = tmp;
	    }

	  int k = 0;
	  while (k < tmp_num)
	    retval[i++] = names[k++];

	  ptr++;
	}
    }

  retval[i] = (char *) NULL;
  num = i;

  return retval;
}

int
NINT (double x)
{
  if (x > INT_MAX)
    return INT_MAX;
  else if (x < INT_MIN)
    return INT_MIN;
  else
    return (x > 0) ? ((int) (x + 0.5)) : ((int) (x - 0.5));
}

double
D_NINT (double x)
{
  if (xisinf (x) || xisnan (x))
    return x;
  else
    return floor (x + 0.5);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
