/* error.c -- Handle info errors. */

/* This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1993 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Written by Brian Fox (bfox@ai.mit.edu). */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "info.h"
#include "dribble.h"
#include "terminal.h"
#include "getopt.h"

/* The version numbers of this version of Info. */
int info_major_version = 2;
int info_minor_version = 10;
int info_patch_level = 1;

/* When non-zero, the Info window system has been initialized. */
int info_windows_initialized_p = 0;


/* **************************************************************** */
/*								    */
/*		  Main Entry Point to the Info Program		    */
/*								    */
/* **************************************************************** */

/* Return a string describing the current version of Info. */
char *
version_string ()
{
  static char *vstring = (char *)NULL;

  if (!vstring)
    {
      vstring = (char *)xmalloc (50);
      sprintf (vstring, "%d.%d", info_major_version, info_minor_version);
      if (info_patch_level)
	sprintf (vstring + strlen (vstring), "-p%d", info_patch_level);
    }
  return (vstring);
}

/* **************************************************************** */
/*								    */
/*		   Error Handling for Info			    */
/*								    */
/* **************************************************************** */

static char *program_name = "info";

/* Non-zero if an error has been signalled. */
int info_error_was_printed = 0;

/* Non-zero means ring terminal bell on errors. */
int info_error_rings_bell_p = 1;

/* Print FORMAT with ARG1 and ARG2.  If the window system was initialized,
   then the message is printed in the echo area.  Otherwise, a message is
   output to stderr. */
void
info_error (format, arg1, arg2)
     char *format;
     void *arg1, *arg2;
{
  info_error_was_printed = 1;

  if (!info_windows_initialized_p || display_inhibited)
    {
      fprintf (stderr, "%s: ", program_name);
      fprintf (stderr, format, arg1, arg2);
      fprintf (stderr, "\n");
      fflush (stderr);
    }
  else
    {
      if (!echo_area_is_active)
	{
	  if (info_error_rings_bell_p)
	    terminal_ring_bell ();
	  window_message_in_echo_area (format, arg1, arg2);
	}
      else
	{
	  NODE *temp;

	  temp = build_message_node (format, arg1, arg2);
	  if (info_error_rings_bell_p)
	    terminal_ring_bell ();
	  inform_in_echo_area (temp->contents);
	  free (temp->contents);
	  free (temp);
	}
    }
}


