/* progname.c: the executable name we were invoked as; general initialization.

Copyright (C) 1994 Karl Berry.

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  */

#include <kpathsea/config.h>

#include <kpathsea/progname.h>


#ifndef HAVE_PROGRAM_INVOCATION_NAME
/* Don't redefine the variables if glibc already has.  */
string program_invocation_name;
string program_invocation_short_name;
#endif


void
kpse_set_progname P1C(const_string, progname)
{
#ifndef HAVE_PROGRAM_INVOCATION_NAME
  program_invocation_name = xstrdup (progname);
  program_invocation_short_name = (string) basename (program_invocation_name);
#endif
  {
    string s = getenv ("KPATHSEA_DEBUG");
    if (s) 
     kpathsea_debug |= atoi (s);
  }
}
