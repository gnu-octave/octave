/* expand.c: general expansion.

Copyright (C) 1993, 94 Karl Berry.

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <kpathsea/config.h>

#include <kpathsea/c-pathch.h>
#include <kpathsea/expand.h>
#include <kpathsea/pathsearch.h>
#include <kpathsea/tilde.h>
#include <kpathsea/variable.h>


/* Do variable expansion so ~${USER} will work.  (Besides, it's what the
   shells do.)  */

string
kpse_expand P1C(const_string, s)
{
  string var_expansion = kpse_var_expand (s);
  string tilde_expansion = kpse_tilde_expand (var_expansion);
  
  /* `kpse_var_expand' always gives us new memory; `kpse_tilde_expand'
     doesn't, necessarily.  So be careful that we don't free what we are
     about to return.  */
  if (tilde_expansion != var_expansion)
    free (var_expansion);
  
  return tilde_expansion;
}


/* Be careful to not waste all the memory we allocate for each element.  */

string
kpse_path_expand P1C(const_string, path)
{
  string elt;
  string ret = xmalloc (1); /* so we can free it */
  *ret = 0;
  
  for (elt = kpse_path_element (path); elt; elt = kpse_path_element (NULL))
    {
      string save_ret = ret;
      string elt_exp = kpse_expand (elt);
      ret = concat3 (ret, elt_exp, ENV_SEP_STRING);
      free (elt_exp);
      free (save_ret);
    }
    
  /* Waste the last byte.  */
  ret[strlen (ret) - 1] = 0;
  
  return ret;
}
