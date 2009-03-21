/*

Copyright (C) 2007, 2008, 2009 David Bateman

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>
#include <string>

#include "oct.h"

template <class LT, class RT>
/* static */ void
typecast (const Array<RT>& x, Array<LT>& y)
{
  octave_idx_type n = x.length ();
  size_t ns = sizeof (RT);
  size_t ms = sizeof (LT);

  if (n * ns % ms != 0)
    error ("typecast: incorrect number of input values to make output value");
  else
    {
      octave_idx_type m = n * ns / ms;
      dim_vector dv = x.dims ();
      for (octave_idx_type i = 0; i < dv.length(); i++)
	if (dv(i) == n)
	  {
	    dv(i) = m;
	    break;
	  }
      y.resize (dv);
      const unsigned char *xp
	= reinterpret_cast<const unsigned char *> (x.fortran_vec ());
      unsigned char *yp = reinterpret_cast<unsigned char *>(y.fortran_vec ());
      for (octave_idx_type i = 0; 
	   i < n * static_cast<octave_idx_type>(ns); i++)
	*yp++ = *xp++;
    }
}

template <class T>
static octave_value
typecast (const T& x, std::string type)
{
  octave_value retval;
  if (type == "uint8")
    {
      uint8NDArray y;
      typecast (x, y);
      retval = octave_value (y);
    }
  else if (type == "uint16")
    {
      uint16NDArray y;
      typecast (x, y);
      retval = octave_value (y);
    }
  else if (type == "uint32")
    {
      uint32NDArray y;
      typecast (x, y);
      retval = octave_value (y);
    }
  else if (type == "uint64")
    {
      uint64NDArray y;
      typecast (x, y);
      retval = octave_value (y);
    }
  else if (type == "int8")
    {
      int8NDArray y;
      typecast (x, y);
      retval = octave_value (y);
    }
  else if (type == "int16")
    {
      int16NDArray y;
      typecast (x, y);
      retval = octave_value (y);
    }
  else if (type == "int32")
    {
      int32NDArray y;
      typecast (x, y);
      retval = octave_value (y);
    }
  else if (type == "int64")
    {
      int64NDArray y;
      typecast (x, y);
      retval = octave_value (y);
    }
  else if (type == "single")
    {
      FloatNDArray y;
      typecast (x, y);
      retval = octave_value (y);
    }
  else
    {
      NDArray y;
      typecast (x, y);
      retval = octave_value (y);
    }

  return retval;
}

DEFUN_DLD (typecast, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} typecast (@var{x}, @var{type})\n\
Convert from one datatype to another without changing the underlying\n\
data.  The argument @var{type} defines the type of the return argument\n\
and must be one of 'uint8', 'uint16', 'uint32', 'uint64', 'int8', 'int16',\n\
'int32', 'int64', 'single' or 'double'.\n\
\n\
An example of the use of typecast on a little-endian machine is\n\
\n\
@example\n\
@group\n\
@var{x} = uint16 ([1, 65535]);\n\
typecast (@var{x}, 'uint8')\n\
@result{} [   0,   1, 255, 255]\n\
@end group\n\
@end example\n\
@seealso{cast, swapbytes}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin != 2)
    print_usage ();
  else
    {
      std::string type = args (1).string_value ();

      if (! error_state)
	{
	  std::transform (type.begin (), type.end (), type.begin (), tolower);

	  if (type != "uint8" && type != "uint16"
		   && type != "uint32" && type != "uint64"
		   && type != "int8" && type != "int16"
		   && type != "int32" && type != "int64"
		   && type != "single" && type != "double")
	    error ("typecast: unrecognized or invalid type");
	  else if (args(0).is_sparse_type () || args(0).is_complex_type ())
	    error ("typecast: sparse and complex types are invalid");
	  else
	    {
	      dim_vector dv = args(0).dims ();
	      bool seen = false;

	      for (octave_idx_type i = 0; i < dv.length(); i++)
		if (dv (i) != 1)
		  {
		    if (seen)
		      {
			error ("typecast: argument must be a vector");
			break;
		      }
		    else
		      seen = true;
		  }

	      if (!error_state)
		{
		  if (args(0).is_uint8_type ())
		    retval = typecast (args(0).uint8_array_value (), type); 
		  else if (args(0).is_uint16_type ())
		    retval = typecast (args(0).uint16_array_value (), type); 
		  else if (args(0).is_uint32_type ())
		    retval = typecast (args(0).uint32_array_value (), type); 
		  else if (args(0).is_uint64_type ())
		    retval = typecast (args(0).uint64_array_value (), type); 
		  else if (args(0).is_int8_type ())
		    retval = typecast (args(0).int8_array_value (), type); 
		  else if (args(0).is_int16_type ())
		    retval = typecast (args(0).int16_array_value (), type); 
		  else if (args(0).is_int32_type ())
		    retval = typecast (args(0).int32_array_value (), type); 
		  else if (args(0).is_int64_type ())
		    retval = typecast (args(0).int64_array_value (), type); 
		  else if (args(0).is_single_type ())
		    retval = typecast (args(0).float_array_value (), type);
		  else
		    retval = typecast (args(0).array_value (), type);
		}
	    }
	}
    }

  return retval;
}
