########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{dimg} =} im2double (@var{img})
## @deftypefnx {} {@var{dimg} =} im2double (@var{img}, "indexed")
## Convert image to double precision.
##
## The conversion of @var{img} to double precision, is dependent on the type of
## input image.  The following input classes are supported:
##
## @table @samp
## @item uint8, uint16, and int16
## The range of values from the class is scaled to the interval [0 1].
##
## @item logical
## True and false values are assigned a value of 1 and 0 respectively.
##
## @item single
## Values are cast to double.
##
## @item double
## Returns the same image.
##
## @end table
##
## If @var{img} is an indexed image, then the second argument should be the
## string @qcode{"indexed"}.  If so, then @var{img} must either be of floating
## point class, or unsigned integer class and it will simply be cast to double.
## If it is an integer class, an offset of +1 is applied.
##
## @seealso{double}
## @end deftypefn

function dimg = im2double (img, im_type)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    ## "normal" (non-indexed) images
    switch (class (img))
      case "uint8",   dimg = double (img) / 255;
      case "uint16",  dimg = double (img) / 65535;
      case "int16",   dimg = (double (img) + 32768) / 65535;
      case "single",  dimg = double (img);
      case "logical", dimg = double (img);
      case "double",  dimg = img;
      otherwise, error ('im2double: IMG is of unsupported class "%s"', class (img));
    endswitch
  else
    ## indexed images
    if (! strcmpi (im_type, "indexed"))
      error ('im2double: second input argument must be the string "indexed"');
    elseif (any (isa (img, {"uint8", "uint16"})))
      dimg = double (img) + 1;
    elseif (isfloat (img) || isbool (img))
      dimg = double (img);
    else
      ## Technically, it could also be of logical class and we do not
      ## enforce positive integers for floating for Matlab compatibility.
      ## Still, no need to tell that to the user.
      error (["im2double: if IMG is indexed, then it must be positive " ...
              "integer floating points, or unsigned integer class"]);
    endif
  endif

endfunction


%!assert (im2double ([1 2 3]), [1 2 3])
%!assert (im2double (single ([1 2 3])), [1 2 3])
%!assert (im2double (uint8 ([0 127 128 255])), [0 127/255 128/255 1])
%!assert (im2double (uint16 ([0 127 128 65535])), [0 127/65535 128/65535 1])
%!assert (im2double (int16 ([-32768 -32767 -32766 32767])),
%!                   [0 1/65535 2/65535 1])

%!assert (im2double (uint8 ([0 1 255]), "indexed"), [1 2 256])
%!assert (im2double (uint16 ([0 1 2557]), "indexed"), [1 2 2558])
%!assert (im2double ([3 25], "indexed"), [3 25])
%!assert (im2double (single ([3 25]), "indexed"), [3 25])

## Test for ND input
%!function test_im2double_nd (cls, low, high)
%!  in = rand (2, 4, 2, 3, 2);
%!  in *= high - low;
%!  in += low;
%!  in = cast (in, cls);
%!  out = zeros (size (in));
%!  for n = 1:12
%!    out(:,:,n) = im2double (in(:,:,n));
%!  endfor
%!  assert (im2double (in), out);
%!endfunction

%!test
%! test_im2double_nd ("double", 0, 1);
%! test_im2double_nd ("single", 0, 1);
%! test_im2double_nd ("uint8", 0, 255);
%! test_im2double_nd ("uint16", 0, 6535);
%! test_im2double_nd ("int16", -32768, 32767);

## Test lack of input check for Matlab compatibility
%!assert (im2double ([0 1 2], "indexed"), [0 1 2])
%!assert (im2double ([0 -1 -2], "indexed"), [0 -1 -2])
%!assert (im2double ([0 -1.5 -2], "indexed"), [0 -1.5 -2])
%!assert (im2double ([0 -1.5 -2i], "indexed"), [0 -1.5 -2i])
%!assert (im2double ([false true], "indexed"), [0 1])

%!error <unsigned integer class> im2double (int16 ([17 8]), "indexed")
%!error <unsigned integer class> im2double (int16 ([-7 8]), "indexed")
%!error <must be the string "indexed"> im2double ([1 2 3], "non-indexed")
