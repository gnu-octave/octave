########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
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
## @deftypefn  {} {} printd (@var{obj}, @var{filename})
## @deftypefnx {} {@var{out_file} =} printd (@dots{})
##
## Convert any object acceptable to @code{disp} into the format selected by
## the suffix of @var{filename}.
##
## If the optional output @var{out_file} is requested, the name of the created
## file is returned.
##
## This function is intended to facilitate manipulation of the output of
## functions such as @code{stemleaf}.
## @seealso{stemleaf}
## @end deftypefn

function pr_out = printd (obj, filename)

  ## Convert any object acceptable to disp() into various display formats.
  ## obj is the input object.
  ## filename is the output file (with required suffix).

  ## Extract .suffix from filename
  if ((sufix = rindex (filename, ".")) <= 0)
    error ("printd: output FILENAME '%s' requires a suffix.\nOptions are: pdf ps eps txt jpg jpeg", filename);
  endif
  opt = substr (filename, sufix+1);
  [pf, tempf, mag] = mkstemp (fullfile (tempdir (), "oct-XXXXXX"), true);
  fprintf (pf, "%s", disp (obj));
  frewind (pf);

  ## It seems best to only use convert for image output.  Its ps and pdf
  ## are badly rendered.
  opt = lower (opt);
  switch (opt)
    case "pdf"
      enscr = sprintf ("enscript --no-header -o %s.ps %s ; ps2pdf %s.ps %s.pdf; mv %s.pdf %s;exit", ...
                       tempf, tempf, tempf, tempf, tempf, filename);
      system (enscr);
      delete ([tempf ".ps"]);
    case "ps"
      enscr = sprintf ("enscript --no-header -o %s %s ; exit", filename, tempf);
      system (enscr);
    case "eps"
      enscr = sprintf ("enscript --no-header -o %s.ps %s ; ps2eps --ignoreBB %s.ps; mv %s.eps %s; exit", ...
                       tempf, tempf, tempf, tempf, filename);
      system (enscr);
      delete ([tempf ".ps"]);
    case "txt"
      enscr = sprintf ("cp %s %s", tempf, filename);
      system (enscr);
    case {"jpg", "jpeg"}
      enscr = sprintf ("convert -trim txt:%s  jpg:%s", tempf, filename);
      system (enscr);
    otherwise
      fclose (pf);
      delete (tempf);
      error ("printd: unknown conversion type: %s.\nOptions are: pdf ps eps txt jpg jpeg", opt);

  endswitch
  fclose (pf);
  delete (tempf);
  pr_out = sprintf ("%s file %s written\n", opt, filename);

endfunction


%!demo
%! r2 = char ( ...
%! "stem step: 10, data: unsorted.", ...
%! "Hinges:    lo: 12, hi: 42"     , ...
%! "   1 | 22118"                  , ...
%! "   2 | 28"                     , ...
%! "   3 | 98"                     , ...
%! "   4 | 244"                    , ...
%! "   5 | 2"                      );
%! printd (r2, "test_p.txt");
%! type ("test_p.txt");
%! unlink ("test_p.txt");

%!test
%! r2 = char (
%! "stem step: 10, data: unsorted.",
%! "Hinges:    lo: 12, hi: 42"     ,
%! "   1 | 22118"                  ,
%! "   2 | 28"                     ,
%! "   3 | 98"                     ,
%! "   4 | 244"                    ,
%! "   5 | 2"                      );
%! unwind_protect
%!   filename = [tempname() ".txt"];
%!   printd (r2, filename);
%!   r4 = fileread (filename);
%!   r2 = disp (r2);
%!   assert (r4, r2);
%! unwind_protect_cleanup
%!   unlink (filename);
%! end_unwind_protect
