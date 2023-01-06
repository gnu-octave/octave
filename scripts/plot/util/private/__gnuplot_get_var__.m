########################################################################
##
## Copyright (C) 2009-2023 The Octave Project Developers
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
## @deftypefn {} {@var{value} =} __gnuplot_get_var__ (@var{h}, @var{gp_var_name}, @var{fmt})
## Undocumented internal function.
## @end deftypefn

function gp_var_value = __gnuplot_get_var__ (h, gp_var_name, fmt = "")

  if (numel (h) == 1 && isfigure (h))
    if (isempty (get (h, "__plot_stream__")))
      ostream = __gnuplot_open_stream__ (2, h);
    else
      ostream = get (h, "__plot_stream__");
    endif
  else
    ostream = h;
  endif
  if (numel (ostream) < 1)
    error ("__gnuplot_get_var__: stream to gnuplot not open");
  elseif (ispc ())
    if (numel (ostream) == 1)
      error ("__gnuplot_get_var__: Need mkfifo that is not implemented under Windows");
    endif
    use_mkfifo = false;
    istream = ostream(2);
    ostream = ostream(1);
  else
    use_mkfifo = true;
    ostream = ostream(1);
  endif

  if (use_mkfifo)
    gpin_name = tempname ();

    [err, msg] = mkfifo (gpin_name, 600);

    if (err)
      error ("__gnuplot_get_var__: Can not make FIFO (%s)", msg);
    endif
  endif

  gp_var_name = strtrim (gp_var_name);
  n = min (strfind (gp_var_name, " "), strfind (gp_var_name, ",")) - 1;
  if (isempty (n))
    n = numel (gp_var_name);
  endif

  unwind_protect

    ## Notes: Variables may be undefined if user closes gnuplot by "q"
    ## or Alt-F4.  Further, this abrupt close also requires the leading
    ## "\n" on the next line.
    if (use_mkfifo)
      fprintf (ostream, ["\n" 'set print "%s";' "\n"], gpin_name);
      fflush (ostream);
      [gpin, err] = fopen (gpin_name, "r");
      if (err)
        ## Try a second time, and then give an error.
        [gpin, err] = fopen (gpin_name, "r");
      endif
      if (err)
        error ("__gnuplot_get_var__: can not open FIFO");
      endif
      gp_cmd = sprintf (["\n" 'if (exists("%s")) print %s; else print NaN' "\n"],
                        gp_var_name(1:n), gp_var_name);
      fputs (ostream, gp_cmd);

      ## Close output file, to force it to be flushed
      fputs (ostream, "set print;\n");
      fflush (ostream);

      ## Now read from fifo.
      reading = true;
      str = {};
      while (reading)
        str{end+1} = fgets (gpin);
        if (isnumeric (str{end}) && (str{end} == -1))
          reading = false;
          str = str(1:(end-1));
        endif
      endwhile
      str = strcat (str{:});
      fclose (gpin);
    else
      ## Direct gnuplot to print to <STDOUT>
      fprintf (ostream, ['set print "-";' "\n"]);
      fflush (ostream);
      gp_cmd = sprintf (["\n" 'if (exists("%s")) print "OCTAVE: ", %s, ' ...
                        '" :END_OCTAVE"; else print NaN' "\n"],
                        gp_var_name(1:n), gp_var_name);
      fputs (ostream, gp_cmd);
      fflush (ostream);
      ## Direct gnuplot to print to <STDERR>
      fputs (ostream, "set print;\n");
      fflush (ostream);

      str = "";
      t_start = tic ();
      while (toc (t_start) < 10)
        str = [str, fread(istream, "*char")'];
        if (! isempty (str))
          re_str = regexp (str, "OCTAVE: (.*) :END_OCTAVE", "tokens");
          if (! isempty (re_str))
            str = re_str{end}{1};
            break;
          endif
        endif
        fclear (istream);
      endwhile
    endif

    ## Strip out EOLs and the continuation character "|"
    str(str=="\n" | str=="\r") = "";
    n_continue = strfind (str, " \\ ");
    if (! isempty (n_continue))
      str(n_continue+1) = "";
    endif

    if (isempty (fmt))
      gp_var_value = strtrim (str);
    else
      gp_var_value = sscanf (str, fmt);
    endif

  unwind_protect_cleanup
    if (use_mkfifo)
      unlink (gpin_name);
    endif
  end_unwind_protect

endfunction
