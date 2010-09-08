## Copyright (C) 2010 Ben Abbott
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} __ghostscript__ (@var{@dots{}})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-26

function status = __ghostscript__ (varargin);

  opts.binary = "";
  opts.source = "";
  opts.output = "";
  opts.device = "";
  opts.epscrop = false;
  opts.antialiasing  = false;
  opts.resolution = [];
  opts.papersize = "";
  opts.pageoffset = [0 0];
  opts.debug = false;
  opts.level = [];

  offsetfile = "";

  args = varargin;
  n = find (cellfun (@isstruct, args));
  if (! isempty (n))
    f = fieldnames (args{n});
    for m = 1:numel(f)
      opts.(f{m}) = args{n}.(f{m});
    endfor
    args(n) = [];
  endif
  for n = 1:2:numel(args)
    opts.(args{n}) = args{n+1};
  endfor

  gs_opts = sprintf ("-dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=%s",
                     opts.device);

  if (! isempty (opts.level) && ismember (opts.level, [1, 2, 3]))
    gs_opts = sprintf ("%s -dLanguageLevel=%d", gs_opts, round (opts.level));
  endif

  if (isempty (strfind (opts.device, "write")))
    ## Empirical observation: "-dpxlcolor" requires a sign change as
    ##                        compared to pdfwrite, or pswrite output.
    opts.pageoffset = opts.pageoffset .* [1, -1];
    if (! isempty (opts.resolution))
      gs_opts = sprintf ("%s -r%dx%d", gs_opts, [1, 1] * opts.resolution);
    endif
    if (opts.antialiasing)
      gs_opts = sprintf ("%s -dTextAlphaBits=4 -dGraphicsAlphaBits=4", gs_opts);
    endif
  elseif (any (strcmp (opts.device, {"pswrite", "ps2write", "pdfwrite"})))
    gs_opts = sprintf ("%s -dEmbedAllFonts=true", gs_opts);
    if (strcmp (opts.device, "pdfwrite"))
      ## Optimize for loading
      gs_opts = sprintf ("%s -dOptimize=true", gs_opts);
    endif
  endif

  if (opts.epscrop)
    ## papersize is specified by the eps bbox
    gs_opts = sprintf ("%s -dEPSCrop", gs_opts);
  elseif (! isempty (opts.papersize))
    if (ischar (opts.papersize))
      gs_opts = sprintf ("%s -sPAPERSIZE=%s", gs_opts, opts.papersize);
    elseif (isnumeric (opts.papersize) && numel (opts.papersize) == 2)
      gs_opts = sprintf ("%s -dDEVICEWIDTHPOINTS=%d -dDEVICEHEIGHTPOINTS=%d",
                         gs_opts, opts.papersize);
      if (opts.papersize(1) > opts.papersize(2))
        ## Lanscape mode: This option will result in automatic rotation of the
        ##                document page if the requested page size matches one
        ##                of the default page sizes.
        gs_opts = sprintf ("%s -dNORANGEPAGESIZE", gs_opts);
      endif
    else
      error ("print:badpapersize", "__ghostscript__.m: invalid 'papersize'")
    endif
    gs_opts = sprintf ("%s -dFIXEDMEDIA", gs_opts);
    offsetfile = strcat (tmpnam (), ".ps");
    unwind_protect
      fid = fopen (offsetfile, "w");
      onCleanup (@() unlink (offsetfile));
      if (fid == -1)
        error ("print:fopenfailed", "__ghostscript__.m: fopen() failed.");
      endif
      fprintf (fid, "%s\n", "%!PS-Adobe-3.0")
      ## "pageoffset" is relative to the coordinates, not the BBox LLHC.
      fprintf (fid, "%s [%d %d] %s\n", "<< /Margins [0 0] /.HWMargins [0 0 0 0] /PageOffset",
               opts.pageoffset, ">> setpagedevice");
      fprintf (fid, "%%EOF");
    unwind_protect_cleanup
      status = fclose (fid);
      if (status == -1)
        error ("print:fclosefailed", "__ghostscript__.m: fclose() failed.");
      endif
    end_unwind_protect
    if (opts.debug)
      [~,output] = system (sprintf ("cat %s", offsetfile));
      fprintf ("---- begin %s ----\n", offsetfile)
      disp (output)
      fprintf ("----- end %s -----\n", offsetfile)
    endif
  endif

  cmd = sprintf ("%s %s -sOutputFile=%s %s %s 2>&1", 
                 opts.binary, gs_opts,
                 opts.output, offsetfile, opts.source);

  if (opts.debug)
    fprintf ("Ghostscript command: %s\n", cmd);
  endif

  [status, output] = system (cmd);

  if (status != 0)
    warning ("print:ghostscripterror", 
             "print.m: %s, '%s'.", output, opts.output)
  endif

endfunction


