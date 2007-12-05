## Copyright (C) 2007 John W. Eaton and David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

function plotimages (nm, typ)
  bury_output ();
  if (strcmp(typ , "txt"))
    image_as_txt(nm);
  elseif (strcmp (nm, "plot"))
    x = -10:0.1:10;
    plot (x, sin (x));
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "hist"))
    hist (randn (10000, 1), 30);
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "errorbar"))
    x = 0:0.1:10;
    y = sin (x);
    yp =  0.1 .* randn (size (x));
    ym = -0.1 .* randn (size (x));
    errorbar (x, sin (x), ym, yp);
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "polar"))
    polar (0:0.1:10*pi, 0:0.1:10*pi);
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "mesh"))
    tx = ty = linspace (-8, 8, 41)';
    [xx, yy] = meshgrid (tx, ty);
    r = sqrt (xx .^ 2 + yy .^ 2) + eps;
    tz = sin (r) ./ r;
    mesh (tx, ty, tz);
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "plot3"))
    t = 0:0.1:10*pi;
    r = linspace (0, 1, numel (t));
    z = linspace (0, 1, numel (t));
    plot3 (r.*sin(t), r.*cos(t), z);
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "extended"))
    x = 0:0.01:3;
    plot(x,erf(x));
    hold on;
    plot(x,x,"r");
    axis([0, 3, 0, 1]);
    text(0.65, 0.6175, strcat('\leftarrow x = {2/\surd\pi {\fontsize{16}',
      '\int_{\fontsize{8}0}^{\fontsize{8}x}} e^{-t^2} dt} = 0.6175'))
    print (strcat (nm, ".", typ), strcat ("-d", typ))
  else
    error ("unrecognized plot requested");
  endif
  bury_output ();
endfunction

function bury_output ()
  f = figure (1);
  set (f, "visible", "off");
endfunction

## generate something for the texinfo @image command to process
function image_as_txt(nm)
  fid = fopen (sprintf ("%s.txt", nm), "wt");
  fputs (fid, "\n");
  fputs (fid, "+---------------------------------+\n");
  fputs (fid, "| Image unavailable in text mode. |\n");
  fputs (fid, "+---------------------------------+\n");
  fclose (fid);
endfunction
