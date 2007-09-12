function plotimages (nm, typ)
  bury_output ();
  if (strcmp (nm, "plot"))
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
  else
    error ("unrecognized plot requested");
  endif
  bury_output ();
endfunction

## Use this function before plotting commands and after every call to
## print since print() resets output to stdout (unfortunately, gnpulot
## can't pop output as it can the terminal type).
function bury_output ()
  f = figure (1);
  set (f, "visible", "off");
endfunction
