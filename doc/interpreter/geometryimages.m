function geometryimages (nm, typ)
  bury_output ();
  if (strcmp (nm, "voronoi"))
    rand("state",9);
    x = rand(10,1);
    y = rand(10,1);
    tri = delaunay (x, y);
    [vx, vy] = voronoi (x, y, tri);
    triplot (tri, x, y, "b");
    hold on;
    plot (vx, vy, "r");
    [r, c] = tri2circ (tri(end,:), x, y);
    pc = [-1:0.01:1];
    xc = r * sin(pi*pc) + c(1);
    yc = r * cos(pi*pc) + c(2);
    plot (xc, yc, "g-", "LineWidth", 3);
    axis([0, 1, 0, 1]);
    legend ("Delaunay Triangulation", "Voronoi Diagram");
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "triplot"))
    rand ("state", 2)
    x = rand (20, 1);
    y = rand (20, 1);
    tri = delaunay (x, y);
    triplot (tri, x, y);
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "griddata"))
    rand("state",1);
    x=2*rand(1000,1)-1;
    y=2*rand(size(x))-1;
    z=sin(2*(x.^2+y.^2));
    [xx,yy]=meshgrid(linspace(-1,1,32));
    griddata(x,y,z,xx,yy);
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "convhull"))
    x = -3:0.05:3;
    y = abs (sin (x));
    k = convhull (x, y);
    plot (x(k),y(k),'r-',x,y,'b+');
    axis ([-3.05, 3.05, -0.05, 1.05]);
    print (strcat (nm, ".", typ), strcat ("-d", typ)) 
  elseif (strcmp (nm, "delaunay"))
    rand ("state", 1);
    x = rand (10, 1);
    y = rand (10, 1);
    T = delaunay (x, y);
    X = [ x(T(:,1)); x(T(:,2)); x(T(:,3)); x(T(:,1)) ];
    Y = [ y(T(:,1)); y(T(:,2)); y(T(:,3)); y(T(:,1)) ];
    axis ([0, 1, 0, 1]);
    plot(X, Y, "b", x, y, "r*");
    print (strcat (nm, ".", typ), strcat ("-d", typ)) 
  else
    error ("unrecognized plot requested");
  endif
  bury_output ();
endfunction

function [r, c] = tri2circ (tri, xx, yy)
  x = xx(tri);
  y = yy(tri);
  m = (y(1:end-1) - y(2:end)) ./ (x(1:end-1) - x(2:end));
  xc = (prod(m) .* (y(1) - y(end)) + m(end)*(x(1)+x(2)) - m(1)*(x(2)+x(3))) ...
        ./ (2 * (m(end) - m(1))); 
  yc = - (xc - (x(2) + x(3))./2) ./ m(end) + (y(2) + y(3)) / 2;
  c = [xc, yc];
  r = sqrt ((xc - x(1)).^2 + (yc - y(1)).^2);
endfunction

## Use this function before plotting commands and after every call to
## print since print() resets output to stdout (unfortunately, gnpulot
## can't pop output as it can the terminal type).
function bury_output ()
  f = figure (1);
  set (f, "visible", "off");
endfunction
function geometryimages (nm, typ)
  bury_output ();
  if (strcmp (nm, "voronoi"))
    rand("state",9);
    x = rand(10,1);
    y = rand(10,1);
    tri = delaunay (x, y);
    [vx, vy] = voronoi (x, y, tri);
    triplot (tri, x, y, "b");
    hold on;
    plot (vx, vy, "r");
    [r, c] = tri2circ (tri(end,:), x, y);
    pc = [-1:0.01:1];
    xc = r * sin(pi*pc) + c(1);
    yc = r * cos(pi*pc) + c(2);
    plot (xc, yc, "g-", "LineWidth", 3);
    axis([0, 1, 0, 1]);
    legend ("Delaunay Triangulation", "Voronoi Diagram");
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "triplot"))
    rand ("state", 2)
    x = rand (20, 1);
    y = rand (20, 1);
    tri = delaunay (x, y);
    triplot (tri, x, y);
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  elseif (strcmp (nm, "griddata"))
    rand("state",1);
    x=2*rand(1000,1)-1;
    y=2*rand(size(x))-1;
    z=sin(2*(x.^2+y.^2));
    [xx,yy]=meshgrid(linspace(-1,1,32));
    griddata(x,y,z,xx,yy);
    print (strcat (nm, ".", typ), strcat ("-d", typ))    
  else
    error ("unrecognized plot requested");
  endif
  bury_output ();
endfunction

function [r, c] = tri2circ (tri, xx, yy)
  x = xx(tri);
  y = yy(tri);
  m = (y(1:end-1) - y(2:end)) ./ (x(1:end-1) - x(2:end));
  xc = (prod(m) .* (y(1) - y(end)) + m(end)*(x(1)+x(2)) - m(1)*(x(2)+x(3))) ...
        ./ (2 * (m(end) - m(1))); 
  yc = - (xc - (x(2) + x(3))./2) ./ m(end) + (y(2) + y(3)) / 2;
  c = [xc, yc];
  r = sqrt ((xc - x(1)).^2 + (yc - y(1)).^2);
endfunction

## Use this function before plotting commands and after every call to
## print since print() resets output to stdout (unfortunately, gnpulot
## can't pop output as it can the terminal type).
function bury_output ()
  f = figure (1);
  set (f, "visible", "off");
endfunction
