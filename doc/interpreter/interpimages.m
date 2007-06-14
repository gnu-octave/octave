function interpimages (nm, typ)
  bury_output ();
  if (strcmp (nm, "interpft"))
    t = 0 : 0.3 : pi; dt = t(2)-t(1);
    n = length (t); k = 100;
    ti = t(1) + [0 : k-1]*dt*n/k;
    y = sin (4*t + 0.3) .* cos (3*t - 0.1);
    yp = sin (4*ti + 0.3) .* cos (3*ti - 0.1);
    plot (ti, yp, 'g', ti, interp1(t, y, ti, 'spline'), 'b', ...
	  ti, interpft (y, k), 'c', t, y, 'r+');
    legend ('sin(4t+0.3)cos(3t-0.1','spline','interpft','data');
    print (strcat (nm, ".", typ), strcat ("-d", typ))
  elseif (strcmp (nm, "interpn"))
    x = y = z = -1:1;
    f = @(x,y,z) x.^2 - y - z.^2;
    [xx, yy, zz] = meshgrid (x, y, z);
    v = f (xx,yy,zz);
    xi = yi = zi = -1:0.1:1;
    [xxi, yyi, zzi] = ndgrid (xi, yi, zi);
    vi = interpn(x, y, z, v, xxi, yyi, zzi, 'spline');
    mesh (zi, yi, squeeze (vi(1,:,:)));
    print (strcat (nm, ".", typ), strcat ("-d", typ))
  elseif (strcmp (nm, "interpderiv"))
    t = 0 : 0.3 : pi; dt = t(2)-t(1);
    n = length (t); k = 100; dti = dt*n/k;
    ti = t(1) + [0 : k-1]*dti;
    y = sin (4*t + 0.3) .* cos (3*t - 0.1);
    ddyc = diff(diff(interp1(t,y,ti,'cubic'))./dti)./dti;
    ddys = diff(diff(interp1(t,y,ti,'spline'))./dti)./dti;
    ddyp = diff(diff(interp1(t,y,ti,'pchip'))./dti)./dti;
    plot (ti(2:end-1), ddyc,'g+',ti(2:end-1),ddys,'b*', ...
          ti(2:end-1),ddyp,'c^');
    legend('cubic','spline','pchip');
    print (strcat (nm, ".", typ), strcat ("-d", typ))
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
