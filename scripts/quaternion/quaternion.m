function [a,b,c,d] = quaternion(w,x,y,z)
# quaternion: construct or extract a quaternion
#  w = a*i + b*j + c*k + d from given data.
#
# calling formats:
# [a,b,c,d]   = quaternion(w)		-or-
# [vv,theta] = quaternion(w)
# w           = quaternion(a,b,c,d)
# w           = quaternion(vv,theta)

switch(nargin)
case(1),					# extract information
  if(!(is_vector(w) & length(w) == 4) )
    error("input vector must be of length 4)")
  endif
  # extract data
  switch(nargout)
  case(4),
    a = w(1);
    b = w(2);
    c = w(3);
    d = w(4);
  case(2),
    if(abs(norm(w)-1) > 1e-12)
      warning(sprintf("quaternion: ||w||=%e, setting=1 for vv, theta",norm(w)))
      w = w/norm(w);
    endif
    [a,b,c,d] = quaternion(w);
    theta = acos(d)*2;
    if(abs(theta) > pi)
      theta = theta - sign(theta)*pi;
    endif
    sin_th_2 = norm([a b c]);

    if(sin_th_2 != 0)
      vv = [a,b,c]/sin_th_2;
    else
      vv = [a b c];
    endif
    a = vv;
    b = theta;
  otherwise,
    usage("[a,b,c,d] = quaternion(w) or [vv,theta] = quaternion(w)");
  endswitch

case(2),
  if(nargout != 1)
    usage("w = quaterion(vv,theta)");
  endif
  vv = w;
  theta = x;

  if(!is_vector(vv) | length(vv) != 3)
    error("vv must be a length three vector")
  elseif(!is_scalar(theta))
    error("theta must be a scalar");
  elseif(norm(vv) == 0)
    error("quaternion: vv is zero.") 
  elseif(abs(norm(vv)-1) > 1e-12)
    warning("quaternion: ||vv|| != 1, normalizing")
    vv = vv/norm(vv);
  endif

  if(abs(theta) > 2*pi)
    warning("quaternion: |theta| > 2 pi, normalizing")
    theta = rem(theta,2*pi);
  endif
  vv = vv*sin(theta/2);
  d = cos(theta/2);
  a = quaternion(vv(1), vv(2), vv(3), d); 

case(4),
  if(nargout != 1)
    usage("w = quaterion(a,b,c,d)");
  endif
  if ( !(is_scalar(w) & is_scalar(x) & is_scalar(y) & is_scalar(z)) )
    error("input values must be scalars.")
  endif
  a = [w x y z];

otherwise,
  error("usage: [a,b,c,d] = quaternion(w), a = quaternion(w,x,y,z)")

endswitch

endfunction
