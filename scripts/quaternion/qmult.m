function retval = qmult(a,b)
  # function retval = qmult(a,b)
  # multiply two quaternions 
  #  [w,x,y,z] = w*i + x*j + y*k + z
  #  identities:
  #    i^2 = j^2 = k^2 = -1
  #    ij = k                    jk = i
  #    ki = j                    kj = -i
  #    ji = -k                   ik = -j
  
  [a1,b1,c1,d1] = quaternion(a);
  [a2,b2,c2,d2] = quaternion(b);
  
  ri = b1*c2 - c1*b2 + d1*a2 + a1*d2;
  rj = c1*a2 - a1*c2 + d1*b2 + b1*d2;
  rk = a1*b2 - b1*a2 + d1*c2 + c1*d2;
  rr = -(a1*a2 + b1*b2 + c1*c2) + d1*d2;
  
  retval = quaternion(ri,rj,rk,rr);
endfunction

