
function retval = qconj(q)
  # function retval = qconj(q)
  # conjugate of a quaternion
  #  q = [w,x,y,z] = w*i + x*j + y*k + z
  # qconj(q) = -w*i -x*j -y*k + z

  [a,b,c,d] = quaternion(q);
  retval = quaternion(-a,-b,-c,d);
endfunction

