function retval = qinv(q)
# function b = qinv(q)
# return the inverse of a quaternion 
#       q =  [w,x,y,z] = w*i + x*j + y*k + z
#  qmult(q,qinv(q)) = 1 = [0 0 0 1]

if(norm(q) != 0)
  retval = qconj(q) /sum(q .* q);
else
  error("qinv: zero quaternion passed!");
end
endfunction
