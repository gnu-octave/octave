function vi = qtransv(vb,qib)
# function vi = qtransv(vb,q)
# transform the 3-D vector v by the unit quaternion q;
# v = [w x y z], q = transformation quaternion
# returns vi = column vector
#    vi = (2*real(q)^2 - 1)*vb + 2*imag(q)*(imag(q)'*vb) 
#      + 2*real(q)*cross(imag(q),vb)
#    where imag(q) is a column vector of length 3.

if(!is_vector(vb) | length(vb) != 3)
  error(sprintf("qtransv: v(%d,%d) must be a 3-D vector",rows(vb),columns(vb)))
elseif(!is_vector(qib) | length(qib) != 4)
  error(sprintf("qtransv: q(%d,%d) must be a quaternion",rows(qib),columns(qib)))
elseif(max(abs(imag(vb))) + max(abs(imag(qib))) != 0)
  vb
  qib
  error("qtransv: input values must be real.");
endif

qr = qib(4);  qimag = vec(qib(1:3));   vb = vec(vb);
vi = (2*qr^2 - 1)*vb + 2*qimag*(qimag'*vb) + 2*qr*cross(qimag,vb);

endfunction
