function Dmat = qderivmat(Omega)
# function Dmat = qderivmat(Omega)
# Derivative of a quaternion.
#   Let Q be a quaternion to transform a vector from a fixed frame to
#      a rotating frame.  If the rotating frame is rotating about the 
#      [x,y,z] axes at angular rates [wx, wy, wz], then the derivative
#      of Q is given by
#   Q' = qderivmat(Omega)*Q
#
#   If the passive convention is used (rotate the frame, not the vector),
#   then Q' = -qderivmat(Omega)*Q; see the accompanying document
#   quaternion.ps for details.

Omega = vec(Omega);
if(length(Omega) != 3)
   error("qderivmat: Omega must be a length 3 vector.");
endif

Dmat = 0.5*[ 0.0,  Omega(3), -Omega(2),  Omega(1); ...
 	  -Omega(3),      0.0,  Omega(1),  Omega(2); ...
  	  Omega(2), -Omega(1),      0.0,  Omega(3); ...
 	  -Omega(1), -Omega(2), -Omega(3),      0.0 ];
endfunction
