function v = qtrans(v,q)
# function v = qtrans(v,q)
# transform the unit quaternion v by the unit quaternion q;
# v = [w x y z], q = transformation quaternion
# returns v = q*v/q

if(!is_vector(v) | length(v) != 4)
  error(sprintf("qtrans: v(%d,%d) must be a quaternion",rows(v),columns(v)))
elseif(!is_vector(q) | length(q) != 4)
  error(sprintf("qtrans: q(%d,%d) must be a quaternion",rows(q),columns(q)))
endif

v = qmult(q,qmult(v,qinv(q)));
