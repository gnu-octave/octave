## Helpful macros for debugging Octave.

## Display a dim-vector object.

define display-dims
  echo ndims: \ 
  output $arg0.rep[-1]
  echo \ndims: \ 
  output *$arg0.rep@$arg0.rep[-1]
  echo \n
  dont-repeat
end

## Display a dense array object.

define display-dense-array
  echo array object: \ 
  output $arg0
  echo \ndimensions:\n
  display-dims $arg0.dimensions
  echo \nrep = \ 
  output *$arg0.rep
  echo \nrep.data = \ 
  output *$arg0.rep.data@$arg0.rep.len
  echo \n
  dont-repeat
end

## Display a sparse array object.

define display-sparse-array
  echo sparse object: \ 
  output $arg0
  echo \ndimensions.rep[0] = \ 
  display-dims $arg0.dimensions
  echo \ndimensions.rep[1] = \ 
  output $arg0.dimensions.rep[1]
  echo \nrep = \ 
  output *$arg0.rep
  echo \nrep.d = \ 
  output *$arg0.rep.d@$arg0.rep.nzmx
  echo \nrep.r = \ 
  output *$arg0.rep.r@$arg0.rep.nzmx
  echo \nrep.c = \ 
  output *$arg0.rep.c@($arg0.rep.ncols+1)
  echo \n
  dont-repeat
end
