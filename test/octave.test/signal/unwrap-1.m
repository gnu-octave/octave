1;

function t = assert(a,b,tol)
  if (nargin == 1)
    t = all(a(:));
  else
    if (nargin == 2)
      tol = 0;
    endif
    if (any (size(a) != size(b)))
      t = 0;
    elseif (any (abs(a(:) - b(:)) > tol))
      t = 0;
    else
      t = 1;
    endif
  endif

endfunction

i = 0;
t = [];

r = [0:100];                        # original vector
w = r - 2*pi*floor((r+pi)/(2*pi));  # wrapped into [-pi,pi]
tol = 1e3*eps;                      # maximum expected deviation

t(++i) = assert(r, unwrap(w), tol);               #unwrap single row
t(++i) = assert(r', unwrap(w'), tol);             #unwrap single column
t(++i) = assert([r',r'], unwrap([w',w']), tol);   #unwrap 2 columns
t(++i) = assert([r;r], unwrap([w;w],[],2), tol);  #verify that dim works
t(++i) = assert(r+10, unwrap(10+w), tol);         #verify that r(1)>pi works

t(++i) = assert(w', unwrap(w',[],2));  #unwrap col by rows should not change it
t(++i) = assert(w, unwrap(w,[],1));    #unwrap row by cols should not change it
t(++i) = assert([w;w], unwrap([w;w])); #unwrap 2 rows by cols should not change them

## verify that setting tolerance too low will cause bad results.
t(++i) = assert(any(abs(r - unwrap(w,0.8)) > 100));

all(t)
