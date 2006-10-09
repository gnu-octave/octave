function sparseimages(nm,typ)
  if (! isempty (findstr (octave_config_info ("DEFS"), "HAVE_COLAMD"))
      && ! isempty (findstr (octave_config_info ("DEFS"), "HAVE_CHOLMOD"))
      && ! isempty (findstr (octave_config_info ("DEFS"), "HAVE_UMFPACK")))
  if (strcmp(typ,"txt"))
    txtimages (nm, 15, typ);
  else
    if (strcmp (nm, "gplot"))
      gplotimages ("gplot", typ);
    elseif (strcmp (nm, "grid"))
      femimages ("grid", typ);
    else
      otherimages (nm, 200, typ);
    endif
  endif
  ## Kluge to give gnuplot enough time to process last figure before we
  ## exit.  Otherwise, Octave will delete the temporary data files when
  ## it exits and gnuplot will fail...
  sleep (1);
  else ## There is no sparse matrix implementation available because
       ## of missing libraries, plot sombreros instead
    sombreroimage (nm, typ);
  endif
endfunction

## Use this function before plotting commands and after every call to
## print since print() resets output to stdout (unfortunately, gnpulot
## can't pop output as it can the terminal type).
function bury_output ()
  automatic_replot (0);
  __gnuplot_set__ term dumb
  [status, dummy] = fileattrib ("/dev/null");
  if (status)
    __gnuplot_raw__ ("set output \"/dev/null\"\n");
  endif
endfunction

function gplotimages (nm, typ)
  bury_output ();
  A = sparse ([2,6,1,3,2,4,3,5,4,6,1,5],
	      [1,1,2,2,3,3,4,4,5,5,6,6], 1, 6, 6);
  xy = [0,4,8,6,4,2;5,0,5,7,5,7]';
  gplot (A, xy)
  print (strcat (nm, ".", typ), strcat ("-d", typ))
  bury_output ();
endfunction

function txtimages(nm,n,typ)
  a = 10*speye(n) + sparse(1:n,ceil([1:n]/2),1,n,n) + ...
      sparse(ceil([1:n]/2),1:n,1,n,n);
  if (strcmp (nm, "gplot") || strcmp (nm, "grid"))
    fid = fopen (sprintf ("%s.txt", nm), "wt");
    fputs (fid, "+---------------------------------+\n");
    fputs (fid, "| Image unavailable in text mode. |\n");
    fputs (fid, "+---------------------------------+\n");
    fclose (fid);
  elseif (strcmp (nm, "spmatrix"))
    printsparse(a,strcat("spmatrix.",typ));
  else
    if (!isempty(findstr(octave_config_info ("DEFS"),"HAVE_COLAMD")) &&
	!isempty(findstr(octave_config_info ("DEFS"),"HAVE_CHOLMOD")))
      if (strcmp (nm, "spchol"))
	r1 = chol(a);
	printsparse(r1,strcat("spchol.",typ));
      elseif (strcmp (nm, "spcholperm"))
	[r2,p2,q2]=chol(a);
	printsparse(r2,strcat("spcholperm.",typ));
      endif
      ## printf("Text NNZ: Matrix %d, Chol %d, PermChol %d\n",nnz(a),nnz(r1),nnz(r2));
    endif
  endif
endfunction

function otherimages(nm,n,typ)
  bury_output ();
  a = 10*speye(n) + sparse(1:n,ceil([1:n]/2),1,n,n) + ...
      sparse(ceil([1:n]/2),1:n,1,n,n);
  if (strcmp (nm, "spmatrix"))
    spy(a);
    axis("ij")
    print(strcat("spmatrix.",typ),strcat("-d",typ))
    bury_output ();
  else
    if (!isempty(findstr(octave_config_info ("DEFS"),"HAVE_COLAMD")) &&
	!isempty(findstr(octave_config_info ("DEFS"),"HAVE_CHOLMOD")))
      if (strcmp (nm, "spchol"))
	r1 = chol(a);
	spy(r1);
	axis("ij")
	print(strcat("spchol.",typ),strcat("-d",typ))
	bury_output ();
      elseif (strcmp (nm, "spcholperm"))
	[r2,p2,q2]=chol(a);
	spy(r2);
	axis("ij")
	print(strcat("spcholperm.",typ),strcat("-d",typ))
	bury_output ();
      endif
      ## printf("Image NNZ: Matrix %d, Chol %d, PermChol %d\n",nnz(a),nnz(r1),nnz(r2));
    endif
  endif
endfunction

function printsparse(a,nm)
  fid = fopen (nm,"wt");
  for i = 1:size(a,1)
    if (rem(i,5) == 0)
      fprintf (fid,"         %2d - ", i);
    else
      fprintf (fid,"            | ");
    endif
    for j = 1:size(a,2)
      if (a(i,j) == 0)
	fprintf(fid,"  ")
      else
	fprintf(fid," *")
      endif
    endfor
    fprintf(fid,"\n")
  endfor
  fprintf(fid,"            |-");
  for j=1:size(a,2)
    if (rem(j,5)==0)
      fprintf(fid,"-|");
    else
      fprintf(fid,"--");
    endif
  endfor
  fprintf(fid,"\n")
  fprintf(fid,"              ");
  for j=1:size(a,2)
    if (rem(j,5)==0)
      fprintf(fid,"%2d",j);
    else
      fprintf(fid,"  ");
    endif
  endfor
  fclose(fid);
endfunction

function femimages (nm,typ)
  bury_output ();
  if (!isempty(findstr(octave_config_info ("DEFS"),"HAVE_COLAMD")) &&
      !isempty(findstr(octave_config_info ("DEFS"),"HAVE_CHOLMOD")) &&
      !isempty(findstr(octave_config_info ("DEFS"),"HAVE_UMFPACK")))
    ## build a rectangle
    node_y = [1;1.2;1.5;1.8;2]*ones(1,11);
    node_x = ones(5,1)*[1,1.05,1.1,1.2,1.3,1.5,1.7,1.8,1.9,1.95,2];
    nodes = [node_x(:), node_y(:)];

    [h,w] = size(node_x);
    elems = [];
    for idx = 1:w-1
      widx = (idx-1)*h;
      elems = [elems; widx+[(1:h-1);(2:h);h+(1:h-1)]']; 
      elems = [elems; widx+[(2:h);h+(2:h);h+(1:h-1)]']; 
    endfor

    E = size(elems,1);  #No. of elements
    N = size(nodes,1);  #No. of elements
    D = size(elems,2);  #dimentions+1

    ## Plot FEM Geometry
    elemx = elems(:,[1,2,3,1])';
    xelems = reshape( nodes(elemx, 1), 4, E);
    yelems = reshape( nodes(elemx, 2), 4, E);

    ## Set element conductivity
    conductivity = [1*ones(1,16),2*ones(1,48),1*ones(1,16)];

    ## Dirichlet boundary conditions
    D_nodes = [1:5, 51:55]; 
    D_value = [10*ones(1,5), 20*ones(1,5)]; 
  
    ## Neumann boundary conditions
    ## Note that N_value must be normalized by the boundary
    ##   length and element conductivity
    N_nodes = [];
    N_value = [];

    ## Calculate connectivity matrix
    C = sparse((1:D*E), reshape(elems',D*E,1),1, D*E, N);

    ## Calculate stiffness matrix
    Siidx = floor([0:D*E-1]'/D)*D*ones(1,D) + ones(D*E,1)*(1:D) ;
    Sjidx = [1:D*E]'*ones(1,D);
    Sdata = zeros(D*E,D);
    dfact = prod(2:(D-1));
    for j = 1:E
      a = inv([ ones(D,1), nodes( elems(j,:), : ) ]);
      const = conductivity(j)*2/dfact/abs(det(a));
      Sdata(D*(j-1)+(1:D),:)= const * a(2:D,:)'*a(2:D,:);
    endfor

    ## Element-wise system matrix
    SE = sparse(Siidx,Sjidx,Sdata);
    ## Global system matrix
    S = C'* SE *C;

    ## Set Dirichlet boundary
    V = zeros(N,1);
    V(D_nodes) = D_value;
    idx = 1:N;
    idx(D_nodes) = [];

    ## Set Neumann boundary
    Q = zeros(N,1);
    Q(N_nodes) = N_value; # FIXME

    V(idx) = S(idx,idx)\( Q(idx) - S(idx,D_nodes)*V(D_nodes) );

    velems = reshape( V(elemx), 4, E);

    sz = size(xelems,2);
    ## FIXME How can I do this without a gnuplot specific commands? plot3 anyone?
    unwind_protect
      __gnuplot_set__  parametric;
      __gnuplot_raw__ ("set nohidden3d;\n");
      tmp = [([xelems; NaN*ones(1,sz)])(:), ([yelems; NaN*ones(1,sz)])(:), ([velems; NaN*ones(1,sz)])(:)];
      __gnuplot_splot__(tmp);
      __gnuplot_raw__ ("set view 80,10;\n")
      print(strcat(nm,".",typ),strcat("-d",typ))
      bury_output ();
    unwind_protect_cleanup
      __gnuplot_set__ noparametric; 
    end_unwind_protect
  endif
endfunction

## There is no sparse matrix implementation available because of missing
## libraries, plot sombreros instead. Also plot a nice title that we are
## sorry about that.
function sombreroimage (nm, typ)
  if (strcmp (typ, "txt"))
    fid = fopen (sprintf ("%s.txt", nm), "wt");
    fputs (fid, "+---------------------------------------+\n");
    fputs (fid, "| Image unavailable because of a        |\n");
    fputs (fid, "| missing sparse matrix implementation. |\n");
    fputs (fid, "+---------------------------------------+\n");
    fclose (fid);
    return;
  else ## if (!strcmp (typ, "txt"))

    bury_output ();

    x = y = linspace (-8, 8, 41)';
    [xx, yy] = meshgrid (x, y);
    r = sqrt (xx .^ 2 + yy .^ 2) + eps;
    z = sin (r) ./ r;
    xlen = length (x);
    ylen = length (y);
    len = 3 * xlen;
    zz = zeros (ylen, len);
    k = 1;
    for i = 1:3:len
      zz(:,i)   = x(k) * ones (ylen, 1);
      zz(:,i+1) = y;
      zz(:,i+2) = z(:,k);
      k++;
    endfor
    unwind_protect
      __gnuplot_raw__ ("set hidden3d;\n");
      __gnuplot_raw__ ("set data style lines;\n");
      __gnuplot_raw__ ("set surface;\n");
      __gnuplot_raw__ ("set nocontour;\n");
      __gnuplot_raw__ ("set nologscale;\n");
      __gnuplot_set__ parametric;
      __gnuplot_raw__ ("set view 60, 30, 1, 1;\n");
      __gnuplot_raw__ ("set nokey;\n");
      __gnuplot_raw__ ("set nocolorbox;\n");
      msg = strcat ("");
      __gnuplot_raw__ ("set title \"Sorry, graphics not available because octave was\\ncompiled without the sparse matrix implementation.\";\n");
      __plt3__ (zz, "", "");
    unwind_protect_cleanup
      __gnuplot_set__ noparametric;
      print (strcat (nm, ".", typ), strcat ("-d", typ));
      bury_output ();
    end_unwind_protect
  endif
endfunction
