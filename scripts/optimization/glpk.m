function varargout=glpk(varargin)
## GLPK - An Octave Interface for the GNU GLPK library
##
## This routine calls the glpk library to solve an LP/MIP problem. A typical
## LP problem has following structure:
##
##           [min|max] C'x
##            s.t.
##                 Ax ["="|"<="|">="] b
##                 {x <= UB}
##                 {x >= LB}
##
## The calling syntax is:
## [XOPT,FOPT,STATUS,EXTRA]=glpkmex(SENSE,C,...
##                                      A,B,CTYPE,LB,UB,...
##                                      VARTYPE,PARAM,LPSOLVER,SAVE)
##
## For a quick reference to the syntax just type glpk at command prompt.
##
## The minimum number of input arguments is 4 (SENSE,C,A,B). In this case we
## assume all the constraints are '<=' and all the variables are continuous.
##
## --- INPUTS ---
##
## SENSE:     indicates whether the problem is a minimization
##            or maximization problem.
##            SENSE = 1 minimize
##            SENSE = -1 maximize.
##
## C:         A column array containing the objective function
##            coefficients.
##
## A:         A matrix containing the constraints coefficients.
##
## B:         A column array containing the right-hand side value for
##            each constraint in the constraint matrix.
##
## CTYPE:     A column array containing the sense of each constraint
##            in the constraint matrix.
##            CTYPE(i) = 'F'  Free (unbounded) variable
##            CTYPE(i) = 'U'  "<=" Variable with upper bound
##            CTYPE(i) = 'S'  "="  Fixed Variable
##            CTYPE(i) = 'L'  ">=" Variable with lower bound
##            CTYPE(i) = 'D'  Double-bounded variable
##            (This is case sensitive).
##
## LB:        An array of at least length numcols containing the lower
##            bound on each of the variables.
##
## UB:        An array of at least length numcols containing the upper
##            bound on each of the variables.
##
## VARTYPE:   A column array containing the types of the variables.
##            VARTYPE(i) = 'C' continuous variable
##            VARTYPE(i) = 'I' Integer variable
##            (This is case sensitive).
##
## PARAM:     A structure containing some parameters used to define
##            the behavior of solver. For more details type
##            HELP GLPKPARAMS.
##
## LPSOLVER:  Selects which solver using to solve LP problems.
##            LPSOLVER=1  Revised Simplex Method
##            LPSOLVER=2  Interior Point Method
##            If the problem is a MIP problem this flag will be ignored.
##
## SAVE:      Saves a copy of the problem if SAVE<>0.
##            The file name can not be specified and defaults to "outpb.lp".
##            The output file is CPLEX LP format.
##
## --- OUTPUTS ---
##
## XOPT:      The optimizer.
##
## FOPT:      The optimum.
##
## STATUS:    Status of the optimization.
##     
##              - Simplex Method -
##              Value   Code     
##              180     LPX_OPT     solution is optimal   
##              181     LPX_FEAS    solution is feasible
##              182     LPX_INFEAS  solution is infeasible
##              183     LPX_NOFEAS  problem has no feasible solution
##              184     LPX_UNBND   problem has no unbounded solution
##              185     LPX_UNDEF   solution status is undefined
##
##              - Interior Point Method -
##              Value   Code
##              150     LPX_T_UNDEF the interior point method is undefined
##              151     LPX_T_OPT   the interior point method is optimal
##              *  Note that additional status codes may appear in
##              the future versions of this routine *
##    
##              - Mixed Integer Method -
##              Value   Code
##              170     LPX_I_UNDEF  the status is undefined
##              171     LPX_I_OPT    the solution is integer optimal
##              172     LPX_I_FEAS   solution integer feasible but
##                                   its optimality has not been proven
##              173     LPX_I_NOFEAS no integer feasible solution
##              
## EXTRA:     A data structure containing the following fields:
##             LAMBDA     Dual variables
##             REDCOSTS   Reduced Costs
##             TIME       Time (in seconds) used for solving LP/MIP problem in seconds.
##             MEM        Memory (in bytes) used for solving LP/MIP problem.
##
##
## In case of error the glpkmex returns one of the
## following codes (these codes are in STATUS). For more informations on
## the causes of these codes refer to the GLPK reference manual.
##
## Value  Code
## 204    LPX_E_FAULT  unable to start the search
## 205    LPX_E_OBJLL  objective function lower limit reached
## 206    LPX_E_OBJUL  objective function upper limit reached
## 207    LPX_E_ITLIM  iterations limit exhausted
## 208    LPX_E_TMLIM  time limit exhausted
## 209    LPX_E_NOFEAS no feasible solution
## 210    LPX_E_INSTAB numerical instability
## 211    LPX_E_SING   problems with basis matrix
## 212    LPX_E_NOCONV no convergence (interior)
## 213    LPX_E_NOPFS  no primal feas. sol. (LP presolver)
## 214    LPX_E_NODFS  no dual feas. sol.   (LP presolver)
##

% --- CODE ---
% If there is no input output the version and syntax
if nargin==0
	printf("glpk: An Octave interface to the GLPK library\n");
	printf("Version: 1.0\n");
	printf("\nSyntax: [xopt,fopt,status,extra]=glpk(sense,c,a,b,ctype,lb,ub,vartype,param,lpsolver,save)\n");
	return;
endif

% If there are less than 4 arguments output an error message
if nargin<4
	error("At least 4 inputs required (sense,c,a,b)\n");
	return;
endif

% At least 2 outputs required
if nargout<2
	error("2 outputs required\n");
	return;
endif

% 1) Sense of optimization
sense=varargin{1};

% 2) Cost vector
c=varargin{2};

if (all(size(c)>1) | iscomplex(c) | ischar(c))
	error("C must be a real vector\n");
	return;
endif
nx=length(c);
if size(c,1) ~= nx
	c=c';
endif

% 3) Matrix constraint
a=varargin{3};
if isempty(a)
	error("A cannot be an empty matrix\n");
	return;
endif
[nc, nxa]=size(a);
if (ischar(a) | iscomplex(a) | (nxa ~= nx))
	error("A must be a real valued %d by %d matrix\n",nc,nx);
	return;
endif

% 4) RHS
b=varargin{4};
if isempty(b)
	error("B cannot be an empty vector\n");
	return;
endif
if (ischar(b) | iscomplex(b) | (length(b) ~= nc))
	error("B must be a real valued %d by 1 vector\n",nc);
	return;
endif

% 5) Sense of each constraint
ctype=[];
if nargin>4 
	ctype=varargin{5};
	if isempty(ctype)
		ctype=char('U'*ones(nc,1));
	elseif (isnumeric(ctype) | all(size(ctype)>1) | (length(ctype) ~= nc))
		error("CTYPE must be a char valued %d by 1 column vector\n",nc);
		return;
	else
		for i=1:nc
			if (ctype(i)!='F' & ctype(i)!='U' & ctype(i)!='S' & ctype(i)!='L' & ctype(i)!='D')
				error("CTYPE must contain only F,U,S,L and D\n");
				return;
			endif
		endfor
	endif
else
	ctype=char('U'*ones(nc,1));
end

% 6) Vector with the lower bound of each variable
lb=[];
if nargin>5
	lb=varargin{6};
	if isempty(lb)
		lb=-Inf*ones(nx,1);
	elseif (ischar(lb) | iscomplex(lb) | all(size(lb)>1) | (length(lb)~=nx))
		error("LB must be a real valued %d by 1 column vector\n",nx);
		return;
	endif
else
	lb=-Inf*ones(nx,1);
end

% 7) Vector with the upper bound of each variable
ub=[];
if nargin>6
	ub=varargin{7};
	if isempty(ub)
		ub=Inf*ones(nx,1);
	elseif (ischar(ub) | iscomplex(ub) | all(size(ub)>1) | (length(ub)~=nx))
		error("UB must be a real valued %d by 1 column vector\n",nx);
		return;
	endif
else
	ub=Inf*ones(nx,1);
end

% 8) Vector with the type of variables 
vartype=[];
if nargin>7
	vartype=varargin{8};
	if isempty(vartype)
		vartype=char('C'*ones(nx,1));
	elseif (isnumeric(vartype) | all(size(vartype)>1) | (length(vartype)~=nx))
		error("VARTYPE must be a char valued %d by 1 column vector\n",nx);
		return;
	else
		for i=1:nx
			if (vartype(i)!='C' & vartype(i)!='I')
				error("VARTYPE must contain only C or I\n");
				return;
			endif
		endfor
	endif
else
	vartype=char('C'*ones(nx,1)); % As default we consider continuous vars
endif

% 9) Parameters vector
param=[];
if nargin>8
	param=varargin{9};
	if !isstruct(param)
		error("PARAM must be a structure\n");
		return;
	endif
else
	param=struct;
endif

% 10) Select solver method: simplex or interior point
lpsolver=[];
if nargin>9
	lpsolver=varargin{10};
	if (!isnumeric(lpsolver) | all(size(lpsolver)>1))
		error("LPSOLVER must be a real scalar value\n");
		return;
	endif
else
	lpsolver=1;
endif

% 11) Save the problem
savepb=[];
if nargin>10
	savepb=varargin{11};
	if (!isnumeric(savepb) | all(size(savepb)>1))
		error("LPSOLVER must be a real scalar value\n");
		return;
	endif
else
	savepb=0;
end

if nargin>11
	warning("Extra parameters ignored\n");
endif

try
	[xopt, fmin, status, extra]=glpkoct(sense,c,a,b,ctype,lb,ub,vartype,param,lpsolver,savepb);
catch
	error("Problems with glpkoct");
end_try_catch

varargout{1}=xopt;
varargout{2}=fmin;
varargout{3}=status;
varargout{4}=extra;

	
endfunction
