function umfpack_make
% UMFPACK_MAKE
%
% Compiles the UMFPACK mexFunction and then runs a simple demo.
%
% See also: umfpack, umfpack_details, umfpack_report, umfpack_demo, and
% umfpack_simple.
%
% UMFPACK Version 4.4, Copyright (c) 2005 by Timothy A. Davis.
% All Rights Reserved.  Type umfpack_details for License.

help umfpack_make

fprintf ('\n--------------------------------------------------------------\n') ;
fprintf ('Now compiling the UMFPACK and AMD mexFunctions.\n') ;
fprintf ('--------------------------------------------------------------\n') ;

try
    % ispc does not appear in MATLAB 5.3
    pc = ispc ;
catch
    % if ispc fails, assume we aren't on a Windows PC.
    pc = 0 ;
end

obj = 'o' ;
blas_lib = '' ;
if (pc)
    obj = 'obj' ;
end

%-------------------------------------------------------------------------------
% BLAS option
%-------------------------------------------------------------------------------

msg = [ ...
    '\nUsing the BLAS is faster, but might not compile correctly.\n', ...
    'If you get an error stating that dgemm, dgemv, dger, zgemm,\n', ...
    'zgemv, and/or zger are not defined, then recompile without the\n', ...
    'BLAS.  You can ignore warnings that these routines are implicitly\n', ...
    'declared.\n\nPlease select one of the following options: \n', ...
    '   1:  attempt to compile with the BLAS (default)\n', ...
    '   2:  do not use the BLAS\n'] ;
fprintf (msg) ;
blas = input (': ') ;
if (isempty (blas))
    blas = 1 ;
end
if (blas == 1)
    % try to link to MATLAB's built-in BLAS
    blas = '' ;
    if (pc)
        % the default lcc compiler needs this library to access the BLAS
        blas_lib = ' libmwlapack.lib' ;
        msg = [ ...
        '\nCheck to see if you have a file called libmwlapack.lib in the\n', ...
        '<matlab>\\extern\\lib\\win32\\lcc\\ directory, where <matlab> is ', ...
        'the\ndirectory where MATLAB is installed.  If a file of that ', ...
        'name is already\nthere, then you don''t have to do anything.  ', ...
        'Otherwise, you must first\ncopy the libmwlapack.lib file from ', ...
        'the umfpack\\lcc_lib\\ directory to the\n', ...
        '<matlab>\\extern\\lib\\win32\\lcc\\ directory.  Next, type\n\n', ...
        '    mex -setup\n\n', ...
        'at the MATLAB prompt, and ask MATLAB to select the lcc compiler.  ',...
        'You can skip\nall of this if you have already done it, or have ', ...
        'configured mex to use\na different compiler.  If you are using ', ...
        'Norton anti-virus software on Windows\n98SE, then you need to ', ...
        'exit MATLAB, turn off virus checking, and restart MATLAB\n', ...
        'before you can use the mex command or compile UMFPACK.\n', ...
        'You may also need to turn off virus checking in other cases.\n', ...
        '\nHit enter to continue, or type control-C if you do not wish to '] ;
        fprintf (msg) ;
        input ('proceed: ') ;
    end
    fprintf ('\nUsing the BLAS (recommended).\n') ;
else
    % No BLAS
    fprintf ('\nNot using the BLAS.  UMFPACK will be slow.\n') ;
    blas = ' -DNBLAS' ;
end

%-------------------------------------------------------------------------------
% -DNUTIL option (using utMalloc or mxMalloc)
%-------------------------------------------------------------------------------

utils = '' ;

if (~pc)
    msg = [ ...
    '--------------------------------------------------------------\n', ...
    '\nUMFPACK uses MATLAB''s memory allocation routines.  The internal', ...
    '\nutMalloc, utFree, and utRealloc allow for better use of memory,', ...
    '\nbut they are internal utility routines that are not documented.\n', ...
    'Thus, they might not always work.  Using mxMalloc, mxFree, and\n', ...
    'mxRealloc works, but UMFPACK might run out of memory when solving\n', ...
    'problems that it could otherwise solve.  Try using the default.\n', ...
    'If you get an error stating that utMalloc, utFree, and/or\n', ...
    'utRealloc are not defined, then recompile with the mx* routines.\n', ...
    '\nPlease select one of the following options:\n', ...
    '    1:  attempt to use the ut* routines (default)\n', ...
    '    2:  use the standard mx* routines\n'] ;
    fprintf (msg) ;
    utils = input (': ') ;
    if (isempty (utils))
	utils = 1 ;
    end
    if (utils == 2)
        fprintf ('\nNot using utMalloc, utFree, or utRealloc\n') ;
        utils = ' -DNUTIL' ;
    else
        fprintf ('\nUsing utMalloc, utFree, and utRealloc\n') ;
        utils = '' ;
    end
end

%-------------------------------------------------------------------------------
% -DNPOSIX option (for sysconf and times timer routines)
%-------------------------------------------------------------------------------

posix = '' ;

if (~pc)
    msg = [ ...
    '--------------------------------------------------------------\n', ...
    '\nUMFPACK can use the POSIX routines sysconf () and times ()\n', ...
    'to provide CPU time and wallclock time statistics.  If you do not\n', ...
    'have a POSIX-compliant operating system, then UMFPACK won''t\n', ...
    'compile.  If you don''t know which option to pick, try the\n', ...
    'default.  If you get an error saying that sysconf and/or times\n', ...
    'are not defined, then recompile with the non-POSIX option.\n', ...
    '\nPlease select one of the following options:\n', ...
    '    1:  use POSIX sysconf and times routines (default)\n', ...
    '    2:  do not use POSIX routines\n'] ;
    fprintf (msg) ;
    posix = input (': ') ;
    if (isempty (posix))
	posix = 1 ;
    end
    if (posix == 2)
        fprintf ('\nNot using POSIX sysconf and times routines.\n') ;
        posix = ' -DNPOSIX' ;
    else
        fprintf ('\nUsing POSIX sysconf and times routines.\n') ;
        posix = '' ;
    end
end

%-------------------------------------------------------------------------------
% mex command
%-------------------------------------------------------------------------------

umfdir = sprintf ('..%sSource%s', filesep, filesep) ;
amddir = sprintf ('..%s..%sAMD%sSource%s', filesep, filesep, filesep, filesep) ;
incdir = sprintf ( ...
' -I..%sInclude -I..%sSource -I..%s..%sAMD%sInclude -I..%s..%sAMD%sSource', ...
filesep,filesep,  filesep, filesep, filesep, filesep, filesep, filesep) ;

mx = sprintf ('mex -inline -O%s%s%s%s', blas, utils, posix, incdir) ;
msg = [ ...
    '--------------------------------------------------------------\n', ...
    '\nCompile options:\n%s\nNow compiling.  Please wait.\n'] ;
fprintf (msg, mx) ;

% The following is adapted from GNUmakefile

%-------------------------------------------------------------------------------
% source files
%-------------------------------------------------------------------------------

% non-user-callable umf_*.[ch] files:
umfch = { 'assemble', 'blas3_update', ...
        'build_tuples', 'create_element', ...
        'dump', 'extend_front', 'garbage_collection', ...
        'get_memory', 'init_front', 'kernel', ...
        'kernel_init', 'kernel_wrapup', ...
        'local_search', 'lsolve', 'ltsolve', ...
        'mem_alloc_element', 'mem_alloc_head_block', ...
        'mem_alloc_tail_block', 'mem_free_tail_block', ...
        'mem_init_memoryspace', ...
        'report_vector', 'row_search', 'scale_column', ...
        'set_stats', 'solve', 'symbolic_usage', 'transpose', ...
        'tuple_lengths', 'usolve', 'utsolve', 'valid_numeric', ...
        'valid_symbolic', 'grow_front', 'start_front', '2by2', ...
	'store_lu', 'scale' } ;

% non-user-callable umf_*.[ch] files, int versions only (no real/complex):
umfint = { 'analyze', 'apply_order', 'colamd', 'free', 'fsize', ...
        'is_permutation', 'malloc', 'realloc', 'report_perm', ...
	'singletons' } ;

% non-user-callable and user-callable amd_*.[ch] files (int versions only):
amd = { 'aat', '1', '2', 'dump', 'postorder', 'post_tree', 'defaults', ...
        'order', 'control', 'info', 'valid', 'preprocess' } ;

% user-callable umfpack_*.[ch] files (real/complex):
user = { 'col_to_triplet', 'defaults', 'free_numeric', ...
        'free_symbolic', 'get_numeric', 'get_lunz', ...
        'get_symbolic', 'get_determinant', 'numeric', 'qsymbolic', ...
        'report_control', 'report_info', 'report_matrix', ...
        'report_numeric', 'report_perm', 'report_status', ...
        'report_symbolic', 'report_triplet', ...
        'report_vector', 'solve', 'symbolic', ...
        'transpose', 'triplet_to_col', 'scale' ...
	'load_numeric', 'save_numeric', 'load_symbolic', 'save_symbolic' } ;

% user-callable umfpack_*.[ch], only one version
generic = { 'timer', 'tictoc' } ;

M = cell (0) ;

%-------------------------------------------------------------------------------
% Create the umfpack and amd mexFunctions for MATLAB (int versions only)
%-------------------------------------------------------------------------------

for k = 1:length(umfint)
    M = make (M, '%s -DDINT -c %sumf_%s.c', 'umf_%s.%s', 'umf_%s_%s.%s', ...
        mx, umfint {k}, umfint {k}, 'm', obj, umfdir) ;
end

rules = { [mx ' -DDINT'] , [mx ' -DZINT'] } ;
kinds = { 'md', 'mz' } ;

for what = 1:2

    rule = rules {what} ;
    kind = kinds {what} ;

    M = make (M, '%s -DCONJUGATE_SOLVE -c %sumf_%s.c', 'umf_%s.%s', ...
        'umf_%s_%s.%s', rule, 'ltsolve', 'lhsolve', kind, obj, umfdir) ;

    M = make (M, '%s -DCONJUGATE_SOLVE -c %sumf_%s.c', 'umf_%s.%s', ...
        'umf_%s_%s.%s', rule, 'utsolve', 'uhsolve', kind, obj, umfdir) ;

    M = make (M, '%s -DDO_MAP -c %sumf_%s.c', 'umf_%s.%s', ...
        'umf_%s_%s_map_nox.%s', rule, 'triplet', 'triplet', kind, obj, umfdir) ;

    M = make (M, '%s -DDO_VALUES -c %sumf_%s.c', 'umf_%s.%s', ...
        'umf_%s_%s_nomap_x.%s', rule, 'triplet', 'triplet', kind, obj, umfdir) ;

    M = make (M, '%s -c %sumf_%s.c', 'umf_%s.%s',  ...
        'umf_%s_%s_nomap_nox.%s', rule, 'triplet', 'triplet', kind, obj, ...
	umfdir) ;

    M = make (M, '%s -DDO_MAP -DDO_VALUES -c %sumf_%s.c', 'umf_%s.%s', ...
        'umf_%s_%s_map_x.%s', rule, 'triplet', 'triplet', kind, obj, umfdir) ;

    M = make (M, '%s -DFIXQ -c %sumf_%s.c', 'umf_%s.%s', ...
	'umf_%s_%s_fixq.%s', rule, 'assemble', 'assemble', kind, obj, umfdir) ;

    M = make (M, '%s -DDROP -c %sumf_%s.c', 'umf_%s.%s', ...
	'umf_%s_%s_drop.%s', rule, 'store_lu', 'store_lu', kind, obj, umfdir) ;

    for k = 1:length(umfch)
        M = make (M, '%s -c %sumf_%s.c', 'umf_%s.%s', 'umf_%s_%s.%s', ...
            rule, umfch {k}, umfch {k}, kind, obj, umfdir) ;
    end

    M = make (M, '%s -DWSOLVE -c %sumfpack_%s.c', 'umfpack_%s.%s', ...
        'umfpack_%s_w%s.%s', rule, 'solve', 'solve', kind, obj, umfdir) ;

    for k = 1:length(user)
        M = make (M, '%s -c %sumfpack_%s.c', 'umfpack_%s.%s', ...
            'umfpack_%s_%s.%s', rule, user {k}, user {k}, kind, obj, umfdir) ;
    end
end

for k = 1:length(generic)
    M = make (M, '%s -c %sumfpack_%s.c', 'umfpack_%s.%s', ...
	'umfpack_%s_%s.%s', mx, generic {k}, generic {k}, 'm', obj, umfdir) ;
end

%----------------------------------------
% AMD routines (int only)
%----------------------------------------

for k = 1:length(amd)
    M = make (M, '%s -DDINT -c %samd_%s.c', 'amd_%s.%s', 'amd_%s_%s.%s', ...
        mx, amd {k}, amd {k}, 'm', obj, amddir) ;
end

%----------------------------------------
% compile the umfpack mexFunction
%----------------------------------------

C = sprintf ('%s -output umfpack umfpackmex.c', mx) ;
for i = 1:length (M)
    C = [C ' ' (M {i})] ;
end
C = [C ' ' blas_lib] ;
cmd (C) ;

%----------------------------------------
% delete the object files
%----------------------------------------

for i = 1:length (M)
    rmfile (M {i}) ;
end

%----------------------------------------
% compile the luflop mexFunction
%----------------------------------------

cmd (sprintf ('%s -output luflop luflopmex.c', mx)) ;

fprintf ('\n\nCompilation has completed.  Now trying the umfpack_simple demo.\n');
umfpack_simple

%-------------------------------------------------------------------------------
% rmfile:  delete a file, but only if it exists
%-------------------------------------------------------------------------------

function rmfile (file)
if (length (dir (file)) > 0)
    delete (file) ;
end

%-------------------------------------------------------------------------------
% cpfile:  copy the src file to the filename dst, overwriting dst if it exists
%-------------------------------------------------------------------------------

function cpfile (src, dst)
rmfile (dst)
if (length (dir (src)) == 0)
    help umfpack_make
    error (sprintf ('File does not exist: %s\n', src)) ;
end
copyfile (src, dst) ;

%-------------------------------------------------------------------------------
% mvfile:  move the src file to the filename dst, overwriting dst if it exists
%-------------------------------------------------------------------------------

function mvfile (src, dst)
cpfile (src, dst) ;
rmfile (src) ;

%-------------------------------------------------------------------------------
% cmd:  display and execute a command
%-------------------------------------------------------------------------------

function cmd (s)
fprintf ('.') ;
eval (s) ;

%-------------------------------------------------------------------------------
% make:  execute a "make" command for a source file
%-------------------------------------------------------------------------------

function M = make (M, s, src, dst, rule, file1, file2, kind, obj, srcdir)
cmd (sprintf (s, rule, srcdir, file1)) ;
src = sprintf (src, file1, obj) ;
dst = sprintf (dst, kind, file2, obj) ;
mvfile (src, dst) ;
M {end + 1} = dst ;

