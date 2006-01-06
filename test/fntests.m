clear all;

global topsrcdir;

currdir = canonicalize_file_name (".");

if (nargin == 1)
  xdir = argv{1};
else
  xdir = ".";
endif

srcdir = canonicalize_file_name (xdir);
topsrcdir = canonicalize_file_name (fullfile (xdir, ".."));

if (strcmp (currdir, srcdir))
  testdirs = {srcdir};
else
  testdirs = {currdir, srcdir};
endif

src_tree = canonicalize_file_name (fullfile (topsrcdir, "src"));
script_tree = canonicalize_file_name (fullfile (topsrcdir, "scripts"));
fundirs = {src_tree, script_tree};

function [dp, dn] = runtestdir (fid, d);
  lst = dir(d);
  dp = dn = 0;
  for i = 1:length(lst)
    nm = lst(i).name;
    if (length(nm) > 5 && strcmp(nm(1:5),"test_") && 
	strcmp(nm((end-1):end),".m"))
      [p,n] = test(nm(1:(end-2)),"quiet",fid);
      printf("  %s -> passes %d of %d\n",nm,p,n);
      dp +=p;
      dn += n;
    endif
  endfor
endfunction

function y = hastests (f)
  fid = fopen(f);
  str = fscanf(fid,"%s");
  if (findstr(str,"%!test") ||
      findstr(str,"%!assert") ||
      findstr(str,"%!error") ||
      findstr(str,"%!warning"))
    y = 1;
  else
    y = 0;
  endif
  fclose(fid);
endfunction

function [dp, dn] = runtestscript (fid, d, ident);
  global topsrcdir
  lst = dir(d);
  dp = dn = 0;
  for i=1:length(lst)
    nm = lst(i).name;
    if (lst(i).isdir && !strcmp(nm,".") && !strcmp(nm,"..") && 
	!strcmp(nm,"CVS"))
      [p, n] = runtestscript (fid, [d, "/",nm], [ident,"  "]);
      dp += p;
      dn += n;
    endif
  endfor
  for i=1:length(lst)
    nm = lst(i).name;
    if ((length(nm) > 3 && strcmp(nm((end-2):end),".cc")) ||
	(length(nm) > 2 && strcmp(nm((end-1):end),".m")))
      f = fullfile (d, nm);
      ## Only run if it contains %!test, %!assert %!error or %!warning
      if (hastests(f))
	[p, n] = test(f,"quiet",fid);
	dp += p;
	dn += n;
      endif
    endif
  endfor 
  printf("%s%s -> passes %d of %d tests\n", ident,
	 strrep (d, topsrcdir, "..."), dp, dn);
##  printf("%s%s -> passes %d of %d tests\n", ident, d, dp, dn);
endfunction

function printf_assert(varargin)
  global _assert_printf;
  _assert_printf=cat(2,_assert_printf,sprintf(varargin{:}));
endfunction

function ret = prog_output_assert(str)
  global _assert_printf;
  if (isempty(_assert_printf))
    ret = isempty(str);
  elseif (_assert_printf(end) == "\n")
    ret = strcmp(_assert_printf(1:(end-1)),str);
  else
    ret = strcmp(_assert_printf,str);
  endif
  _assert_printf = "";
endfunction

pso = page_screen_output;
warn_state = warning("query","quiet");
warning("on","quiet");
try
  page_screen_output =0;
  fid=fopen("fntests.log","wt");
  if (fid < 0)
    error("could not open fntests.log for writing");
  endif
  test("","explain",fid);
  dp=dn=0;
  printf("\nIntegrated test scripts\n");
  for i=1:length(fundirs)
    [p,n] = runtestscript(fid,fundirs{i}," ");
    dp+=p;
    dn+=n;
  endfor
  printf("\nFixed test scripts\n");
  for i=1:length(testdirs)
    [p, n] = runtestdir (fid, testdirs{i});
    dp+=p;
    dn+=n;
  endfor
  printf("---> passes %d out of %d tests",dp,dn); 
  if dp==dn 
    printf("\n---> success"); 
  endif;
  disp("");
  printf("see fntests.log for details");
  disp("");
  fclose(fid);
  page_screen_output = pso;
  warning(warn_state.state,"quiet");
catch
  page_screen_output = pso;
  warning(warn_state.state,"quiet");
  disp(lasterr());
end_try_catch

