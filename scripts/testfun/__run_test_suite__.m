## Copyright (C) 2005-2018 David Bateman
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} __run_test_suite__ (@var{fcndirs}, @var{fixedtestdirs})
## @deftypefnx {} {} __run_test_suite__ (@var{fcndirs}, @var{fixedtestdirs}, @var{topsrcdir}, @var{topbuilddir})
## Undocumented internal function.
## @end deftypefn

function [pass, fail, xfail, xbug, skip, rtskip, regress] = __run_test_suite__ (fcndirs, fixedtestdirs, topsrcdir = [], topbuilddir = [])

  testsdir = __octave_config_info__ ("octtestsdir");
  libinterptestdir = fullfile (testsdir, "libinterp");
  liboctavetestdir = fullfile (testsdir, "liboctave");
  fixedtestdir = fullfile (testsdir, "fixed");
  fcnfiledir = __octave_config_info__ ("fcnfiledir");
  if (nargin == 0)
    fcndirs = { liboctavetestdir, libinterptestdir, fcnfiledir };
    fixedtestdirs = { fixedtestdir };
  endif
  files_with_no_tests = {};
  files_with_tests = {};
  ## FIXME: These names don't really make sense if we are running
  ##        tests for an installed copy of Octave.
  if (isempty (topsrcdir))
    topsrcdir = fcnfiledir;
  endif
  if (isempty (topbuilddir))
    topbuilddir = testsdir;
  endif

  pso = page_screen_output ();
  orig_wstate = warning ();
  logfile = make_absolute_filename ("fntests.log");
  unwind_protect
    page_screen_output (false);
    warning ("on", "quiet");
    warning ("off", "Octave:deprecated-function");
    warning ("off", "Octave:legacy-function");
    nfail = dp = dn = dxf = dxb = dsk = drtsk = drgrs = 0;
    try
      fid = fopen (logfile, "wt");
      if (fid < 0)
        error ("__run_test_suite__: could not open %s for writing", logfile);
      endif
      test ("", "explain", fid);
      puts ("\nIntegrated test scripts:\n\n");
      for i = 1:length (fcndirs)
        [p, n, xf, xb, sk, rtsk, rgrs] = run_test_script (fid, fcndirs{i});
        dp += p;
        dn += n;
        dxf += xf;
        dxb += xb;
        dsk += sk;
        drtsk += rtsk;
        drgrs += rgrs;
      endfor
      puts ("\nFixed test scripts:\n\n");
      for i = 1:length (fixedtestdirs)
        [p, n, xf, xb, sk, rtsk, rgrs] = run_test_dir (fid, fixedtestdirs{i});
        dp += p;
        dn += n;
        dxf += xf;
        dxb += xb;
        dsk += sk;
        drtsk += rtsk;
        drgrs += rgrs;
      endfor
      puts ("\nSummary:\n\n");
      nfail = dn - dp - dxf - dxb - drgrs;
      printf ("  %-30s %6d\n", "PASS", dp);
      printf ("  %-30s %6d\n", "FAIL", nfail);
      if (drgrs > 0)
        printf ("  %-30s %6d\n", "REGRESSION", drgrs);
      endif
      if (dxb > 0)
        printf ("  %-30s %6d\n", "XFAIL (reported bug)", dxb);
      endif
      if (dxf > 0)
        printf ("  %-30s %6d\n", "XFAIL (expected failure)", dxf);
      endif
      if (dsk > 0)
        printf ("  %-30s %6d\n", "SKIP (missing feature)", dsk);
      endif
      if (drtsk > 0)
        printf ("  %-30s %6d\n", "SKIP (run-time condition)", drtsk);
      endif
      puts ("\n");
      printf ("See the file %s for additional details.\n", logfile);
      if (dxf > 0 || dxb > 0)
        puts ("\n");
        puts ("XFAIL items are known bugs or expected failures.\n");
        puts ("Bug report numbers may be found in the log file:\n");
        puts (logfile);
        puts ("\nPlease help improve Octave by contributing fixes for them.\n");
      endif
      if (dsk > 0 || drtsk > 0)
        puts ("\n");
        puts ("Tests are often skipped because required features were\n");
        puts ("disabled or were not present when Octave was built.\n");
        puts ("The configure script should have printed a summary\n");
        puts ("indicating which dependencies were not found.\n");
      endif

      ## Weed out deprecated, legacy, and private functions
      weed_idx = cellfun (@isempty, regexp (files_with_tests, '\<deprecated\>|\<legacy\>|\<private\>', 'once'));
      files_with_tests = files_with_tests(weed_idx);
      weed_idx = cellfun (@isempty, regexp (files_with_no_tests, '\<deprecated\>|\<legacy\>|\<private\>', 'once'));
      files_with_no_tests = files_with_no_tests(weed_idx);

      report_files_with_no_tests (files_with_tests, files_with_no_tests, ".m");

      puts ("\nPlease help improve Octave by contributing tests for these files\n");
      printf ("(see the list in the file %s).\n\n", logfile);

      fprintf (fid, "\nFiles with no tests:\n\n%s",
                    list_in_columns (files_with_no_tests, 80));
      fclose (fid);
    catch
      disp (lasterr ());
    end_try_catch
  unwind_protect_cleanup
    warning ("off", "all");
    warning (orig_wstate);
    page_screen_output (pso);
  end_unwind_protect

  if (nargout > 0)
    pass = dp;
    fail = nfail;
    xfail = dxf;
    xbug = dxb;
    skip = dsk;
    rtskip = drtsk;
    regress = drgrs;
  endif


  function [dp, dn, dxf, dxb, dsk, drtsk, drgrs] = run_test_dir (fid, d)

    lst = dir (d);
    dp = dn = dxf = dxb = dsk = drtsk = drgrs = 0;
    for i = 1:length (lst)
      nm = lst(i).name;
      if (lst(i).isdir
          && nm(1) != "." && ! strcmp (nm, "private") && nm(1) != "@")
        [p, n, xf, xb, sk, rtsk, rgrs] = run_test_dir (fid, [d, filesep, nm]);
        dp += p;
        dn += n;
        dxf += xf;
        dxb += xb;
        dsk += sk;
        drtsk += rtsk;
        drgrs += rgrs;
      endif
    endfor

    saved_dir = pwd ();
    unwind_protect
      cd (d);
      for i = 1:length (lst)
        nm = lst(i).name;
        if (length (nm) > 4 && strcmpi (nm((end-3):end), ".tst"))
          p = n = xf = xb = sk = rtsk = 0;
          ffnm = fullfile (d, nm);
          if (has_tests (ffnm))
            print_test_file_name (nm);
            [p, n, xf, xb, sk, rtsk, rgrs] = test (nm, "quiet", fid);
            print_pass_fail (p, n, xf, xb, sk, rtsk, rgrs);
            files_with_tests(end+1) = ffnm;
          else
            files_with_no_tests(end+1) = ffnm;
          endif
          dp += p;
          dn += n;
          dxf += xf;
          dxb += xb;
          dsk += sk;
          drtsk += rtsk;
          drgrs += rgrs;
        endif
      endfor
    unwind_protect_cleanup
      cd (saved_dir);
    end_unwind_protect

  endfunction

  function [dp, dn, dxf, dxb, dsk, drtsk, drgrs] = run_test_script (fid, d)

    lst = dir (d);
    dp = dn = dxf = dxb = dsk = drtsk = drgrs = 0;
    for i = 1:length (lst)
      nm = lst(i).name;
      if (lst(i).isdir && nm(1) != ".")
        [p, n, xf, xb, sk, rtsk, rgrs] = run_test_script (fid, [d, filesep, nm]);
        dp += p;
        dn += n;
        dxf += xf;
        dxb += xb;
        dsk += sk;
        drtsk += rtsk;
        drgrs += rgrs;
      endif
    endfor

    for i = 1:length (lst)
      nm = lst(i).name;
      ## Ignore hidden files
      if (nm(1) == '.')
        continue
      endif
      f = fullfile (d, nm);
      if ((length (nm) > 2 && strcmpi (nm((end-1):end), ".m"))
          || (length (nm) > 4
              && (   strcmpi (nm((end-3):end), "-tst")
                  || strcmpi (nm((end-3):end), ".tst"))))
        p = n = xf = xb = 0;
        ## Only run if contains %!test, %!assert, %!error, %!fail, or %!warning
        if (has_tests (f))
          tmp = reduce_test_file_name (f, topbuilddir, topsrcdir);
          print_test_file_name (tmp);
          [p, n, xf, xb, sk, rtsk, rgrs] = test (f, "quiet", fid);
          print_pass_fail (p, n, xf, xb, sk, rtsk, rgrs);
          dp += p;
          dn += n;
          dxf += xf;
          dxb += xb;
          dsk += sk;
          drtsk += rtsk;
          drgrs += rgrs;
          files_with_tests(end+1) = f;
        else
          ## To reduce the list length, only mark .cc files that contain
          ## DEFUN definitions.
          files_with_no_tests(end+1) = f;
        endif
      endif
    endfor
    ##  printf("%s%s -> passes %d of %d tests\n", ident, d, dp, dn);

  endfunction

endfunction

function print_test_file_name (nm)
  filler = repmat (".", 1, 60-length (nm));
  printf ("  %s %s", nm, filler);
endfunction

function print_pass_fail (p, n, xf, xb, sk, rtsk, rgrs)

  if ((n + sk + rtsk + rgrs) > 0)
    printf (" PASS   %4d/%-4d", p, n);
    nfail = n - p - xf - xb - rgrs;
    if (nfail > 0)
      printf ("\n%71s %3d", "FAIL ", nfail);
    endif
    if (rgrs > 0)
      printf ("\n%71s %3d", "REGRESSION", rgrs);
    endif
    if (sk > 0)
      printf ("\n%71s %3d", "(missing feature) SKIP ", sk);
    endif
    if (rtsk > 0)
      printf ("\n%71s %3d", "(run-time condition) SKIP ", rtsk);
    endif
    if (xb > 0)
      printf ("\n%71s %3d", "(reported bug) XFAIL", xb);
    endif
    if (xf > 0)
      printf ("\n%71s %3d", "(expected failure) XFAIL", xf);
    endif
  endif
  puts ("\n");

endfunction

function retval = reduce_test_file_name (nm, builddir, srcdir)

  ## Reduce the given absolute file name to a relative path by removing one
  ## of the likely root directory prefixes.

  prefix = { builddir, fullfile(builddir, "scripts"), ...
             srcdir, fullfile(srcdir, "scripts") };

  retval = nm;

  for i = 1:length (prefix)
    tmp = strrep (nm, [prefix{i}, filesep], "");
    if (length (tmp) < length (retval))
      retval = tmp;
    endif
  endfor

endfunction

function retval = has_functions (f)

  n = length (f);
  if (n > 3 && strcmpi (f((end-2):end), ".cc"))
    fid = fopen (f);
    if (fid < 0)
      error ("__run_test_suite__: fopen failed: %s", f);
    endif
    str = fread (fid, "*char")';
    fclose (fid);
    retval = ! isempty (regexp (str,'^(DEFUN|DEFUN_DLD)\>',
                                    'lineanchors', 'once'));
  elseif (n > 2 && strcmpi (f((end-1):end), ".m"))
    retval = true;
  else
    retval = false;
  endif

endfunction

function retval = has_tests (f)

  fid = fopen (f);
  if (fid < 0)
    error ("__run_test_suite__: fopen failed: %s", f);
  endif

  str = fread (fid, "*char")';
  fclose (fid);
  retval = ! isempty (regexp (str,
                              '^%!(assert|error|fail|test|xtest|warning)',
                              'lineanchors', 'once'));

endfunction

function n = num_elts_matching_pattern (lst, pat)
  n = sum (! cellfun ("isempty", regexp (lst, pat, 'once')));
endfunction

function report_files_with_no_tests (with, without, typ)
  pat = ['\' typ "$"];
  n_with = num_elts_matching_pattern (with, pat);
  n_without = num_elts_matching_pattern (without, pat);
  n_tot = n_with + n_without;
  printf ("\n%d (of %d) %s files have no tests.\n", n_without, n_tot, typ);
endfunction


## No test coverage for internal function.  It is tested through calling fcn.
%!assert (1)
