function bench (varargin)

  % The tests to run
  %
  % {name, {arg_type, n}, ...}
  tests = get_bench_conf ();

  reg = '';
  calibrate = 0;
  do_both = 1;
  n_factor = 1;
  no_compile = 0;
  filter = "";
  i = 1;
  while i <= nargin
    arg = varargin{i++};
    if strcmp (arg, "reg")
      assert (i <= nargin)
      reg = varargin{i++};
    elseif strcmp (arg, "calibrate")
      calibrate = 1;
    elseif strcmp (arg, "n_factor")
      assert (i <= nargin)
      n_factor = varargin{i++};
    elseif strcmp (arg, "nocompile")
      no_compile = 1;
    end
  end

  % For compatibility with older releases and Matlab
  if ~exist("vm_compile")
    vm_compile = @(varargin) true;
  end
  if ~exist("__dummy_mark_1__")
    __dummy_mark_1__ = @() true;
  end
  if ~exist("__dummy_mark_2__")
    __dummy_mark_2__ = @() true;
  end

  cal_res = {};

  for i = 1:length(tests)

    test = tests{i};
    name = test{1};
    complexity = test{3};
    also_compile = test{4};
    is_script = test{5};
    j = 1;

    % Skip on not matching regex, if there is one
    if length (reg) && isempty (regexp (name, reg))
      continue;
    end

    fn = str2func (name);

    printf ("%s:\n", name);

    n = 0;
    arg = 0;
    conf = test{2};
    conf_type = conf {1}; %"n", "rand sq" etc
    n_norm = conf{2};

    if strcmp (conf_type, "n")
      n = round (conf{2} * n_factor);
      arg = n;
    elseif strcmp (conf_type, "rand sq")
      if exist ("rng") % rng is a kinda new function. Keep backwards compatibility
        rng (0); % Reset rng
      end
      n = round (conf{2} * n_factor);
      arg = randn (n);
    elseif strcmp (conf_type, "rand rowvec")
      if exist ("rng")
        rng (0); % Reset rng
      end
      n = round (conf{2} * n_factor);
      arg = randn (n, 1);
    end
    n = round (n);

    iters = 1:1;
    if calibrate
      iters = 1:40;
      e_i = 0;
    end

    for j = iters

      if strcmp (conf_type, "n")
        n = round (n_norm * n_factor);
        arg = n;
      elseif strcmp (conf_type, "rand sq")
        if exist ("rng")
          rng (0); % Reset rng
        end
        n = round (n_norm * n_factor);
        arg = randn (n);
      elseif strcmp (conf_type, "rand rowvec")
        if exist ("rng")
          rng (0); % Reset rng
        end
        n = round (n_norm * n_factor);
        arg = randn (n, 1);
      end
      n = round (n);


      tic;
      [ccttot0, cctuser0, cctsys0] = cputime;
      if !no_compile
        if ! vm_compile (name)
          warning ("Could not compile %s, skipping ...", name)
          continue;
        end
      end
      [ccttot1, cctuser1, cctsys1] = cputime;
      cctwall = toc;

      try
        [cttot0, ctuser0, ctsys0] = cputime;
        tic;
        __dummy_mark_1__ ();
        if is_script
          fn ();
        else
          fn (arg);
        end
        __dummy_mark_1__ ();
        __dummy_mark_2__ ();
        [cttot1, ctuser1, ctsys1] = cputime;
        ctwall = toc;
      catch
        warning ("Could not run %s due to '%s', skipping ...", e.msg, name)
        continue;
      end

      printf ("                %-16s %-16s %-16s %-16s %-16s\n", "t tic","t cpu", "t usr" , "t sys", "n");
      printf ("    Runtime:    %-16g %-16g %-16g %-16g %-16g\n", ctwall, cttot1 - cttot0, ctuser1 - ctuser0, ctsys1 - ctsys0, n);
      printf ("    Compiletime %-16g %-16g %-16g %-16g\n\n", cctwall, ccttot1 - ccttot0, cctuser1 - cctuser0, cctsys1 - cctsys0);

      if calibrate
        t_target = 1;
        e = ctwall - t_target;
        if e > 0.5
          e = 0.5;
        elseif e < -0.5
          e = -0.5;
        end

        n_norm_new = n_norm * (1 - e);
        if j > 30
          n_norm = 0.998 * n_norm + 0.002 * n_norm_new;
        elseif j > 20
          n_norm = 0.99 * n_norm + 0.01 * n_norm_new;
        elseif j > 10
          n_norm = 0.95 * n_norm + 0.05 * n_norm_new;
        else
          n_norm = n_norm_new;
        end

        printf ("    n = %g, e = %g, e_i = %g\n", n_norm, e, e_i);
      end
    end

    if calibrate
      printf ("    Calibrated n: %d\n\n", n);
      cal_res{end + 1} = {name, n};
    end
  end

  if calibrate
    printf ("Calibrated n:s for 1s\n\n");
    for e = cal_res
      printf ("%s %d\n", e{1}{1}, e{1}{2});
    end
  end
end
