function bench (varargin)

  % The tests to run
  % 
  % {name, {arg_type, n}, ...}
  tests = {
    {"for_loop_empty", {"n", 206824596}, 1, {}},
    {"for_loop_silly", {"n", 34894840}, 1, {}},
    {"for_loop_binop_1", {"n", 20300088}, 1, {}},
    {"for_loop_sinpi", {"n", 12991066}, 1, {}},
    {"for_loop_ifs", {"n", 5874007}, 1, {}},
    {"while_loop_empty", {"n", 24237997}, 1, {}},
    {"do_until_loop_empty", {"n", 27109647}, 1, {}},
    {"for_loop_subfun_1", {"n", 11930390}, 1, {}},
    {"for_loop_matselfmul", {"rand sq",150}, 3, {}},
    {"for_sum_1", {"rand rowvec", 19267692}, 1, {}},
    {"for_sum_2", {"rand rowvec", 8742659}, 1, {}},
    {"qsort_recursive", {"rand rowvec", 107851}, 1, {}}, % Mostly copies vectors around
    {"qsort_iterative", {"rand rowvec", 344418}, 1, {}},
    {"for_loop_fncall", {"n", 2164885}, 1, {}},
    {"bench_median", {"rand rowvec", 1927}, 1, {}},
    {"bench_cov", {"rand rowvec", 15261}, 1, {}},
    {"str_mod", {"n", 2335290}, 1, {}},
  };

  reg = '';
  calibrate = 0;
  do_both = 1;
  n_factor = 1;
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
    end
  end

  % For compatibility with older releases and Matlab
  if ~exist("__compile")
    __compile = @(varargin) true;
  end
  if ~exist("__dummy_mark_1")
    __dummy_mark_1 = @() true;
  end
  if ~exist("__dummy_mark_2")
    __dummy_mark_2 = @() true;
  end

  cal_res = {};

  for i = 1:length(tests)

    test = tests{i};
    name = test{1};
    complexity = test{3};
    also_compile = test{4};
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
      rng (0); % Reset rng
      n = round (conf{2} * n_factor);
      arg = randn (n);
    elseif strcmp (conf_type, "rand rowvec")
      rng (0); % Reset rng
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
        rng (0); % Reset rng
        n = round (n_norm * n_factor);
        arg = randn (n);
      elseif strcmp (conf_type, "rand rowvec")
        rng (0); % Reset rng
        n = round (n_norm * n_factor);
        arg = randn (n, 1);
      end
      n = round (n);


      tic;
      [ccttot0, cctuser0, cctsys0] = cputime;
      assert (__compile (name));
      [ccttot1, cctuser1, cctsys1] = cputime;
      cctwall = toc;

      [cttot0, ctuser0, ctsys0] = cputime;
      tic;
      __dummy_mark_1 ();
      fn (arg);
      __dummy_mark_2 ();
      [cttot1, ctuser1, ctsys1] = cputime;
      ctwall = toc;

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
