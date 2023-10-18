function bench_valgrind (octbin_path, factor, benchname_filter, octbin_path_ref, logdir)

  s = unix ("valgrind --version");
  if s
    error ("Valgrind probably not installed");
  end

  orig_dir = pwd;

  path_to_bench_folder = which ("bench_valgrind");
  path_to_bench_folder = strrep (path_to_bench_folder, "bench_valgrind.m", "");
  logsubfolder_name = ["run_" datestr(now, "yyyy_mm_dd_HH_MM_ss")];

  tests = get_bench_conf ();

  result = {};

  unwind_protect
    mkdir (logdir);
    cd (logdir);
    mkdir (logsubfolder_name)
    cd (logsubfolder_name)

    for i = 1:length(tests)

      test = tests{i};
      name = test{1};
      complexity = test{3};
      also_compile = test{4};
      is_script = test{5};

      % Skip on not matching regex, if there is one
      if length (benchname_filter) && isempty (regexp (name, benchname_filter))
        continue;
      end

      % logfilename, octbin, bench folder, benchname filter
      cmd_template = ["valgrind --tool=callgrind  --callgrind-out-file=%s  --separate-recs=10 " ...
                      "--dump-instr=yes --collect-jumps=yes \"--dump-after=dummy_mark_1\" " ...
                      " %s -W --eval \"__vm_enable__ (1); cd %s; bench('reg','%s','n_factor', %d);exit(0)\""];

      logfilename1 = ["callgrind.out.log_mark_", name, "_", num2str(i), "_", name, ".log"];
      logfilename2 = ["callgrind.out.log_ref1_", name, "_", num2str(i), "_", name, ".log"];

      cmd_1 = sprintf (cmd_template, logfilename1, octbin_path, path_to_bench_folder, name, factor);
      cmd_2 = sprintf (cmd_template, logfilename2, octbin_path_ref, path_to_bench_folder, name, factor);

      [status1, stdout1] = unix (cmd_1);
      [status2, stdout2] = unix (cmd_2);

      log1 = fileread ([logfilename1 ".2"]);
      log2 = fileread ([logfilename2 ".2"]);

      [~,~,~,~, cost1] = regexp (log1, "summary:\\s+(\\d+)", 'once');
      cost1 = str2num (cost1{1});
      [~,~,~,~, cost2] = regexp (log2, "summary:\\s+(\\d+)", 'once');
      cost2 = str2num (cost2{1});

      result{end + 1} = {name, cost1, cost2, cost2/cost1*100};
    endfor

    summary = sprintf ("%30s %15s %15s %15s\n", "name", "mark", "ref", "ref/mark%");
    for i = 1:length (result)
      run_result = result{i};
      summary = [summary, sprintf("%30s %15d %15d %15.7f\n", run_result{:})];
    endfor

    printf ("\n\n\n%s", summary);
    % Save summary to file
    f = fopen ("summary.log", "w");
    fprintf (f, "%s", summary);
    fclose (f);

  unwind_protect_cleanup
    cd (orig_dir);
  end
end
