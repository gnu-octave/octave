1;

function [ret, files] = testls (input)

  ## flag a1 global so as to test the storage of global flags
  global a1;

  ## Input or output, so as to be able to exchange between versions
  if (nargin < 1)
    input = 0;
  endif

  ## Force the random seed to be the same
  rand("seed",1);

  ## Setup some variable to be saved or compared to loaded variables

  ## scalar
  a1 = 1;
  ## matrix
  a2 = hilb(3);
  ## complex scalar
  a3 = 1 + 1i;
  ## complex matrix
  a4 = hilb(3) + 1i*hilb(3);
  ## bool
  a5 = (1 == 1);
  ## bool matrix
  a6 = ([ones(1,5), zeros(1,5)] == ones(1,10));
  ## range
  a7 = 1:10;
  ## structure
  a8.a = a1;
  a8.b = a3;
  ## cell array
  a9{1} = a1;
  a9{2} = a3; 
  ## string
  a10 = ["test"; "strings"];
  ## int8 array
  a11 = int8(floor(256*rand(2,2)));
  ## int16 array
  a12 = int16(floor(65536*rand(2,2)));
  ## int32 array
  a13 = int32(floor(1e6*rand(2,2)));
  ## int64 array
  a14 = int64(floor(10*rand(2,2)));
  ## uint8 array
  a15 = uint8(floor(256*rand(2,2)));
  ## uint16 array
  a16 = uint16(floor(65536*rand(2,2)));
  ## int32 array
  a17 = uint32(floor(1e6*rand(2,2)));
  ## uint64 array
  a18 = uint64(floor(10*rand(2,2)));
  ## sparse
  a19 = sprandn(100,100,0.01);
  ## complex sparse
  a20 = sprandn(100,100,0.01) + 1i * sprandn(100,100,0.01);

  ret = 0;

  files = {"ascii.mat", "binary.mat", "mat5.mat", "mat7.mat"};
  opts = {"-z -text", "-z -binary", "-z -mat", "-v7"};

  vars = "a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20";
  if (! input)
    for i = 1:length(files)
      eval (["save " opts{i} " " files{i} " " vars]);
    endfor
  else
    b1 = a1; b2 = a2; b3 = a3; b4 = a4; b5 = a5;
    b6 = a6; b7 = a7; b8 = a8; b9 = a9;
    b10 = a10; b11 = a11; b12 = a12; b13 = a13; b14 = a14; b15 = a15;
    b16 = a16; b17 = a17; b18 = a18; b19 = a19; b20 = a20;

    for f = files

      clear a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a19 a20;

      file = f{1};

      eval(["load -force " file]);

      if (a1 != b1)
	error(["failed: " file " scalar"])
      endif
      if (a2 != b2)
	error(["failed: " file " matrix"]);
      endif
      if (a3 != b3)
	error(["failed: " file " complex scalar"]);
      endif
      if (a4 != b4)
	error(["failed: " file " complex matrix"]);
      endif
      if (a5 != b5)
	error(["failed: " file " boolean"]);
      endif
      if (!strcmp (file,"mat5") && !strcmp(file,"mat7"))
        if (a6 != b6)
	  error(["failed: " file " boolean matrix"]);
        endif
      endif
      if (a7 != b7)
	error(["failed: " file " range"]);
      endif

      ## != not implemented for structs!!!!
      if (!isstruct(a8) || (length(fieldnames(a8)) != 2) || !isfield(a8,"a")
	  || !isfield(a8,"b") || (a8.a != b8.a) || (a8.b != b8.b))
	error(["failed: " file " struct"]);
      endif
      
      ## != not implemented for cell arrays!!!!
      if (!iscell(a9) || (size(a9) != size(b9)) || (a9{1} != b9{1})
	  || (a9{2} != b9{2}))
	error(["failed: " file " cell"]);
      endif
      
      if (a10 != b10)
	error(["failed: " file " string"]);
      endif

      if (a11 != b11)
	error(["failed: " file " int8"]);
      endif

      if (a12 != b12)
	error(["failed: " file " int16"]);
      endif

      if (a13 != b13)
	error(["failed: " file " int32"]);
      endif

      if (a14 != b14)
	error(["failed: " file " int64"]);
      endif

      if (a15 != b15)
	error(["failed: " file " uint8"]);
      endif

      if (a16 != b16)
	error(["failed: " file " uint16"]);
      endif

      if (a17 != b17)
	error(["failed: " file " uint32"]);
      endif

      if (a18 != b18)
	error(["failed: " file " uint64"]);
      endif

      if (a19 != b19)
	error(["failed: " file " sparse"]);
      endif

      if (a20 != b20)
	error(["failed: " file " complex sparse"]);
      endif

      ## Test for global flags
      if (!isglobal("a1") || isglobal("a2") || isglobal("a3") || 
	  isglobal("a4") || isglobal("a5") || isglobal("a6") || 
	  isglobal("a7") || isglobal("a8") || isglobal("a9") || 
	  isglobal("a10") || isglobal("a11") || isglobal("a12") ||
	  isglobal("a13") || isglobal("a14") || isglobal("a15") ||
	  isglobal("a16") || isglobal("a17") || isglobal("a18") ||
	  isglobal("a19") || isglobal("a20"))
	error (["failed: " file " global test"]); 
      endif
    endfor
  endif

  ret = 1;

endfunction

[save_status, save_files] = testls (0);
[load_status, load_files] = testls (1);

for f = [save_files, load_files]
  unlink (f{1});
endfor

save_status && load_status
