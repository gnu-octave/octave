type_list = ["char"; "char*1"; "integer*1"; "int8"; 
	     "schar"; "signed char"; "uchar"; "unsigned char";
	     "short"; "ushort"; "unsigned short"; "int";
	     "uint"; "unsigned int"; "long"; "ulong"; "unsigned long";
	     "float"; "float32"; "real*4"; "double"; "float64";
	     "real*8"; "int16"; "integer*2"; "int32"; "integer*4"];

n = rows (type_list);

nm = tmpnam ();

id = fopen (nm, "wb");

if (id > 0)

  for i = 1:n
    fwrite (id, i, deblank (type_list(i,:)));
  endfor

  fclose (id);

  id = fopen (nm, "rb");

  if (id > 0)

    x = zeros (1, n);

    for i = 1:n
      x(i) = fread (id, [1, 1], deblank (type_list(i,:)));
    endfor

    if (x == 1:n)
      printf ("ok\n");
    endif

  endif

endif

unlink (nm);
