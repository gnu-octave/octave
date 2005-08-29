arch_list = ["native"; "ieee-le"; "ieee-be"; "vaxd"; "vaxg"; "cray"];

status = 1;

for i = 1:6
  arch = deblank (arch_list (i,:));
  for j = 1:6
    if (j == 1)
      mode_list = ["w"; "r"; "a"];
    elseif (j == 2)
      mode_list = ["w+"; "r+"; "a+"];
    endif
    nm = tmpnam ();
    for k = 1:3
      mode = deblank (mode_list (k,:))
      [id, err] = fopen (nm, mode, arch);
      if (id < 0)
	printf ("open failed: %s (%s, %s): %s\n", nm, mode, arch, err);
      	status = 0;
	break;
      else
	fclose (id);
      endif
      tmp_mode = strcat (mode, "b");
      [id, err] = fopen (nm, tmp_mode, arch);
      if (id < 0)
	printf ("open failed: %s (%s, %s): %s\n", nm, tmp_mode, arch, err);
      	status = 0;
	break;
      else
	fclose (id);
      endif
      tmp_mode = strcat (mode, "t");
      [id, err] = fopen (nm, tmp_mode, arch);
      if (id < 0)
	printf ("open failed: %s (%s, %s): %s\n", nm, tmp_mode, arch, err);
      	status = 0;
	break;
      else
	fclose (id);
      endif
    endfor
    if (status == 0)
      break;
    endif
  endfor
  if (status == 0)
    break;
  endif
endfor

status == 1
