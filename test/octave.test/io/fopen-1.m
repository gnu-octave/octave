arch_list = ["native"; "ieee-le"; "ieee-be"; "vaxd"; "vaxg"; "cray"];

status = 1;

for i = 1:6
  arch = deblank (arch_list (i,:))
  for j = 1:2
    if (j == 1)
      mode_list = ["w"; "r"; "a"];
    else
      mode_list = ["w+"; "r+"; "a+"];
    endif
    nm = tmpnam ();
    for k = 1:3
      mode = deblank (mode_list (k,:));
      if ((id = fopen (nm, mode, arch)) < 0)
      	status = 0; break;
      endif
      fclose (id);
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
