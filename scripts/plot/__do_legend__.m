function __do_legend__ ()

  __plot_globals__;

  cf = __current_figure__;
  mxi = __multiplot_xi__(cf);
  myi = __multiplot_yi__(cf);

  props = __plot_key_properties__{cf}{mxi,myi};

  if (isstruct (props))
    if (isfield (props, "visible"))
      visible = props.visible;
    else
      error ("__do_legend__: missing field \"visible\"");
    endif
    if (isfield (props, "box"))
      box = props.box;
    else
      error ("__do_legend__: missing field \"box\"");
    endif
    if (isfield (props, "position"))
      position = props.position;
    else
      error ("__do_legend__: missing field \"position\"");
    endif
    if (visible)
      switch (position)
	case 1
	  __gnuplot_raw__ ("set  key right top;\n")
	case 2
	  __gnuplot_raw__ ("set  key left top;\n")
	case 3
	  __gnuplot_raw__ ("set  key left bottom;\n")
	case 4
	  __gnuplot_raw__ ("set  key right bottom;\n")
	case -1
	  __gnuplot_raw__ ("set  key right top outside;\n")
	case -2
	  __gnuplot_raw__ ("set  key right bottom outside;\n")
	case -3
	  __gnuplot_raw__ ("set  key below;\n")
      endswitch
      if (box)
        __gnuplot_raw__ ("set key box;\n")
      else
        __gnuplot_raw__ ("set key nobox;\n")
      endif
    else
      __gnuplot_raw__ ("unset key;\n")
    endif
  else
    error ("__do_legend__: expecting properties to be a struct");
  endif

endfunction
