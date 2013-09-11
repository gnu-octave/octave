## -*- texinfo -*-
## @deftypefn{Function File} stop (@var{playerObj})
## Stop the playback and reset the relevant variables to their starting values.
## @end deftypefn

function stop(player)
    __player_stop__(struct(player).player);
endfunction