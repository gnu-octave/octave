## -*- texinfo -*-
## @deftypefn{Function File} isplaying (@var{playerObj})
## Returns 1 if the audioplayer object is currently playing back audio.
## Returns 0 otherwise.
## @end deftypefn

function result = isplaying(player)
    result = __player_isplaying__(struct(player).player);
endfunction