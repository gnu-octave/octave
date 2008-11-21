## Copyright (C) 2008 Ivana Varekova & Radek Salac
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} treelayout (@var{Tree})
## @deftypefnx {Function File} {} treelayout (@var{Tree}, @var{Permutation})
## treelayout lays out a tree or a forest. The first argument @var{Tree} is a vector of
## predecessors, optional parameter @var{Permutation} is an optional postorder permutation.
## The complexity of the algorithm is O(n) in
## terms of time and memory requirements.
## @seealso{etreeplot, gplot,treeplot}
## @end deftypefn

function [XCoordinate, YCoordinate, Height, s] = treelayout (Tree, Permutation)
  if (nargin < 1 || nargin > 2 || nargout > 4)
    print_usage ();
  elseif (! isvector (Tree) || rows (Tree) != 1 || ! isnumeric (Tree) 
        ||  any (Tree > length (Tree)) || any (Tree < 0) )
    error ("treelayout: the first input argument must be a vector of predecessors");
  else
    ## make it a row vector
    Tree = Tree(:)';

    ## the count of nodes of the graph
    NodNumber = length (Tree);
    ## the number of children
    ChildNumber = zeros (1, NodNumber + 1);


    ## checking vector of predecessors
    for i = 1 : NodNumber
      if (Tree (i) < i)
	## this part of graph was checked before
        continue;
      endif

      ## Try to find cicle in this part of graph
      ## we use modified Floyd's cycle-finding algorithm
      tortoise = Tree (i);
      hare = Tree (tortoise);

      while (tortoise != hare)
	## we end after find a cicle or when we reach a checked part of graph

        if (hare < i)
          ## this part of graph was checked before
          break
        endif

        tortoise = Tree (tortoise);
	## hare will move faster than tortoise so in cicle hare
	## must reach tortoise
        hare = Tree (Tree (hare));

      endwhile

      if (tortoise == hare)
	## if hare reach tortoise we find cicle
        error ("treelayout: vector of predecessors has bad format");
      endif

    endfor
    ## vector of predecessors has right format

    for i = 1:NodNumber
      ## VecOfChild is helping vector which is used to speed up the
      ## choose of descendant nodes

      ChildNumber (Tree (i) + 1) = ChildNumber (Tree (i) + 1) + 1;
    endfor

    Pos = 1;
    for i = 1 : NodNumber + 1
      Start (i) = Pos;
      Help (i) = Pos;
      Pos += ChildNumber (i);
      Stop (i) = Pos;
    endfor

    if (nargin == 1)
      for i = 1 : NodNumber
        VecOfChild (Help (Tree (i) + 1)) = i;  
        Help (Tree (i) + 1) = Help (Tree (i) + 1) + 1;
      endfor
    else
      VecOfChild = Permutation;
    endif


    ## the number of "parent" (actual) node (it's descendants will be
    ## browse in the next iteration)
    ParNumber = 0;

    ## the x-coordinate of the left most descendant of "parent node"
    ## this value is increased in each leaf		
    LeftMost = 0;

    ## the level of "parent" node (root level is NodNumber)
    Level = NodNumber;

    ## NodNumber - Max is the height of this graph
    Max = NodNumber;

    ## main stack - each item consists of two numbers - the number of
    ## node and the number it's of parent node on the top of stack
    ## there is "parent node"
    St = [-1, 0];

    #number of vertices s in the top-level separator
    s = 0;
    # flag which says if we are in top level separator
    topLevel = 1;
    ## the top of the stack
    while (ParNumber != -1)
      if (Start(ParNumber + 1) < Stop(ParNumber + 1))
        idx = VecOfChild (Start (ParNumber + 1) : Stop (ParNumber + 1) - 1);
      else
        idx = zeros (1, 0);
      endif

      ## add to idx the vector of parent descendants
      St = [St ; [idx', ones(fliplr(size(idx))) * ParNumber]];

      # we are in top level separator when we have one children
      ## and the flag is 1
      if (columns(idx) == 1 && topLevel ==1 )
        s += 1;
      else
        # we arent in top level separator now
        topLevel = 0;
      endif
      ## if there is not any descendant of "parent node":
      if (St(end,2) != ParNumber)
       LeftMost = LeftMost + 1;
       XCoordinateR(ParNumber) = LeftMost;           
       Max = min (Max, Level);
       if ((length(St) > 1) && (find((shift(St,1)-St) == 0) >1) 
	   && St(end,2) != St(end-1,2))
	  ## return to the nearest branching the position to return
	  ## position is the position on the stack, where should be
          ## started further search (there are two nodes which has the
          ## same parent node)

          Position = (find ((shift (St(:, 2), 1) - St(:, 2)) == 0))(end)+1;
          ParNumberVec = St(Position : end, 2);

          ## the vector of removed nodes (the content of stack form
          ## position to end)

          Level = Level + length(ParNumberVec);

	  ## the level have to be decreased

          XCoordinateR(ParNumberVec) = LeftMost;
          St(Position:end, :) = [];
        endif	

        ## remove the next node from "searched branch"

        St(end, :) = [];
	## choose new "parent node"
        ParNumber = St(end, 1);
	## if there is another branch start to search it
	if (ParNumber != -1)
          YCoordinate(ParNumber) = Level;	
          XCoordinateL(ParNumber) = LeftMost + 1;
	endif
      else

        ## there were descendants of "parent nod" choose the last of
        ## them and go on through it
        Level--;
        ParNumber = St(end, 1);
        YCoordinate(ParNumber) = Level;     
        XCoordinateL(ParNumber) = LeftMost+1;
      endif
    endwhile

    ## calculate the x coordinates (the known values are the position
    ## of most left and most right descendants)
    XCoordinate = (XCoordinateL + XCoordinateR) / 2;

    Height = NodNumber - Max - 1;
  endif
endfunction

%!demo
%! % Compute a simple tree layout 
%! [x,y,h,s]=treelayout([0 1 2 2])

%!demo
%! % Compute a simple tree layout with defined postorder permutation
%! [x,y,h,s]=treelayout([0 1 2 2],[1 2 3 4]) 
