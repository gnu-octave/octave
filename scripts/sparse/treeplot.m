## Copyright (C) 2005 Ivana Varekova
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301  USA

## -*- texinfo -*-
## @deftypefn {Function File} {} treeplot (@var{Tree})
## @deftypefnx {Function File} {} treeplot (@var{Tree}, @var{LineStyle}, @var{EdgeStyle})
## Produces a graph of tree or forest. The first argument is vector of
## predecessors, optional parametres @var{LineStyle} and @var{EdgeStyle}
## define the output style. The complexity of the algorithm is O(n) in
## terms of is time and memory requirements.
## @seealso{etreeplot, gplot}
## @end deftypefn

function treeplot (Tree, NodeS, EdgeS)

  if (nargin < 1 || nargin > 3 || nargout > 0)
    print_usage ();
  else
    if (! ismatrix (Tree) || rows (Tree) != 1 || ! isnumeric (Tree) 
        || ! isvector (Tree) || any (Tree > length (Tree)))
      error ("treeplot: the first input argument must be a vector of predecessors");
    else
      ## the inicialization of node end edge style
      NodeStyle = "k*";
      EdgeStyle = "r";      
      if (nargin > 2)
        EdgeStyle = EdgeS;
        if (nargin > 1) 
	  if (length (findstr (NodeS, "*")) == 0
	      && length (findstr (NodeS, "+")) == 0
	      && length (findstr (NodeS, "x")) == 0)
	    NodeStyle = [NodeS, "o"];
	  else
	    NodeStyle = NodeS;
	  endif
        endif
      endif

      Tree = Tree(:)';		            ## make it a row vector
      NodNumber = length (Tree);            ## the count of nodes of the graph
      ChildNumber = zeros (1, NodNumber+1); ## the number of childrens
      
      for i = 1:NodNumber
        ## VecOfChild is helping vector which is used to speed up the
        ## choose of descendant nodes

        ChildNumber(Tree(i)+1) = ChildNumber(Tree(i)+1) + 1;
      endfor
      Pos = 1;
      for i = 1:NodNumber+1
        Start(i) = Pos;
	Help(i) = Pos;
	Pos += ChildNumber(i);
	Stop(i) = Pos;
      endfor
      for i = 1:NodNumber        
        VecOfChild(Help(Tree(i)+1)) = i;  
	Help(Tree(i)+1) = Help(Tree(i)+1)+1;
      endfor

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
      St = [-1,0];

      ## stack which is use to draw the graph edge (it have to be
      ## uninterupted line)
      Skelet = 0;

      ## the top of the stack
      while (ParNumber != -1)
	if (Start(ParNumber+1) < Stop(ParNumber+1))
	  idx = VecOfChild(Start(ParNumber+1):Stop(ParNumber+1)-1);
	else
	  idx = zeros (1, 0);
	endif
        ## add to idx the vector of parent descendants
	St = [St ; [idx', ones(fliplr(size(idx)))*ParNumber]];
	## add to stack the records relevant to parent descandant s
	if (ParNumber != 0)
	  Skelet = [Skelet; ([ones(size(idx))*ParNumber; idx])(:)];
	endif

	## if there is not any descendant of "parent node":
	if (St(end,2) != ParNumber)
	  LeftMost = LeftMost + 1;
          XCoordinateR(ParNumber) = LeftMost;           
	  Max = min (Max, Level);
          if ((length(St)>1) && (find((shift(St,1)-St) == 0) >1)
	      && St(end,2) != St(end-1,2))
	    ## return to the nearest branching the position to return
	    ## position is the position on the stack, where should be
	    ## started further search (there are two nodes which has the
	    ## same parent node)
            Position = (find((shift(St(:,2),1)-St(:,2)) == 0))(end)+1;
            ParNumberVec = St(Position:end,2);
            ## the vector of removed nodes (the content of stack form
	    ## position to end)
            Skelet = [Skelet; flipud(ParNumberVec)];
            Level = Level + length(ParNumberVec);
	    ## the level have to be decreased
            XCoordinateR(ParNumberVec) = LeftMost;
            St(Position:end,:) = [];
          endif	
       	  ## remove the next node from "searched branch"
	  St(end,:) = [];
	  ## choose new "parent node"
          ParNumber = St(end,1);
	  ## if there is another branch start to search it
	  if (ParNumber != -1)
	    Skelet = [Skelet ; St(end,2); ParNumber];
            YCoordinate(ParNumber) = Level;	
	    XCoordinateL(ParNumber) = LeftMost + 1;
	  endif
	else
          ## there were descendants of "parent nod" choose the last of
	  ## them and go on through it
          Level--;
	  ParNumber = St(end,1);
	  YCoordinate(ParNumber) = Level;     
	  XCoordinateL(ParNumber) = LeftMost+1;
	endif
      endwhile

      ## calculate the x coordinates (the known values are the position
      ## of most left and most right descendants)
      XCoordinate = (XCoordinateL + XCoordinateR) / 2;

      hold ("on");

      ## plot grah nodes 
      plot (XCoordinate,YCoordinate,NodeStyle);
      
      ## helping command - usable for plotting edges
      Skelet = [Skelet; 0];
      
      ## draw graph edges 
      idx = find (Skelet == 0);
       
      ## plot each tree component in one loop
      for i = 2:length(idx)
        ## tree component start
	istart = idx(i-1) + 1;
        ## tree component end
	istop = idx(i) - 1;
	if (istop - istart < 1)                          
	  continue;
	endif
	plot (XCoordinate(Skelet(istart:istop)),
	      YCoordinate(Skelet(istart:istop)), EdgeStyle)
      endfor
      
      ## set axis and graph size 
      axis ([0.5, LeftMost+0.5, Max-0.5, NodNumber-0.5], "nolabel");
      
      hold ("off");
      
    endif
  endif
endfunction

%!demo
%! % Plot a simple tree plot 
%! treeplot([2 4 2 0 6 4 6])

%!demo
%! % Plot a simple tree plot defining the edge and node styles
%! treeplot([2 4 2 0 6 4 6], "b+", "g") 
