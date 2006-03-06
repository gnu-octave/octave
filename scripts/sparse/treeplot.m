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
    error ("treeplot: wrong number of input/output arguments");
  else
    if (!ismatrix(Tree) || size(Tree)(1) != 1 || !isnumeric(Tree) 
        || !isvector(Tree) || any(Tree>length (Tree)))
      error ("treeplot: the first input argument must be a vector of predecessors");
    else	
    
      NodeStyle = "0*;;";                  ## the inicialization of node end edge style
      EdgeStyle = "1;;";      
      if (nargin > 2)
        EdgeStyle = EdgeS;
        if (nargin > 1) 
	  if ((length(findstr(NodeS,"*")) == 0) 
	    && (length(findstr(NodeS,"+")) == 0) 
	    && (length(findstr(NodeS,"x")) == 0) )
	    NodeStyle = [NodeS,"o"];
	  else
	    NodeStyle = NodeS;
	  endif
        endif
      endif
      
      Tree = Tree(:)';		            ## make it a row vector
      NodNumber = length (Tree);            ## the count of nodes of the graph
      ChildNumber = zeros (1,NodNumber+1);  ## the number of childrens
      
      for i = 1:(NodNumber)       ## VecOfChild is helping vector which is used to speed up the
                                  ##   choose of descendant nodes

        ChildNumber(Tree(i)+1) = ChildNumber(Tree(i)+1) + 1;
      endfor
      Pos = 1;
      for i = 1:(NodNumber+1)
        Start(i) = Pos;
	Help(i) = Pos;
	Pos = Pos + ChildNumber(i);
	Stop(i) = Pos;
      endfor
      for i = 1:NodNumber        
        VecOfChild(Help(Tree(i)+1)) = i;  
	Help(Tree(i)+1)=Help(Tree(i)+1)+1;
      endfor                    ## VecOfChild is helping vector which is used to speed up the
                                ##   choose of descendant nodes
				
      ParNumber = 0;            ## the number of "parent" (actual) node (it's descendants will 
                                ##   be browse in the next iteration)
      LeftMost = 0;		## the x-coordinate of the left most descendant of "parent node"
                                ##   this value is increased in each leaf		
      Level = NodNumber;        ## the level of "parent" node (root level is NodNumber)
      Max = NodNumber;          ## NodNumber - Max is the height of this graph
      St = [-1,0];		## main stack - each item consists of two numbers - the number of node and
                                ##   the number it's of parent node
				##   on the top of stack there is "parent node"
      Skelet = 0;		## stack which is use to draw the graph edge (it 
                                ##   have to be uninterupted line)
      while (ParNumber!=-1)     ## the top of the stack
	if (Start(ParNumber+1) < Stop(ParNumber+1))
	  idx = VecOfChild(Start(ParNumber+1):Stop(ParNumber+1)-1);
	else
	  idx = zeros(1,0);
	endif                   ## add to idx the vector of parent descendants
	St = [St ; [idx', ones(fliplr(size(idx)))*ParNumber]];
	                        ## add to stack the records relevant to parent descandant s
	if (ParNumber != 0)
	  Skelet = [Skelet ; ([ones(size(idx))*ParNumber; idx])(:)];
	endif
	if (St(end,2) != ParNumber)  ## if there is not any descendant of "parent node":
	  LeftMost = LeftMost + 1;
          XCoordinateR(ParNumber) = LeftMost;           
	  Max = min (Max, Level);
          if ((length(St)>1) && (find((shift(St,1)-St) == 0) >1) && St(end,2) != St(end-1,2))
	                             ## return to the nearest branching
            Position = (find((shift(St(:,2),1)-St(:,2)) == 0))(end)+1;      ## the position to return
                          ## position is the position on the stack, where should be started 
			  ## further search (there are two nodes which has the same parent node)
            ParNumberVec = St(Position:end,2);
                          ## the vector of removed nodes (the content of stack form position to end)
            Skelet = [Skelet; flipud(ParNumberVec)];
            Level = Level + length(ParNumberVec);
	                  ## the level have to be decreased
            XCoordinateR(ParNumberVec) = LeftMost;
            St(Position:end,:) = [];
          endif	
	  St(end,:) = [];       	## remove the next node from "searched branch"
          ParNumber = St(end,1);	## choose new "parent node"
	  if (ParNumber != -1)		## if there is another branch start to search it
	    Skelet = [Skelet ; St(end,2); ParNumber];
            YCoordinate(ParNumber) = Level;	
	    XCoordinateL(ParNumber) = LeftMost + 1;
	  endif
	else                      ## there were descendants of "parent nod"
          Level = Level - 1;      ## choose the last of them and go on through it
	  ParNumber = St(end,1);
	  YCoordinate(ParNumber) = Level;     
	  XCoordinateL(ParNumber) = LeftMost+1;
	endif
      endwhile

      XCoordinate = (XCoordinateL + XCoordinateR) / 2;     ## calculate the x coordinates 
               ## (the known values are the position of most left and most right descendants)

      axis ([0.5 LeftMost+0.5 Max-0.5 NodNumber-0.5], "nolabel");  ## set axis and graph size 
      
      plot (XCoordinate,YCoordinate,NodeStyle);           ## plot grah nodes 
      hold ("on");
      
      Skelet = [Skelet; 0];     ## helping command - usable for plotting edges
      
      idx = find (Skelet == 0);                           ## draw graph edges 
       
      for i = 2:length(idx)                               ## plot each tree component in one loop
	istart = idx(i-1) + 1;                            ## tree component start
	istop = idx(i) - 1;                               ## tree component end
	if (istop - istart < 1)                          
	  continue;
	endif
	plot (XCoordinate(Skelet(istart:istop)),
	      YCoordinate(Skelet(istart:istop)), EdgeStyle)
      endfor
      
      hold ("off");
      
    endif
  endif
  St;
  Skelet;
endfunction

%!demo
%! % Plot a simple tree plot 
%! treeplot([2 4 2 0 6 4 6])

%!demo
%! % Plot a simple tree plot defining the edge and node styles
%! treeplot([2 4 2 0 6 4 6], "b+", "g") 
