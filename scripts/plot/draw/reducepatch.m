########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{reduced_fv} =} reducepatch (@var{fv})
## @deftypefnx {} {@var{reduced_fv} =} reducepatch (@var{faces}, @var{vertices})
## @deftypefnx {} {@var{reduced_fv} =} reducepatch (@var{patch_handle})
## @deftypefnx {} {} reducepatch (@var{patch_handle})
## @deftypefnx {} {@var{reduced_fv} =} reducepatch (@dots{}, @var{reduction_factor})
## @deftypefnx {} {@var{reduced_fv} =} reducepatch (@dots{}, "fast")
## @deftypefnx {} {@var{reduced_fv} =} reducepatch (@dots{}, "verbose")
## @deftypefnx {} {[@var{reduced_faces}, @var{reduces_vertices}] =} reducepatch (@dots{})
##
## Reduce the number of faces and vertices in a patch object while retaining
## the overall shape of the patch.
##
## The input patch can be represented by a structure @var{fv} with the
## fields @code{faces} and @code{vertices}, by two matrices @var{faces} and
## @var{vertices} (see, e.g., the result of @code{isosurface}), or by a
## handle to a patch object @var{patch_handle}
## (@pxref{XREFpatch,,@code{patch}}).
##
## The number of faces and vertices in the patch is reduced by iteratively
## collapsing the shortest edge of the patch to its midpoint (as discussed,
## e.g., here:
## @url{https://libigl.github.io/libigl/tutorial/tutorial.html#meshdecimation}).
##
## Currently, only patches consisting of triangles are supported.  The
## resulting patch also consists only of triangles.
##
## If @code{reducepatch} is called with a handle to a valid patch
## @var{patch_handle}, and without any output arguments, then the given
## patch is updated immediately.
##
## If the @var{reduction_factor} is omitted, the resulting structure
## @var{reduced_fv} includes approximately 50% of the faces of the original
## patch.  If @var{reduction_factor} is a fraction between 0 (excluded) and 1
## (excluded), a patch with approximately the corresponding fraction of faces
## is determined.
## If @var{reduction_factor} is an integer greater than or equal to 1, the
## resulting patch has approximately @var{reduction_factor} faces.  Depending
## on the geometry of the patch, the resulting number of faces can differ from
## the given value of @var{reduction_factor}.  This is especially true when
## many shared vertices are detected.
##
## For the reduction, it is necessary that vertices of touching faces are
## shared.  Shared vertices are detected automatically.  This detection can be
## skipped by passing the optional string argument @qcode{"fast"}.
##
## With the optional string arguments @qcode{"verbose"}, additional status
## messages are printed to the command window.
##
## Any string input arguments must be passed after all other arguments.
##
## If called with one output argument, the reduced faces and vertices are
## returned in a structure @var{reduced_fv} with the fields @code{faces} and
## @code{vertices} (see the one output option of @code{isosurface}).
##
## If called with two output arguments, the reduced faces and vertices are
## returned in two separate matrices @var{reduced_faces} and
## @var{reduced_vertices}.
##
## @seealso{isosurface, isonormals, reducevolume, patch}
## @end deftypefn

## FIXME: Convert faces to only triangles if necessary

function varargout = reducepatch (varargin)

  if (nargin < 1 || nargin > 5)
    print_usage ();
  endif

  [faces, vertices, max_faces, patch_handle, fast, verbose] = ...
                          __get_check_reducepatch_args__ (varargin{:});

  if (verbose)
    printf (["reducepatch: before reduction of faces:           ", ...
             "%5d faces, %5d vertices\n"], rows (faces), rows (vertices));
  endif

  if (max_faces >= rows (faces))
    faces_reduced = faces;
    vertices_reduced = vertices;
  else
    if (! fast)
      [faces, vertices] = __unite_shared_vertices__ (faces, vertices);

      if (verbose)
        printf (["reducepatch: after detection of shared vertices:  ", ...
                 "%5d faces, %5d vertices\n"], rows (faces), rows (vertices));
      endif
    endif

    [faces_reduced, vertices_reduced] = ...
        __reducepatch__ (vertices, faces, max_faces, verbose);
  endif

  if (verbose)
    printf (["reducepatch: after reduction of faces:            ", ...
             "%5d faces, %5d vertices\n"],                         ...
            rows (faces_reduced), rows (vertices_reduced));
  endif

  ## output
  if (! isempty (patch_handle) && nargout == 0)
    ## update patch object
    set (patch_handle, "Faces", faces_reduced, "Vertices", vertices_reduced);
  elseif (nargout == 2)  # faces, vertices
    varargout{1} = faces_reduced;
    varargout{2} = vertices_reduced;
  else  # fv structure
    varargout{1}.faces = faces_reduced;
    varargout{1}.vertices = vertices_reduced;
  endif

endfunction

## assign input parameters and check their validity
function [faces, vertices, max_faces, patch_handle, fast, verbose] = ...
              __get_check_reducepatch_args__ (varargin)

  ## default values
  faces = vertices = max_faces = patch_handle = [];
  reduction_factor = 0.5;
  fast = verbose = false;

  num_string_inputs = 0;
  ## check whether last 2 input arguments are strings and assign parameters
  valid_vals = nargin:-1:nargin-1;
  valid_vals(valid_vals < 1) = [];
  for arg = varargin(valid_vals)
    if (! ischar (arg{1}))
      break;  # no string arguments at end, exit checking
    endif
    switch (tolower (arg{1}))
      case {"f", "fast"}
        fast = true;
        num_string_inputs += 1;

      case {"v", "verbose"}
        verbose = true;
        num_string_inputs += 1;

      case ""
        num_string_inputs++;
        ## silently ignore empty strings

      otherwise
        error ("reducepatch: parameter '%s' not supported", arg{1});

    endswitch
  endfor

  ## get faces and vertices
  i_fv = 1;
  arg1 = varargin{1};
  if (isstruct (arg1))
    if (all (isfield (arg1, {"vertices", "faces"})))
      vertices = arg1.vertices;
      faces = arg1.faces;
    else
      error (["reducepatch: struct FV must contain the fields ", ...
              "'vertices' and 'faces'."]);
    endif
  elseif (isscalar (arg1))
    patch_handle = arg1;
    if (isgraphics (patch_handle, "patch"))
      vertices = get (patch_handle, "Vertices");
      faces = get (patch_handle, "Faces");
    else
      error ("reducepatch: PATCH_HANDLE must be a valid handle to a patch");
    endif
  elseif (ismatrix (arg1))
    faces = arg1;
    if (nargin - num_string_inputs > 1 && ismatrix (varargin{2}))
      vertices = varargin{2};
      i_fv = 2;
    else
      error (["reducepatch: If first argument is a matrix containing ", ...
             "FACES, second argument must be a matrix containing VERTICES"]);
    endif
  else
    print_usage ("reducepatch");
  endif

  ## get reduction_factor
  if (nargin - num_string_inputs > i_fv)
    reduction_factor = varargin{i_fv + 1};
    if (! isscalar (reduction_factor) || reduction_factor <= 0)
      error ("reducepatch: REDUCTION_FACTOR must be a positive scalar");
    endif
  endif

  ## check faces and vertices
  if (columns (vertices) != 3)
    error ("reducepatch: VERTICES must be an Mx3 matrix");
  endif
  if (columns (faces) != 3)
    ## FIXME: Convert faces to only triangles if necessary
    error ("reducepatch: Currently patches must consist of triangles only");
  endif
  if (any (mod (faces(:), 1)) || any (faces(:) < 1))
    error ("reducepatch: FACES must consist of positive integer indices only");
  endif
  if (max (faces(:)) > rows (vertices))
    error ("reducepatch: not enough VERTICES for given FACES");
  endif

  ## get max_faces
  num_faces = rows (faces);
  if (reduction_factor < 1)
    max_faces = floor (num_faces * reduction_factor);
  else
    max_faces = reduction_factor;
  endif

endfunction

## actual function to reduce number of faces
function [faces_reduced, vertices_reduced] = ...
              __reducepatch__ (vertices, faces, max_faces, verbose)

  num_faces = rows (faces);
  faces = sort (faces, 2);

  ## get all unique edges
  all_edges = sort ([faces(:,1) faces(:,2);
                     faces(:,2) faces(:,3);
                     faces(:,3) faces(:,1)], 2);
  edges = unique (all_edges, "rows");
  num_edges = rows (edges);

  ## calculate length of edges
  edge_length = norm (vertices(edges(:,1),:) - vertices(edges(:,2),:), ...
                      2, "rows");

  if (verbose)
    printf ("reducepatch: reducing number of faces");
    ## do not spam the command window with dots if many faces are collapsed
    verbose_stepwidth = (num_faces - max_faces) / 100;  # max. 100 dots
    verbose_counter = 0;
  endif

  ## reduce number of faces
  clean_finish = true;
  do
    ## find shortest edge
    i_shortest_edge = find (edge_length == min (edge_length), 1, "first");
    if (any (isinf (vertices(edges(i_shortest_edge,:),:))))
      warning (["reducepatch: shortest edge is marked as deleted. ", ...
                "This should never happen."])
      clean_finish = false;
      break;
    endif

    start_point_idx = edges(i_shortest_edge,1);
    end_point_idx = edges(i_shortest_edge,2);

    ## collapse the edge to its midpoint
    vertices(start_point_idx,:) = 0.5 * ...
        (vertices(start_point_idx,:) + vertices(end_point_idx,:));
    ## endpoint will be deleted later

    ## remove collapsed edge info
    edges(i_shortest_edge,:) = [];
    edge_length(i_shortest_edge) = [];

    ## unite collapsed edges of neighboring faces to one edge
    faces_with_collapsed_edge = any (faces == start_point_idx, 2) & ...
                                any (faces == end_point_idx, 2);
    edges_of_collapsed_faces = faces(faces_with_collapsed_edge,:);
    ## get other vertex of collapsed faces
    third_vertices = ...
        edges_of_collapsed_faces(...
            faces(faces_with_collapsed_edge,:) != start_point_idx & ...
            faces(faces_with_collapsed_edge,:) != end_point_idx);
    if (! isempty (third_vertices))
      ## delete edge of the collapsed faces which also has end_point
      ## (keep the one with start_point)
      edges_to_delete = any (edges == end_point_idx, 2) & ...
          any (any (bsxfun (@eq, edges, ...
                            reshape (third_vertices, 1, 1, [])), 3), 2);
      edges(edges_to_delete,:) = [];
      edge_length(edges_to_delete) = [];
    endif

    ## mark the faces that collapsed for later removal
    faces(faces_with_collapsed_edge,:) = Inf;

    ## update the lengths of the moved edges
    edges_with_moved_point = any (edges == start_point_idx, 2);
    edge_length(edges_with_moved_point) = ...
        norm (vertices(edges(edges_with_moved_point,1),:) - ...
              vertices(edges(edges_with_moved_point,2),:), 2, "rows");

    ## replace all vertices that use end_point to use start_point
    if (start_point_idx != end_point_idx)
      edges(edges == end_point_idx) = start_point_idx;
      faces(faces == end_point_idx) = start_point_idx;
      vertices(end_point_idx,:) = Inf; # mark vertex for later removal
    endif

    ## Pretty print a row of dots while performing calculation
    if (verbose && ++verbose_counter > verbose_stepwidth)
      printf (".");
      verbose_counter = 0;
    endif

  until (max_faces > num_faces - sum (isinf (faces(:,1))))

  if (verbose)
    printf ("\n");
  endif

  if (! clean_finish)
    ## FIXME: What should be done in this case?
    ## continue anyway?
  endif

  ## finally, remove vertices and faces
  removed_vertices = isinf (vertices(:,1));
  vertices_reduced = vertices(! removed_vertices,:);

  faces_reduced = faces(! isinf (faces(:,1)),:);

  ## renumber vertices in faces with subsequent numbers
  vertex_renum = ones (rows (vertices), 1);
  vertex_renum(removed_vertices) = 0;
  vertex_renum = cumsum (vertex_renum);
  faces_reduced = vertex_renum(faces_reduced);

endfunction


%!demo
%! clf;
%! [x,y,z] = meshgrid (-2:0.5:2, -2:0.5:2, -2:0.5:2);
%! val = x.^2 + y.^2 + z.^2;
%! fv = isosurface (x, y, z, val, 1);
%! ax1 = subplot (1, 2, 1);
%! patch (fv, "FaceColor", "g");
%! view (3);  axis equal;
%! title ("Sphere with all faces");
%! ax2 = subplot (1, 2, 2);
%! patch (reducepatch (fv, 72), "FaceColor", "g");
%! view (3);  axis equal;
%! title ("Sphere with reduced number of faces");
%! linkprop ([ax1, ax2], {"CameraPosition", "CameraUpVector"});

%!shared faces, vertices, fv, num_faces
%! [x,y,z] = meshgrid (-2:0.5:2, -2:0.5:2, -2:0.5:2);
%! val = x.^2 + y.^2 + z.^2;
%! [faces, vertices] = isosurface (x, y, z, val, 1);
%! fv.faces = faces;  fv.vertices = vertices;
%! num_faces = rows (faces);

## one input (structure), one output
%!test
%! fv_reduced = reducepatch (fv);
%! assert (size (fv_reduced.faces, 1), num_faces * .5, 3);
%! assert (size (fv_reduced.faces, 2), 3);
%! assert (size (fv_reduced.vertices, 2), 3);

## two inputs (faces, vertices), one output
%!test
%! fv_reduced = reducepatch (faces, vertices);
%! assert (size (fv_reduced.faces, 1), num_faces * .5, 3);
%! assert (size (fv_reduced.faces, 2), 3);
%! assert (size (fv_reduced.vertices, 2), 3);

## two inputs (structure, reduction_factor > 1), two outputs
%!test
%! [faces_reduced, vertices_reduced] = reducepatch (fv, 20);
%! assert (rows (faces_reduced), 20, 3);
%! assert (columns (faces_reduced), 3);
%! assert (columns (vertices_reduced), 3);

## three inputs (faces, vertices, reduction_factor < 1), two outputs
%!test
%! [faces_reduced, vertices_reduced] = reducepatch (faces, vertices, .5);
%! assert (rows (faces_reduced), num_faces * .5, 3);
%! assert (columns (faces_reduced), 3);
%! assert (columns (vertices_reduced), 3);

## two inputs (structure, reduction_factor < 1) + one string, no outputs
## (update patch object in figure)
%!test
%! h_fig = figure ("visible", "off");
%! p = patch (fv);
%! reducepatch (p, .35, "f");
%! assert (size (get (p, "Faces"), 1), num_faces * .35, 3);
%! assert (size (get (p, "Faces"), 2), 3);
%! assert (size (get (p, "Vertices"), 2), 3);
%! close (h_fig);

## three inputs (faces, vertices, reduction_factor > 1) + one empty
## string, one output
%!test
%! fv_reduced = reducepatch (faces, vertices, 52, "");
%! assert (size (fv_reduced.faces, 1), 52, 3);
%! assert (size (fv_reduced.faces, 2), 3);
%! assert (size (fv_reduced.vertices, 2), 3);

## two inputs (structure, reduction_factor < 1) + one string, two outputs
%!test
%! [faces_reduced, vertices_reduced] = reducepatch (fv, .4, "fast");
%! assert (rows (faces_reduced), num_faces * .4, 3);
%! assert (columns (faces_reduced), 3);
%! assert (columns (vertices_reduced), 3);

## one input (structure) + two (empty) strings, two outputs
%!test
%! [faces_reduced, vertices_reduced] = reducepatch (fv, "f", "");
%! assert (rows (faces_reduced), num_faces * .5, 3);
%! assert (columns (faces_reduced), 3);
%! assert (columns (vertices_reduced), 3);

## test for each error
%!error <Invalid call> reducepatch ()
%!error <Invalid call> reducepatch (fv, faces, vertices, .5, "f", "v")
%!error <reducepatch: parameter 'foo' not supported>
%! fv_reduced = reducepatch (faces, vertices, .7, "foo");
%!error <struct FV must contain the fields 'vertices' and 'faces'>
%! fv_incomplete.faces = faces;
%! reducepatch (fv_incomplete, .7);
%!error <PATCH_HANDLE must be a valid handle to a patch> reducepatch (pi, .7)
%!error <If first argument is a matrix containing FACES, second argument must be a matrix containing VERTICES> reducepatch (faces)
%!error <REDUCTION_FACTOR must be a positive scalar> reducepatch (fv, [.7 .5])
%!error <REDUCTION_FACTOR must be a positive scalar> reducepatch (fv, -5)
%!error <VERTICES must be an Mx3 matrix> reducepatch (faces, .7, "v")
%!error <reducepatch: Currently patches must consist of triangles only>
%! faces_new = NaN (rows (faces), 4);
%! faces_new(:,1:3) = faces;
%! faces_new(1,4) = 5;
%! reducepatch (faces_new, vertices);
%!error <reducepatch: not enough VERTICES for given FACES>
%! faces_new = faces;
%! faces_new(1,3) = rows (vertices) + 1;
%! reducepatch (faces_new, vertices);
