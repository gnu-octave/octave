FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__matlab_ref__.m \
  %reldir%/private/__resolve_endpoint__.m \
  %reldir%/private/__resolve_node_list__.m \
  %reldir%/private/__resolve_single_node__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/digraph.m \
  %reldir%/graph.m \
  %reldir%/indegree.m \
  %reldir%/neighbors.m \
  %reldir%/numedges.m \
  %reldir%/numnodes.m \
  %reldir%/outdegree.m \
  %reldir%/predecessors.m \
  %reldir%/successors.m

%canon_reldir%dir = $(fcnfiledir)/graph

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/graph/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
