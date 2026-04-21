FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__addedge_impl__.m \
  %reldir%/private/__addnode_impl__.m \
  %reldir%/private/__edgecount_impl__.m \
  %reldir%/private/__findedge_impl__.m \
  %reldir%/private/__findnode_impl__.m \
  %reldir%/private/__matlab_ref__.m \
  %reldir%/private/__reordernodes_impl__.m \
  %reldir%/private/__resolve_endpoint__.m \
  %reldir%/private/__resolve_node_list__.m \
  %reldir%/private/__resolve_single_node__.m \
  %reldir%/private/__rmedge_impl__.m \
  %reldir%/private/__rmnode_impl__.m \
  %reldir%/private/__subgraph_impl__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/addedge.m \
  %reldir%/addnode.m \
  %reldir%/adjacency.m \
  %reldir%/degree.m \
  %reldir%/digraph.m \
  %reldir%/edgecount.m \
  %reldir%/findedge.m \
  %reldir%/findnode.m \
  %reldir%/graph.m \
  %reldir%/incidence.m \
  %reldir%/indegree.m \
  %reldir%/inedges.m \
  %reldir%/ismultigraph.m \
  %reldir%/laplacian.m \
  %reldir%/neighbors.m \
  %reldir%/numedges.m \
  %reldir%/numnodes.m \
  %reldir%/outdegree.m \
  %reldir%/outedges.m \
  %reldir%/predecessors.m \
  %reldir%/reordernodes.m \
  %reldir%/rmedge.m \
  %reldir%/rmnode.m \
  %reldir%/subgraph.m \
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
