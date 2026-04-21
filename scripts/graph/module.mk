FCN_FILE_DIRS += \
  %reldir% \
  %reldir%/private

%canon_reldir%_PRIVATE_FCN_FILES = \
  %reldir%/private/__addedge_impl__.m \
  %reldir%/private/__addnode_impl__.m \
  %reldir%/private/__allcycles_impl__.m \
  %reldir%/private/__allpaths_impl__.m \
  %reldir%/private/__bfsdfs_parse_opts__.m \
  %reldir%/private/__bfsearch_events_impl__.m \
  %reldir%/private/__bfsearch_impl__.m \
  %reldir%/private/__biconncomp__.m \
  %reldir%/private/__centrality_betweenness__.m \
  %reldir%/private/__centrality_closeness__.m \
  %reldir%/private/__centrality_eigenvector__.m \
  %reldir%/private/__centrality_pagerank__.m \
  %reldir%/private/__conncomp_parse_opts__.m \
  %reldir%/private/__conncomp_strong__.m \
  %reldir%/private/__conncomp_weak__.m \
  %reldir%/private/__dfsearch_events_impl__.m \
  %reldir%/private/__dfsearch_impl__.m \
  %reldir%/private/__distances_bellman_ford__.m \
  %reldir%/private/__distances_dag__.m \
  %reldir%/private/__distances_dijkstra__.m \
  %reldir%/private/__distances_johnson__.m \
  %reldir%/private/__distances_parse_opts__.m \
  %reldir%/private/__distances_unweighted__.m \
  %reldir%/private/__edgecount_impl__.m \
  %reldir%/private/__findedge_impl__.m \
  %reldir%/private/__findnode_impl__.m \
  %reldir%/private/__matlab_ref__.m \
  %reldir%/private/__maxflow_edmonds_karp__.m \
  %reldir%/private/__maxflow_parse_algorithm__.m \
  %reldir%/private/__maxflow_searchtrees__.m \
  %reldir%/private/__reordernodes_impl__.m \
  %reldir%/private/__resolve_endpoint__.m \
  %reldir%/private/__resolve_node_list__.m \
  %reldir%/private/__resolve_single_node__.m \
  %reldir%/private/__rmedge_impl__.m \
  %reldir%/private/__rmnode_impl__.m \
  %reldir%/private/__shortestpath_bellman_ford__.m \
  %reldir%/private/__shortestpath_dijkstra__.m \
  %reldir%/private/__shortestpath_parse_method__.m \
  %reldir%/private/__shortestpathtree_bellman_ford__.m \
  %reldir%/private/__shortestpathtree_dijkstra__.m \
  %reldir%/private/__shortestpathtree_impl__.m \
  %reldir%/private/__simplify_parse_opts__.m \
  %reldir%/private/__subgraph_impl__.m \
  %reldir%/private/__toposort_parse_opts__.m

%canon_reldir%_FCN_FILES = \
  %reldir%/.oct-config \
  %reldir%/addedge.m \
  %reldir%/addnode.m \
  %reldir%/adjacency.m \
  %reldir%/allcycles.m \
  %reldir%/allpaths.m \
  %reldir%/bfsearch.m \
  %reldir%/biconncomp.m \
  %reldir%/centrality.m \
  %reldir%/condensation.m \
  %reldir%/conncomp.m \
  %reldir%/degree.m \
  %reldir%/dfsearch.m \
  %reldir%/digraph.m \
  %reldir%/distances.m \
  %reldir%/edgecount.m \
  %reldir%/findedge.m \
  %reldir%/findnode.m \
  %reldir%/flipedge.m \
  %reldir%/graph.m \
  %reldir%/incidence.m \
  %reldir%/indegree.m \
  %reldir%/inedges.m \
  %reldir%/isdag.m \
  %reldir%/ismultigraph.m \
  %reldir%/laplacian.m \
  %reldir%/maxflow.m \
  %reldir%/mincut.m \
  %reldir%/neighbors.m \
  %reldir%/numedges.m \
  %reldir%/numnodes.m \
  %reldir%/outdegree.m \
  %reldir%/outedges.m \
  %reldir%/predecessors.m \
  %reldir%/reordernodes.m \
  %reldir%/rmedge.m \
  %reldir%/rmnode.m \
  %reldir%/shortestpath.m \
  %reldir%/shortestpathtree.m \
  %reldir%/simplify.m \
  %reldir%/subgraph.m \
  %reldir%/successors.m \
  %reldir%/toposort.m \
  %reldir%/transclosure.m \
  %reldir%/transreduction.m

%canon_reldir%dir = $(fcnfiledir)/graph

%canon_reldir%_DATA = $(%canon_reldir%_FCN_FILES)

%canon_reldir%_privatedir = $(fcnfiledir)/graph/private

%canon_reldir%_private_DATA = $(%canon_reldir%_PRIVATE_FCN_FILES)

FCN_FILES += \
  $(%canon_reldir%_FCN_FILES) \
  $(%canon_reldir%_PRIVATE_FCN_FILES)

PKG_ADD_FILES += %reldir%/PKG_ADD

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)
