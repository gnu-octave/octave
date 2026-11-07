graph_doc_examples_TEST_FILES = \
  %reldir%/doc-examples/addedge_example.m \
  %reldir%/doc-examples/addnode_example.m \
  %reldir%/doc-examples/adjacency_incidence_laplacian.m \
  %reldir%/doc-examples/bfsearch_example.m \
  %reldir%/doc-examples/centrality_example.m \
  %reldir%/doc-examples/conncomp_example.m \
  %reldir%/doc-examples/degree_example.m \
  %reldir%/doc-examples/dfsearch_example.m \
  %reldir%/doc-examples/digraph_edge_weights.m \
  %reldir%/doc-examples/digraph_from_adjacency.m \
  %reldir%/doc-examples/digraph_node_names.m \
  %reldir%/doc-examples/digraph_numeric_ids.m \
  %reldir%/doc-examples/digraph_omitselfloops.m \
  %reldir%/doc-examples/distances_example.m \
  %reldir%/doc-examples/findnode_findedge_example.m \
  %reldir%/doc-examples/flipedge_example.m \
  %reldir%/doc-examples/graph_edge_weights.m \
  %reldir%/doc-examples/graph_from_adjacency.m \
  %reldir%/doc-examples/graph_node_names.m \
  %reldir%/doc-examples/graph_numeric_ids.m \
  %reldir%/doc-examples/ismultigraph_example.m \
  %reldir%/doc-examples/neighbors_example.m \
  %reldir%/doc-examples/reordernodes_example.m \
  %reldir%/doc-examples/rmedge_example.m \
  %reldir%/doc-examples/rmnode_example.m \
  %reldir%/doc-examples/shortestpath_example.m \
  %reldir%/doc-examples/subgraph_example.m \
  %reldir%/doc-examples/successors_predecessors_example.m \
  %reldir%/doc-examples/toposort_isdag_example.m

graph_TEST_FILES = \
  %reldir%/doc-examples.tst \
  %reldir%/run_doc_examples.m \
  $(graph_doc_examples_TEST_FILES)

TEST_FILES += $(graph_TEST_FILES)
