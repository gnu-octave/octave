Octave Roadmap (version 10 onwards)

This roadmap is intended to be a living document, and serves multiple purposes:
* For developers to agree on activities and large-scale features,
* To decide priorities,
* To inform new contributors where they can contribute and feel ownership,
* To allow easier parcelling of smaller activities into GSoC etc,
* To be a transparent basis for any financial decisions to spend project money.

This document is different from a wishlist of features, such as those
present on the Octave wiki, in that this document lists names of people
willing to work on specific activities, whether they are contributors writing code
themselves, or existing Octave maintainers soliciting / reviewing / accepting code
from new contributors. For new contributors, this is a way to get your contribution
reviewed and merged into the Octave codebase with more confidence, and a way
to work with a more experienced Octave developer so you can become an Octave
developer over time too. For existing maintainers, this is a way keep getting
new developers and contributors in, and helps Octave development scale.

If you want to take ownership of some activity below, or create a new activity,
please edit the list as necessary, add your name, and post about it on Discourse.
If you can, please split up a large activity into smaller pieces that can then
be specifically written by new contributors, GSoC interns, etc. The aim is to
allow anyone to jump in to start contributing to a topic.

This roadmap process starts from Octave 10. As has been the practice for many
years, a major version of Octave is released each year, and no single feature
will be "required" for any given version to be released, but it is good to have
target versions for this list of features so that certain long-pending activities
get more priority.

- Better support for classdef. (LEAD?)
  - Matlab-compatible classdef behavior.
  - (Needs to be split up into smaller activities.)
  - Target: Octave 10.

- HDF5 compatibility with Matlab: (LEAD?)
  - Deprecate Octave's native savefile formats in favor of Matlab-compatible HDF5.
  - Decide how to handle backwards compatibility with older savefiles.
  - Influenced by classdef decision above.
  - Target: Octave 10.

- String class "foo" as distinct from array of characters 'foo'. (LEAD?)
  - See https://octave.discourse.group/t/implementation-of-a-string-class/1089
  - Depends on HDF5.
  - Target: Octave 10.

- Hashmap containers.  (LEAD? GUILLAUME?)
  - See https://octave.discourse.group/t/adding-hashmaps-to-octave/3306
  - Depends on HDF5 and string.
  - Target: Octave 10.

- Release bytecode interpreter.  (PETTER, JWE)
  - First question: When Octave is released with the bytecode interpreter,
    will it *replace* the tree-walker or will it sit alongside it? Once that
    question is answered and agreed upon, these activities follow.
  - Behavior compatibility with tree-walking interpreter.
  - Code clarity and documentation.
  - Style check.
  - Performance experiments.
  - Target: Octave 10

- Graph theory routines (ARUN)
  - These need a full overhaul and in some cases ground-up implementation.
  - Matlab switched to graph objects some versions ago, as opposed to the
    traditional approach (pre-2016) of directly manipulating adjacency matrices
    and edge lists.
  - Octave needs to implement / import many classical graph functions
    (e.g. all-pairs shortest paths, transitive closure, betweenness centrality, etc).
    These are not difficult to write, but have mostly been written by end users
    for their own work, so the first effort is to converge on a usable function API.
  - Possible dependency on HDF5 if graph classes are written as classdefs,
    so graph objects will not be saved until then, but the rest of the
    development can start and proceed.
  - Target: Octave 10-11, potential GSoC project for 2024.

- Argument blocks implementation. (LEAD?)
  - Some work has already been done, but needs to be completed.
  - Target: Octave 11.

- Assess OpenGL role and improvements.  (RIK?)
  - Target: Octave 11

- Replace GLPK with more performant solver for LP / MILP. (ARUN)
  - Candidate: HiGHS.
  - More generally, provide a usable API for optimization routines so that
    users can drop in their own favorite solver. The idea is that the user
    should be able to switch backend solvers with just a simple change
    like changing this:
    `linprog (... , "solver", "glpk")`
    to this:
    `linprog (... , "solver", "highs")`
    without changing any other user-written code. Can this sort of thing
    be done properly so that new solvers are easy to add to Octave by their
    respective authors?
  - Target: Octave 11
