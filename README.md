## LucreData

### statement

(C)opyright 2011 Hanns Holger Rutz. This software is released under the [GNU General Public License](http://github.com/Sciss/LucreData/blob/master/licenses/LucreData-License.txt).

This is a sandbox project for implementing and probing various data structures in the Scala programming language related to marked ancestor problems and spatial nearest neighbour queries.

* Alstrup, S. and Husfeldt, T. and Rauhe, T., "Marked ancestor problems," in Proceedings of the 39th Annual Symposium on Foundations of Computer Science, p. 534, 1998.

* Eppstein, D. and Goodrich, M.T. and Sun, J.Z., "The Skip Quadtree: A Simple Dynamic Data Structure for Multidimensional Data," in Proceedings of the twenty-first annual symposium on Computational geometry, pp. 296--305, 2005.

* Papadakis, T., "Skip lists and probabilistic analysis of algorithms", University of Waterloo, Waterloo (CA), 1993.

* Bender, M. and Cole, R. and Demaine, E. and Farach-Colton, M. and Zito, J., "Two Simplified Algorithms for Maintaining Order in a List," in Algorithms—ESA 2002, pp. 219--223, 2002.

* Fiat, A. and Kaplan, H., "Making data structures confluently persistent," in Proceedings of the 12th annual ACM-SIAM symposium on Discrete algorithms, pp. 537–546, 2001.

### requirements

Builds with xsbt (sbt 0.11) against Scala 2.9.1. Depends on [LucreSTM](http://github.com/Sciss/LucreSTM). Sub projects:

* `structs-core` &ndash; Some basic stuff used by the data structure implementations. Also the geometry types.
* `mutable` &ndash; Mutable data structures
* `txn` &ndash; Transactional data structures (based on [LucreSTM](https://github.com/Sciss/LucreSTM)).
* `views-core` &ndash; Some basic stuff used by the specific views
* `mutable-views` &ndash; Some Swing based views to show the mutable data structures
* `txn-views` &ndash; Some Swing based views to show the transactional data structures


### creating an IntelliJ IDEA project

If you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
    
    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "0.11.0")

Then to create the IDEA project, run the following two commands from the xsbt shell:

    > set ideaProjectName := "LucreData"
    > gen-idea
