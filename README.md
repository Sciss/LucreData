## LucreData

### statement

(C)opyright 2011&ndash;2012 Hanns Holger Rutz. This software is released under the [GNU General Public License](http://github.com/Sciss/LucreData/blob/master/licenses/LucreData-License.txt).

LucreData provides implementations for various data structures in the Scala programming language related to marked ancestor problems and spatial nearest neighbour queries. These are mutable structures implemented transactionally on top of the LucreSTM library.

* Rutz, H. H., "A Reactive, Confluently Persistent Framework for the Design of Computer Music," in Proceedings of the 9th Sound an Music Computing Conference (SMC), Copenhagen 2012.

* Eppstein, D. and Goodrich, M.T. and Sun, J.Z., "The Skip Quadtree: A Simple Dynamic Data Structure for Multidimensional Data," in Proceedings of the twenty-first annual symposium on Computational geometry, pp. 296--305, 2005.

* Papadakis, T., "Skip lists and probabilistic analysis of algorithms", University of Waterloo, Waterloo (CA), 1993.

* Bender, M. and Cole, R. and Demaine, E. and Farach-Colton, M. and Zito, J., "Two Simplified Algorithms for Maintaining Order in a List," in Algorithms—ESA 2002, pp. 219--223, 2002.

* Fiat, A. and Kaplan, H., "Making data structures confluently persistent," in Proceedings of the 12th annual ACM-SIAM symposium on Discrete algorithms, pp. 537–546, 2001.

### requirements

Builds with sbt 0.12 against Scala 2.9.2. Depends on [LucreSTM](http://github.com/Sciss/LucreSTM). Sub projects:

* `core` &ndash; Transactional data structures (based on [LucreSTM](https://github.com/Sciss/LucreSTM)).
* `views` &ndash; Some Swing based views to show the transactional data structures

## linking to LucreData

Either of the following dependencies is necessary:

    resolvers += "Oracle Repository" at "http://download.oracle.com/maven"
    
    "de.sciss" %% "lucredata" % "1.4.+"
    "de.sciss" %% "lucredata-core" % "1.4.+"
    "de.sciss" %% "lucredata-views" % "1.4.+"

### creating an IntelliJ IDEA project

To develop the sources of LucreData, if you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
    
    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

Then to create the IDEA project, run `sbt gen-idea`.
