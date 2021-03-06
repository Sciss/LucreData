# LucreData

[![Build Status](https://travis-ci.org/Sciss/LucreData.svg?branch=master)](https://travis-ci.org/Sciss/LucreData)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucredata_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucredata_2.11)

## statement

(C)opyright 2011&ndash;2014 Hanns Holger Rutz. This software is released under the [GNU Lesser General Public License](http://github.com/Sciss/LucreData/blob/master/licenses/LucreData-License.txt) v2.1+.

LucreData provides implementations for various data structures in the Scala programming language related to marked ancestor problems and spatial nearest neighbour queries. These are mutable structures implemented transactionally on top of the LucreSTM library.

* Rutz, H. H., "A Reactive, Confluently Persistent Framework for the Design of Computer Music," in Proceedings of the 9th Sound an Music Computing Conference (SMC), Copenhagen 2012.

* Eppstein, D. and Goodrich, M.T. and Sun, J.Z., "The Skip Quadtree: A Simple Dynamic Data Structure for Multidimensional Data," in Proceedings of the twenty-first annual symposium on Computational geometry, pp. 296--305, 2005.

* Papadakis, T., "Skip lists and probabilistic analysis of algorithms", University of Waterloo, Waterloo (CA), 1993.

* Bender, M. and Cole, R. and Demaine, E. and Farach-Colton, M. and Zito, J., "Two Simplified Algorithms for Maintaining Order in a List," in Algorithms—ESA 2002, pp. 219--223, 2002.

* Fiat, A. and Kaplan, H., "Making data structures confluently persistent," in Proceedings of the 12th annual ACM-SIAM symposium on Discrete algorithms, pp. 537–546, 2001.

## requirements

Builds with sbt 0.13 against Scala 2.11, 2.10. Depends on [LucreSTM](http://github.com/Sciss/LucreSTM). Sub projects:

* `core` &ndash; Transactional data structures (based on [LucreSTM](https://github.com/Sciss/LucreSTM)).
* `views` &ndash; Some Swing based views to show the transactional data structures

Due to a problem with Java Swing generics, the view tests currently require to compile on a JDK 6 instead of 7 or higher. This will be addressed in the next release.

## linking to LucreData

The following draws in all sub modules:

    "de.sciss" %% "lucredata" % v

For a particular sub module:

    "de.sciss" %% "lucredata-core"  % v
    "de.sciss" %% "lucredata-views" % v

The current version `v` is `"2.3.3"`.
