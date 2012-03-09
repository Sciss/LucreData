seq(lsSettings: _*)

// ---- ls.implicit.ly ----

(LsKeys.tags   in LsKeys.lsync) := Seq( "data-structures", "transactional", "spatial", "stm" )

(LsKeys.ghUser in LsKeys.lsync) := Some( "Sciss" )

(LsKeys.ghRepo in LsKeys.lsync) := Some( "LucreData" )

// bug in ls -- doesn't find the licenses from global scope
(licenses in LsKeys.lsync) := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))
