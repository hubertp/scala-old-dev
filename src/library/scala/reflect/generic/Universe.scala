package scala.reflect
package generic

import event.EventsUniverse

abstract class Universe extends Symbols
                           with EventsUniverse
                           with Types 
                           with Constants 
                           with Scopes
                           with Names
                           with StdNames
                           with Trees
                           with AnnotationInfos
                           with StandardDefinitions {
  type Position
  val NoPosition: Position
}
    
