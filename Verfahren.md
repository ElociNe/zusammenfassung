##Verfahren die man können muss!
########################################################

1. CPS - Continuation passing style 
* Lambda Lifting
* Defunktionalisierung
* System F
* Typ Ableitung/Erkennung
* Analyse von
    * Joins
    * Meets
    * Subtyping
* Churchencoding
* letcc
* Robinson Algorithmus
* Monaden
   * Eine Monade ist ein Weg Berechnungen in "terms of values and sequences" zu strukturieren. Monaden erlauben dem Programmierer berechnungen die sequentielle Blöcke nutzen aufzubauen, die wiederum auch aus sequentiellen Blöcken bestehen können.
   * Monaden bestimmen wie kombinierte Berechungen eine neue Berechnung ergeben und befreien den Programmierer davon diese jedes  Mal wieder manuell coden zu müssen.
   * Monaden weren benutzt um impure Effekte in pure funktionale Sprache zu integrieren.
      * Error Handling (Exceptions)
      * Execution Trace (Ausgabe als Side Effekt)
      * I/O
   * Monaden sin ein abstrakter Datentyp bzw. ein Pattern von funktionaler Struktur, über diese Pattern kann abstrahiert werden.
      * Funktionale Strukturen:
         * Enviroment passing style
         * Store passing style
         * Continuation passing style
         
* Arten von Monaden:
   * Reader aka Enviroment Monad
   * State Monad
   * Continuation Monad
   * Identity Monad
   * I/O Monad
