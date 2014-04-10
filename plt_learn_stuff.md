# To Learn :)
===============================================
## 2. AE - Arithmetic Expressions
1. Enviroments are mappings: Identifiers(Symbols) -> Values
    * Interpreter:
  ```scala
  def eval(e: Exp, env: Env) : Int = e match {
	case Num(n) => n
	case Id(x) => env(x)
	case Add(l,r) => eval(l,env) + eval(r,env)
	case Mul(l,r) => eval(l,env) * eval(r,env)
  }
  val testEnv = Map('x -> 3, 'y -> 4)
  ```
2. fold - Visitor Pattern
    
## 3. WAE - With-Arithmetic Expressions
1. with Binder - etwas einen Namen geben

  ``` scala
  type Env = Map[Symbol,Int]
  ```

  ```scala
  case class With(x: Symbol, xdef: Exp, body: Exp) extends Exp
  ```
  
2. Definitions:
	* Binding Instance: 
		*A binding instance of an identiﬁer is the instance of the identiﬁer that gives it its value. In WAE , the 'x' position of a with is the only binding instance.
	* Scope: 
		*The scope of a binding instance is the region of program text in which instances of the identiﬁer refer to the value bound by the binding instance.
	* Bound Instance:
		* An identiﬁer is bound if it is contained within the scope of a binding instance of its name.
	* Free Instance:
		* An identiﬁer not contained in the scope of any binding instance of its name is said to be free.
3. Substitution kann verwendet werden um "Namen" in PL zu verstehen
4. Die korrekte Implementation von Substitution muss mit freien und gebundenen Variablen sowie gebundenen Instanzen von Namen und deren Scopes korrekt umgehen können
  ``` scala
   def makeEval(subst: (Exp,Symbol,Num)=>Exp) : Exp=>Int = {
    def eval(e: Exp) : Int = e match {
      case Num(n) => n
      case Id(x) => sys.error("unbound variable: " + x.name)
      case Add(l,r) => eval(l) + eval(r)
      case Mul(l,r) => eval(l) * eval(r)
      case With(x, xdef, body) => eval(subst(body,x,Num(eval(xdef)))) // take the int and wrap it into a Num
     }
    eval
   }
   
   val subst5 : (Exp,Symbol,Num) => Exp = (e,i,v) => e match {
    case Num(n) => e
    case Id(x) => if (x == i) v else e
    case Add(l,r) => Add( subst5(l,i,v), subst5(r,i,v))
    case Mul(l,r) => Mul( subst5(l,i,v), subst5(r,i,v))
    // handle shadowing correctly
    case With(x,xdef,body) => With(x,
                                   subst5(xdef,i,v),
                                   if (x == i) body else subst5(body,i,v))
   }
   makeEval(With('x, 5, With('x, 'x, 'x)) // == 5
```
##4. F1- WAE - First order-With-AE    

1. Warum Funktionen( Prozeduren/Methoden)?
	  * Mit with lässt sich x*x/2 nicht umsetzten (speichern) 
2. First-Order Funktion
	  * no expression
	  * kann nicht übergeben- oder zurückgegeben werden
	  * called by name
	  * beinhaltet bel. # von args und body
  ```scala	  
  case class Call(f: Symbol, args: List[Exp]) extends Exp // functions are called by name	  
  
  case class FunDef(args: List[Symbol], body: Exp)
  
  type Funs = Map[Symbol,FunDef]
  ```
3. different namespaces
    * Name der Funktion wird in eval gehandhabt, subst macht den Rest
4. ``` type Env = Map[Symbol,Int] ```
 ``` scala
 def evalWithEnv(funs: Funs, env: Env, e: Exp) : Int = e match {
    case Num(n) => n
    case Id(x) => env(x) // look up in repository of deferred substitutions
    case Add(l,r) => evalWithEnv(funs,env,l) + evalWithEnv(funs,env,r)
    case Mul(l,r) => evalWithEnv(funs,env,l) * evalWithEnv(funs,env,r)
    case With(x, xdef, body) => evalWithEnv(funs,env+ ((x,evalWithEnv(funs,env,xdef))),body) 
    case Call(f,args) => {
      val fd = funs(f) // lookup function definition 
      val vargs = args.map(evalWithEnv(funs,env,_)) // evaluate function arguments
      if (fd.args.size != vargs.size) sys.error("number of paramters in call to " + f.name + " does not match")
      // We construct the environment by associating each formal argument to its actual value    
      val newenv = Map() ++ fd.args.zip(vargs)
      evalWithEnv(funs,newenv,fd.body)
  }   
}

 evalWithEnv(someFuns,Map.empty, Call('doubleadder,List(2,3))

 def evalDynScope(funs: Funs, env: Env, e: Exp) : Int = e match {
    case Num(n) => n
    case Id(x) => env(x)
    case Add(l,r) => evalDynScope(funs,env,l) + evalDynScope(funs,env,r)
    case Mul(l,r) => evalDynScope(funs,env,l) * evalDynScope(funs,env,r)
    case With(x, xdef, body) => evalDynScope(funs,env+ ((x,evalDynScope(funs,env,xdef))),body) 
    case Call(f,args) => {
      val fd = funs(f) 
      val vargs = args.map(evalDynScope(funs,env,_)) 
      if (fd.args.size != vargs.size) sys.error("number of paramters in call to "+f+" does not match")   
      val newenv = env ++ fd.args.zip(vargs) // extending env instead of Map() !!
      evalDynScope(funs,newenv,fd.body)
  }   
}
 evalDynScope(someFuns,Map.empty, Call('doubleadder,List(2,3))
 ```
5. Static Scope: In einer PL mit "static scope", der Scope eines Identifiers ist eine syntaktisch eingegrenzte Region
    * Eine typische Region wäre der body einer Funktion oder einem anderen "binding construct" 
 
6. Dynamic Scope: In einer PL mit dynamischen scoping, der Scope eines Identifiers ist das komplette Enviroments der Ausführung des "binding construct"
    
##5. FAE Funktionale AE - lambda kalkül
1. FAE ist die Sprache der AE + Funktionsabstraktion und -applikation
2. first class Funktionen
	  * Funktionen werden zu Values und können zurück- oder übergeben werden
3. higher-order Funktionen
	  * Die Funktionen akzeptieren oder returnen andere Funktionen
4. lambda x.x+3 (in Scala) => (x) => x+3
5. desugering: constructive translation
6. Freie Variablen: Um vorzubeuen, das x in einer Methode das "gleiche" x ist wie außerhalb.. Sonst: static scoping!
 ``` scala
 def subst(e1 : Exp, x: Symbol, e2: Exp) : Exp = e1 match {
    case Num(n) => e1
    case Add(l,r) => Add(subst(l,x,e2), subst(r,x,e2))
    case Id(y) => if (x == y) e2 else Id(y)
    case App(f,a) => App(subst(f,x,e2),subst(a,x,e2))
    case Fun(param,body) => 
      if (param == x) e1 else {
	val fvs = freeVars(body) ++ freeVars(e2)
	val newvar = freshName(fvs, param)
	Fun(newvar, subst(subst(body, param, Id(newvar)), x, e2))
      }                            
}
def eval(e: Exp) : Exp = e match {
    case Id(v) => sys.error("unbound identifier: " + v.name)
    case Add(l,r) => (eval(l), eval(r)) match {
                     case (Num(x),Num(y)) => Num(x+y)
                     case _ => sys.error("can only add numbers")
                    }
    case App(f,a) => eval(f) match {
     case Fun(x,body) => eval( subst(body,x, eval(a)))
     case _ => sys.error("can only apply functions")
  }
    case _ => e // numbers and functions evaluate to themselves
}
```
7. closures werden gebraucht, da für die Substitution Parts zur Evaluation zurückgestellt werden, diese können ohne ihre passende Environments nicht ausgewertet werden
``` scala  
case class ClosureV(f: Fun, env: Env) extends Value
  
def evalWithEnv(e: Exp, env: Env) : Value = e match {
    case Num(n: Int) => NumV(n)
    case Id(x) => env(x)
    case Add(l,r) => {
      (evalWithEnv(l,env), evalWithEnv(r,env)) match {
	case (NumV(v1),NumV(v2)) => NumV(v1+v2)
	case _ => sys.error("can only add numbers")
      }
    }
    case f@Fun(param,body) => ClosureV(f, env)
    case App(f,a) => evalWithEnv(f,env) match {
      // Use environment stored in closure to realize proper lexical scoping!
      case ClosureV(f,closureEnv) => evalWithEnv(f.body, closureEnv + (f.param -> evalWithEnv(a,env)))
      case _ => sys.error("can only apply functions")
    }
} 
evalWithEnv(App( Fun('x,Add('x,5)), 7), Map.empty)
```   

##6. lcfae - lambda kalkül
1. lazy evaluation 
	  * Erfordert nur eine semantische Änderung und keine der Syntax
	  
2. call-by-name - -Ähnlich lazy evaluation
	  * Anstelle das Argument a im App-Fall zu substituieren, wird zuerst das nicht ausgewertete Argument in den body substituiert (der Rest bleibt)
 ``` scala Evalcbn:
  case App(f,a) => evalcbn(f) match {
     case Fun(x,body) => evalcbn( subst(body,x, a)) // no evaluation of a!
     case _ => sys.error("can only apply functions")
   }
  ```
3. beta-equivalence: ? 
	  * Wenn Funktionen übergeben werden, returnen Evalcbn und eval nicht mehr das gleiche Ergebnis. Wenn wir innerhalb der Funktionsbodys evaluieren würden (under a lambda) dann könnte man den Rückgabewert den eval ausgibt mit dem Rückgabewert von evalcbn produzieren
4. Thunks (Nicht in Scala umsetzbar)
	  * type Thunk = (Exp, Env)
	  * type Env = Map[Symbol, Thunk]
5. Thunks (In Scala)
	  i)represent thunks (type Thunk)
	  ii) create thunks (method delay)
	  iii) evaluate thunks (method force)
```scala
trait CBN {
  type Thunk
    
  case class Env(map: Map[Symbol, Thunk]) {
      def apply(key: Symbol) = map.apply(key)
      def +(other: (Symbol, Thunk)) : Env = Env(map+other)
  }

  def delay(e: Exp, env: Env) : Thunk
  def force(t: Thunk) : Value

  // since values also depend on Env and hence on Thunk they need to
  // be defined within this trait    
  sealed abstract class Value
  case class NumV(n: Int) extends Value
  case class ClosureV(f: Fun, env: Env) extends Value 
  def eval(e: Exp, env: Env) : Value = e match {
      case Id(x) => force(env(x)) // force evaluation of thunk if identifier is evaluated
      case Add(l,r) => {
        (eval(l,env), eval(r,env)) match {
          case (NumV(v1),NumV(v2)) => NumV(v1+v2)
          case _ => sys.error("can only add numbers")
        }
      }
      case App(f,a) => eval(f,env) match {
        // delay argument expression and add it to environment of the closure
        case ClosureV(f,cenv) => eval(f.body, cenv + (f.param -> delay(a,env)))
        case _ => sys.error("can only apply functions")
      }
      case Num(n) => NumV(n)
      case f@Fun(x,body) => ClosureV(f,env)
 }
}
```

6. Call-by-need
	  * Call-by-Name: Ein Argument das n Mal im Body verwendet wird, wird auch n mal ausgewertet 
	  * Im Call-by-need wird das ausgewertete Value gespeichert
``` scala  
object CallByNeed extends CBN {
  case class MemoThunk(e: Exp, env: Env) {
    var cache: Value = null
  }
  type Thunk = MemoThunk
  def delay(e: Exp, env: Env) = MemoThunk(e,env)
  def force(t: Thunk) = {
    if (t.cache == null) {
      println("Forcing evaluation of expression: "+t.e)
      t.cache = eval(t.e, t.env)
    } else println ("Reusing cached value "+t.cache+" for expression "+t.e)
    t.cache
  }
}
```
7. Ist es eine gute idee Implicite Mutation PL mit Lazy Evaluation zu mixen?
	  * Nein, da Variablen weiter verwendet werden..
      
##7. Haskelzeugs..

##8. RCFAE - Recursive Funktionale AE
1. Vorteile von Mutation:
	  * Effizienz: Space (Call Stack), Zeit
	  * Schleifen sind einfacher zu Verstehen als Rekursion
	  * "Memoisation" (Erinnerung?) ist schwierig durch Deklaration auszudrücken
2. Nachteile von Mutation:
	  * Laziness bringt Probleme
	  * Von-Neumann bottleneck 
		* Immer nur eine Sache auf einmal
		*das Verbindungssystem (Daten- und Befehls-Bus) wird zum Engpass zwischen dem Prozessor und dem Speicher
	  * Mangen von referienzielle Integrität (Wegen Reihenfolgeabhängigkeit)
      
##9. BCFAE - Boxes and Conditional FAE 
1. Mutable Data Structure: Boxes
``` scala
case class NewBox(e: Exp) extends Exp // create a new box
case class SetBox(b: Exp, e: Exp) extends Exp // assign to a box
case class OpenBox(b: Exp) extends Exp // read value in a box
case class Seq(e1: Exp, e2: Exp) extends Exp // sequencing of expressions
	  *Enviroment wird nach jeder Änderung angepasst
```
2. ....
    
##10. Garbage collector
1. Wir setzten es mit einer Mutable Flag um, echte Systeme nutzen Bit Flaggen
2. Mark and Sweep Store 
    * Jedes Objekt das erreicht werden kann, wird makiert, alle nicht makierten Objekte werden wieder "freigegeben" - Speicherfragmentierung
3. Referenzzählung
	  * Es werden die Referenzen die auf ein Objekt zeigen gezählt, bei 0 Ref. wird es freigegeben
	  
##11. Syntatktische- vs. Meta- Interpretation
1. Syntaktische Interpretation
    * Es werden primitive Sprachkonstrukte verwendet
    * wird verwendet wenn wir genau das Feature verstehen wollen
    * Wenn man das Feature anders implementieren will
    * Falls das Feature nicht existiert
    * Man könnte unsere Sprache syntaktischer machen indem man Nummern als Sequencen von Ziffern schreibt, anstelle von Scala Nummern
2. Meta Interpretation
    * implementiert wobei man die Features der Meta Sprache nutzt
    * Wenn wir keine genaue kontrolle über das exakte Meaning des Konstrukts brauchen
    * Wenn die Meta Sprache das feature schon genau so wie gewollt umsetzten
3. HOAS - Higher-order abstract syntax
4. 2 Objekt Sprachen Expressions sind equivalent wenn ihre "denotations" als Meta-Level Expressions sind equivalent im Meta-level
##13. CPS Transformation

##15. Let/cc - Racket/Scheme
1. speichert die continuation (let/cc k (k 3))
2. nützlich für Multithreading und exceptions
    
##16. First class continuation with letcc
1.  Für die Umsetzung von continuation in Scala benötigen wir:
    * CPS-transformation des Interpreters
    * Ein Zweig für Lettcc im Interpreter
2. 
    
##17. Defunktionalisierung
  1. !!?!
##18. Monaden
  1. !!?!
      
    
##23. Typ Systeme - (78)
1. Grundlagen der Induktion
    * Wenn es für P(0) gilt und für alle i P(i) -> P(i+1) dann gilt für alle n.P(n)
2. Syntax
    * Eine abstrakte Gramatik definiert ein Set von abstrakten Syntax Bäumen und schlägt ein mapping von Char Strings zu 	Bäumen vor
3. Introduction on Syntax
    * Relation - Function
    * Induktion über die Länge
4. Structural Operational Semantics (SOS)
    * Reduktions Relationen
    * Digression ?
        * "Computation" Regeln
        * "Congruence" Regeln
5. Grund für Reduktion
    * Ableitungen
    * Normalform
        * Values sind Normalformen
	* Multi-step Reduktion
	  
##24. The Lambda Calculus, formal (119)
1. (lambda_x.t) v 
2. Evaluationsstrategien
	  * Call-by-value lambda-calculus
	  * Call-by-name lambda-calculus
3. Currying
	  * Multiple arguments lambda_f.lamda_y. f (f y)
4. Church Booleans and Numerals
5. Funktionen an Booleans
6. Normalform
	  * Eine Normalform ist ein Term der nicht weiter evaluiert werden kann
	  * Ein "stuck" Term ist eine Normalform die kein Value ist
7. Induktion an Ableitungen (Induction on derivations)
	  * Strukturelle Induktion
8. Substitution und alpha_Equivalenz 
9. Typen
	  * Typen Regeln
	  * Typen Ableitungen
	  * Ungenauigkeit des Typens (Imprecision of Typing)
10. Eigenschaften der Typen Relation
	  * Type Safty
		* Progress: Fortschritt
		* Preservation: Erhaltung
	  * Untyped lamdba-calculus mit Booleans
	  * Typ Annotationen
	  * Typ Regeln
	  * Bsp Preservation/Erhaltung
11. The Simply Typed Lamdba-Calculus (Lambda_Pfeil/L_P)
	  * Eigenschaften
		* Progress
		* Preservation
	  * Inversion (Umkehrung)
	  * Ascription (Belegung/Zuweisung)
	  * Let-bindings
12. Pairs, Tuples and Records
	  * Pairs
	    * Evaluationregeln
	    * Typenregeln
	  * Tupel
	    * Evaluationregeln
	    * Typenregeln
	  * Records
	    * Generalisiere Binary Products -> labeled Records
	    * Evaluationregeln
	    * Typenregeln
13. Sums und Variants
	  * Syntaktische Form
	  * Sums und Uniqueness von Typen
	  * Variants
		* Generalisiere Binary Sums -> Labeled Variants
		* Syntaktische Form
		* Neue Evaluationregeln
	  * Options und Enumerations
	      * Kann durch Sum und Product Typen wie in Haskell umgesetzt werden
14. Rekursion
	  * (typed) fixed-points operator
		* Neue syntaktische Form
		* Neue Typenregeln
		* letrec
	      
##25. Subtyping - (85)
1. Subtype Relation
	  * Records
	  * Arrow types
	  * Top
	  * General Rules
2. Eigenschaften von Subtyping
	  * Safety
	  * Preservation
	  * Inversion
3. Subtyping mit anderen Features
	* Ascription
	* Casting
	* Subtyping und Variants
	* Subtyping und Lists
	* Subtyping und References
	* Subtyping und Arrays
4. Algorithmic Subtyping
	* Syntax-directed Regeln
	* Non-Syntax-directedness of 
		* Typing
		* Subtyping
	* Entwicklung eines Alorithmus für die "subtyping relation"
5. Algorithmic Typing
	  * Type Checker for the lambda-calculus with subtyping
	  * Typing Rules
6. Meets and Joins
	  * Meets: 
	  * Joins:
7. Universal Types
	  * System F
	  * Eigenschaften von System F
	
##26. Typ Rekonstruktion - (19)
1. aka Type inference
2. Vor- und Nachteile
	  + Type checker ist einfach
	  - Typ Annotationen immer hinzuschreiben kann ermüdend sein, da viele Annotationen eindeutig sind
3. Robinson Algorithm
4. Curry-Howard Isomorphism aka "Propositions as Types" - Zeichen der Logik
5. Intuitionistische Logik
6. Reduktion = Beweis Normalisation
7. CPS Transformation = Doppelte Negation
	
##27. Modeling Java - (71)
1. Featherweight Java (FJ)
2. Formalizing FJ
3. JF Syntax
4. Subtyping
5. Evaluation 
6. Typing
7. Eigenschaften
	  * Progress
	  * Preservation
		* "Stupid Cast" typing Rule
8. More on Evaluation Context 

##29. Existential Types - (13)
1. Building and using terms with existential types
	  * Introduction and Elimination Regeln
2. Encoding existential types by universal types
	  * SML
3. Open vs. closed Scope

##30. Higher-Order Typen - (22)
1. Higher-Order Types in Haskell
2. Evaluation
3. Kinding rules
4. Typing rules
5. Type Equivalence
6. Erweiterung von System F
7. Higher-Order Existentials
8. Algorithmic Type-Checking for F
