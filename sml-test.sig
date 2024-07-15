
signature SML_TEST =
sig

type 'a tree
type spec = unit tree

(** Creating tests **)

(* Create a test - if the function returns then it's a success *)
val it : string -> ('a -> unit) -> 'a tree
val specify : string -> ('a -> unit) -> 'a tree

(* Group tests together under a label *)
val describe : string -> 'a tree list -> 'a tree
val context : string -> 'a tree list -> 'a tree

(* Skip the test(s) *)
val xit : string -> ('a -> unit) -> 'a tree
val xspecify : string -> ('a -> unit) -> 'a tree
val xdescribe : string -> 'a tree list -> 'a tree
val xcontext : string -> 'a tree list -> 'a tree

(** Asserting conditions **)

(* Signal the test as incomplete, and is skipped *)
val todo : unit -> 'a
val todoWith : string -> 'a
val skip : unit -> 'a
val skipWith : string -> 'a

(* Signals an error on [false] *)
val assertBool : string -> bool -> unit
(* Signals an error if the values aren't equal
   [assertEqualWith eq toString label expected actual] *)
val assertEqualWith : ('a * 'a -> bool) -> ('a -> string) -> string -> 'a -> 'a -> unit
(* Signals an error unconditionally *)
val assertFailure : string -> 'a
(* Signals an error if the string is non-empty *)
val assertString : string -> unit

(* Specialized [assertEqualWith] *)
val assertEqualBool : string -> bool -> bool -> unit
val assertEqualChar : string -> char -> char -> unit
val assertEqualString : string -> string -> string -> unit
val assertEqualInt : string -> int -> int -> unit
val assertEqualWord : string -> word -> word -> unit

(** Running tests **)

(* Write the output to the given outstream *)
val runTAP : TextIO.outstream * spec -> unit

(* Write the output to a spawned process [(nameOrPath, arguments, spec)]
 * On Posix, the arguments become argv[1] .. argv[n]
 * On Windows, the arguments are escaped for CommandLineToArgvW() *)
val runTAPHarness : string * string list * spec -> unit

(** Modifying the test tree **)

(* Run a custom action around each of the tests *)
val around : (('a -> unit) -> unit) -> 'a tree -> spec
(* Run a custom action before each of the tests *)
val before : (unit -> 'a) -> 'a tree -> spec
(* Run a custom action after each of the tests (handles exceptions) *)
val after : ('a -> unit) -> 'a tree -> 'a tree
(* Versions of the functions that modify the context instead of consuming it *)
val aroundWith : (('a -> unit) -> ('b -> unit)) -> 'a tree -> 'b tree
val beforeWith : ('b -> 'a) -> 'a tree -> 'b tree

end
