
structure SMLTest : SML_TEST =
struct

nonfix before

datatype 'a test_arguments = TestArgs of {
    hook : ('a -> unit) -> unit
  }

datatype failure_reason =
    NoReason
  | Reason of string
  | Expected of {message: string option, expected: string, actual: string}
  | Exception of string option * exn

(* Non-successes include the reason *)
datatype result_status =
    Success
  | Todo of string option
  | Skip of string option
  | Failure of failure_reason

(* Status and any information gained during testing *)
datatype result = Result of string * result_status

type 'a item = {
    desc : string,
    run : 'a test_arguments -> result
  }

datatype 'a tree =
    Test of 'a item
  | Group of string * 'a tree list

type spec = unit tree

(* Thrown by a test to signal an error *)
exception ExnStatus of result_status

fun assertFailure msg = raise ExnStatus (Failure (Reason msg))

fun assertEqualWith eq tos label expected actual =
  if eq (expected, actual)
    then ()
    else raise ExnStatus (Failure (Expected {
          message = if String.size label = 0 then NONE else SOME label,
          expected = tos expected,
          actual = tos actual
        }))

val assertEqualBool = assertEqualWith op = Bool.toString
val assertEqualChar = assertEqualWith op = (fn c => "#\"" ^ Char.toString c ^ "\"")
val assertEqualString = assertEqualWith op = (fn s => "\"" ^ String.toString s ^ "\"")
val assertEqualInt = assertEqualWith op = Int.toString
val assertEqualWord = assertEqualWith op = (fn w => "0wx" ^ Word.toString w)

fun assertBool _ true = ()
  | assertBool "" false = raise ExnStatus (Failure NoReason)
  | assertBool msg false = raise ExnStatus (Failure (Reason msg))

fun assertString "" = ()
  | assertString msg = raise ExnStatus (Failure (Reason msg))

fun exnToStatus (ExnStatus r) = r
  | exnToStatus e = Failure (Exception (NONE, e))

fun todo () = raise ExnStatus (Todo NONE)
fun todoWith reason = raise ExnStatus (Todo (SOME reason))

fun skip () = raise ExnStatus (Skip NONE)
fun skipWith reason = raise ExnStatus (Skip (SOME reason))

fun aroundWith action =
  let
    fun withHook (TestArgs {hook}) =
      TestArgs {hook = hook o action}
    fun walk (Test {desc, run}) =
          Test {desc = desc, run = run o withHook}
      | walk (Group (name, tests)) =
          Group (name, List.map walk tests)
  in
    walk
  end

fun around action = aroundWith (fn k => fn _ => action k)
fun beforeWith action = aroundWith (fn k => k o action)
fun before action = around (fn k => k (action ()))
fun after action = aroundWith (fn k => fn x =>
  let
    val y = k x
      handle e => (action x; raise e)
  in
    action x;
    y
  end)

fun it desc p = Test {
    desc = desc,
    run = fn TestArgs {hook, ...} =>
      
      let
        val s = ref Success
      in
        hook (fn x => (p x handle e => s := exnToStatus e));
        Result ("", !s)
      end
  }

fun numberOfTests tree =
  let
    fun go (Test _, n) = n + 1
      | go (Group (_, tests), n) = List.foldl go n tests
  in
    go (tree, 0)
  end

fun runTAP (strm, tree) =
  let
    val arg = TestArgs {
        hook = fn k => k ()
      }
    val escape = String.translate
      (fn #"#" => "\\#"
        | #"\\" => "\\\\"
        | c => String.str c)
    fun output s = TextIO.output (strm, s)
    fun outputDesc desc =
      if String.size desc = 0
        then ()
        else (output " - "; output (escape desc))
    fun outputPragma (pragma, reason) = (
      output " # ";
      output pragma;
      case reason of
          NONE => ()
        | SOME s => (output " "; output s))
    fun outputReason NoReason = ()
      | outputReason (Reason str) =
          let
            val yaml = [("label", YAMLish.SCALAR str)]
          in
            YAMLish.output (2, strm, YAMLish.MAP yaml)
          end
      | outputReason (Expected {message, expected, actual}) =
          let
            val yaml = [("actual", YAMLish.SCALAR actual)]
            val yaml = ("expected", YAMLish.SCALAR expected) :: yaml
            val yaml = case message of
                          NONE => yaml
                        | SOME lbl => ("label", YAMLish.SCALAR lbl) :: yaml
          in
            YAMLish.output (2, strm, YAMLish.MAP yaml)
          end
      | outputReason (Exception (reason, exn)) =
          let
            val yaml = [("exnMessage", YAMLish.SCALAR (exnMessage exn))]
            val yaml = ("exnName", YAMLish.SCALAR (exnName exn)) :: yaml
            val yaml = case reason of
                          NONE => yaml
                        | SOME lbl => ("label", YAMLish.SCALAR lbl) :: yaml
          in
            YAMLish.output (2, strm, YAMLish.MAP yaml)
          end
    fun runItem (path, n, {desc, run}) =
      let
        fun jsonString s =
          "\"" ^ String.toString s ^ "\""
        (* val path = String.concatWithMap ", " jsonString path *)
        val Result (info, status) = run arg
      in
        case status of
            Success => (
              output "ok ";
              output (Int.toString n);
              outputDesc desc;
              output "\n")
          | Todo reason => (
              output "ok ";
              output (Int.toString n);
              outputDesc desc;
              outputPragma ("TODO", reason);
              output "\n")
          | Skip reason => (
              output "ok ";
              output (Int.toString n);
              outputDesc desc;
              outputPragma ("SKIP", reason);
              output "\n")
          | Failure reason => (
              output "not ok ";
              output (Int.toString n);
              outputDesc desc;
              output "\n";
              outputReason reason)
      end
    fun walk path (Test item, n) =
          (runItem (path, n, item); n + 1)
      | walk path (Group ("", tests), n) =
          List.foldl (walk path) n tests
      | walk path (Group (name, tests), n) =
          let
            val depth = Int.toString (List.length path + 1)
            val () = output "# "
            val () = output name
            val () = output " {{{"
            val () = output depth
            val () = output "\n"
            val n = List.foldl (walk (name :: path)) n tests
            val () = output "# }}}"
            val () = output depth
            val () = output "\n"
          in
            n
          end
    val n = numberOfTests tree
  in
    (* TODO: Upgrade to TAP 14 *)
    output "TAP version 13\n";
    output "1..";
    output (Int.toString n);
    output "\n";
    ignore (walk [] (tree, 0));
    output "\n"
  end

fun runTAPHarness (harness, args, tree) =
  let
    val (outstream, instream, pid) = Helpers.spawnProcess (harness, args)
  in
    runTAP (outstream, tree);
    TextIO.closeOut outstream;
    TextIO.print (TextIO.inputAll instream);
    Helpers.wait pid;
    TextIO.print "\n"
  end

fun describe label tests = Group (label, tests)
val context = describe
val specify = it
fun xdescribe label spec = before skip (describe label spec)
val xcontext = xdescribe
fun xit label action = before skip (it label action)
val xspecify = xit

end
