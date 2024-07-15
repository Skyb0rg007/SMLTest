
structure YAMLish =
struct

datatype t =
    SCALAR of string
  | ARRAY of t list
  | MAP of (string * t) list

fun escapeChar #"\000" = "\\z"
  | escapeChar #"\a" = "\\a"
  | escapeChar #"\t" = "\\t"
  | escapeChar #"\n" = "\\n"
  | escapeChar #"\f" = "\\f"
  | escapeChar #"\r" = "\\r"
  | escapeChar #"\027" = "\\e"
  | escapeChar #"\v" = "\\v"
  | escapeChar #"\\" = "\\\\"
  | escapeChar #"\"" = "\\\""
  | escapeChar c =
      let
        val n = Char.ord c
      in
        if n < 32
          then "\\x" ^ StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX n)
          else String.str c
      end

fun spaces n = CharVector.tabulate (n, fn _ => #" ")

fun isSpecialChar c = c < #" " orelse c = #"\127" orelse c = #"\""

fun outputScalar (key, stream, str) =
  if CharVector.exists isSpecialChar str
     orelse (key andalso String.size str > 0 andalso Char.isSpace (String.sub (str, 0)))
    then (TextIO.output1 (stream, #"\"");
          TextIO.output (stream, String.translate escapeChar str);
          TextIO.output1 (stream, #"\""))
  else if String.size str = 0 orelse CharVector.exists (fn c => c = #"'") str
    then (TextIO.output1 (stream, #"'");
          CharVector.app
            (fn #"'" => TextIO.output (stream, "''")
              | c => TextIO.output (stream, escapeChar c))
            str;
          TextIO.output1 (stream, #"'"))
  else TextIO.output (stream, str)

fun outputObject (indent, stream, SCALAR str) = (
      TextIO.output1 (stream, #" ");
      outputScalar (false, stream, str);
      TextIO.output1 (stream, #"\n"))
  | outputObject (indent, stream, ARRAY []) =
      TextIO.output (stream, " []\n")
  | outputObject (indent, stream, ARRAY arr) =
      let
        val pad = spaces indent
      in
        TextIO.output1 (stream, #"\n");
        List.app
          (fn x => (
            TextIO.output (stream, pad);
            TextIO.output1 (stream, #"-");
            outputObject (indent + 1, stream, x)))
          arr
      end
  | outputObject (indent, stream, MAP []) =
      TextIO.output (stream, " {}\n")
  | outputObject (indent, stream, MAP kvs) =
      let
        val pad = spaces indent
      in
        TextIO.output1 (stream, #"\n");
        List.app
          (fn (k, v) => (
            TextIO.output (stream, pad);
            outputScalar (true, stream, k);
            TextIO.output1 (stream, #":");
            outputObject (indent + 1, stream, v)))
          kvs
      end

fun output (indent, stream, obj) =
  let
    val pad = spaces indent
  in
    TextIO.output (stream, pad);
    TextIO.output (stream, "---");
    outputObject (indent, stream, obj);
    TextIO.output (stream, pad);
    TextIO.output (stream, "...\n")
  end

end
