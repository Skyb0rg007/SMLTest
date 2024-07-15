
structure Helpers : sig

type process

val wait : process -> unit
val spawnProcess : string * string list -> TextIO.outstream * TextIO.instream * process

end = struct

type process = (TextIO.instream, TextIO.outstream) Windows.proc

fun wait pid =
  ignore (Windows.reap pid)

fun isSpecial c =
  CharVector.exists (fn c' => c = c') " \t\n\v\""

fun escape str =
  if CharVector.exists isSpecial str
    then
      let
        val arr = CharArray.array (2 + 2 * String.size str, #"\\")
        fun f (#"\\", (pos, bs)) =
              (pos, bs + 1)
          | f (#"\"", (pos, bs)) = (
              CharArray.update (arr, pos + 2 * bs + 1, #"\"");
              (pos + 2 * bs + 2, 0))
          | f (c, (pos, bs)) = (
              CharArray.update (arr, pos + bs, c);
              (pos + bs + 1, 0))
        val () = CharArray.update (arr, 0, #"\"")
        val (pos, backslashes) = CharVector.foldl f (1, 0) str
        val pos = pos + 2 * backslashes
        val () = CharArray.update (arr, pos, #"\"")
        val len = pos + 1
      in
        CharArraySlice.vector (CharArraySlice.slice (arr, 0, SOME len))
      end
    else str

fun spawnProcess (path, args) =
  let
    val cmdline = String.concatWithMap " " escape args
    val proc = Windows.execute (path, cmdline)
    val instream = Windows.textInstreamOf proc
    val outstream = Windows.textOutstreamOf proc
  in
    (outstream, instream, proc)
  end

end
