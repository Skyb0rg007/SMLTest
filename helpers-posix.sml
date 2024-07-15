
structure Helpers : sig

type process

val wait : process -> unit
val spawnProcess : string * string list -> TextIO.outstream * TextIO.instream * process

end = struct

type process = Posix.Process.pid

fun wait pid =
  ignore (Posix.Process.waitpid (Posix.Process.W_CHILD pid, []))

fun spawnProcess (path, args) =
  let
    val {infd = readIn, outfd = writeIn} = Posix.IO.pipe ()
    val {infd = readOut, outfd = writeOut} = Posix.IO.pipe ()
    fun child () = (
      Posix.IO.dup2 {old = readIn, new = Posix.FileSys.stdin};
      Posix.IO.dup2 {old = writeOut, new = Posix.FileSys.stdout};
      Posix.IO.dup2 {old = writeOut, new = Posix.FileSys.stderr};
      Posix.IO.close readIn; Posix.IO.close writeIn;
      Posix.IO.close readOut; Posix.IO.close writeOut;
      Posix.Process.execp (path, path :: args)
      handle OS.SysErr (err, _) => OS.Process.exit OS.Process.failure)
    fun parent () =
      let
        val () = Posix.IO.close readIn
        val () = Posix.IO.close writeOut
        val writer = Posix.IO.mkTextWriter {
            fd = writeIn,
            name = "",
            appendMode = false,
            initBlkMode = false,
            chunkSize = 4096
          }
        val reader = Posix.IO.mkTextReader {
            fd = readOut,
            name = "",
            initBlkMode = false
          }
        val outstream = TextIO.StreamIO.mkOutstream (writer, IO.BLOCK_BUF)
        val instream = TextIO.StreamIO.mkInstream (reader, "")
      in
        (TextIO.mkOutstream outstream, TextIO.mkInstream instream)
      end
  in
    case Posix.Process.fork () of
        NONE => child ()
      | SOME pid =>
          let
            val (outstream, instream) = parent ()
          in
            (outstream, instream, pid)
          end
  end

end
