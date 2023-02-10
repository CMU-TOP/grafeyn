structure ParseQASM:
sig
  (* (gate name, indices of qubit arguments) *)
  type gate = string * int Seq.t

  (* returns (numQubits, circuit) *)
  val readQASM: string -> int * gate Seq.t
end =
struct

  type gate = string * int Seq.t

  exception InvalidFormat

  (*
    fun writeToQASM f gs {perm, M} =
      let
        fun writeFile (f: string) (s: string) : unit =
            let val os = TextIO.openOut f
            in (TextIO.output(os,s); TextIO.closeOut os)
              handle X => (TextIO.closeOut os; raise X)
            end
  
        val header = "OPENQASM 2.0; \ninclude \"qelib1.inc\"; \nqreg q[1]; \n"
        fun write_gate i = (GateSet.label gs i) ^ " q[0];\n"
        val gate_s = Seq.reduce (fn (a, b) => a ^ b) "" (Seq.map write_gate perm)
      in
        writeFile f (header ^ gate_s)
      end
  *)

  fun readQASM path =
    let
      val chars = ReadFile.contentsSeq path
      fun split chars char =
        let
          fun isChar i =
            Seq.nth chars i = char
          val charPos = ArraySlice.full
            (SeqBasis.filter 10000 (0, Seq.length chars) (fn i => i) isChar)
          fun splitStart i =
            if i = 0 then 0 else 1 + Seq.nth charPos (i - 1)
          fun splitEnd i =
            if i = Seq.length charPos then Seq.length chars
            else Seq.nth charPos i
        in
          DelayedSeq.tabulate
            (fn i => Seq.subseq chars (splitStart i, splitEnd i - splitStart i))
            (Seq.length charPos)
        end
      val lines = split chars (#"\n")
      fun line i = DelayedSeq.nth lines i

      (* given a char sequence of the form {q[n]...}, it returns the integer n *)
      fun getqindex chars =
        case Parse.parseInt (DelayedSeq.nth (split (Seq.drop chars 2) (#"]")) 0) of
          SOME n => n
        | NONE => raise InvalidFormat

      (* from qreg q[n], retrieve n *)
      val nqubits = getqindex (Seq.drop (line 2) 5)
      val head_off = 3
      fun parseGateLine i =
        let
          val i = i + head_off
          val line_split = split (line i) (#" ")
          val gate = Parse.parseString (DelayedSeq.nth line_split 0)
          val numinputs = DelayedSeq.length line_split - 1
        in
          ( gate
          , Seq.tabulate (fn i => getqindex (DelayedSeq.nth line_split (1 + i)))
              numinputs
          )
        end

      val numLines = DelayedSeq.length lines
      val circuit = Seq.tabulate parseGateLine (numLines - head_off)
    in
      (nqubits, circuit)
    end
end
