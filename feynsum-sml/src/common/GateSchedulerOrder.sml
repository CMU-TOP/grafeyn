structure GateSchedulerOrder:
sig
  type gate_idx = int
  val mkScheduler: gate_idx Seq.t -> GateScheduler.t
end =
struct

  type qubit_idx = int
  type gate_idx = int

  fun mkScheduler (order: gate_idx Seq.t) args =
      let val i = ref 0
          val N = Seq.length order
      in
        fn () => if !i >= N then
                   Seq.empty ()
                 else
                   let val gi = !i in i := gi + 1; Seq.singleton gi end
      end
end
