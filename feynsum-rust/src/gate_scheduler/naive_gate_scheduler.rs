use super::GateScheduler;

pub struct NaiveGateScheduler {
    pub next: usize,
    pub num_gates: usize,
}

impl GateScheduler for NaiveGateScheduler {
    fn pick_next_gates(&mut self) -> Vec<usize> {
        if self.next >= self.num_gates {
            vec![]
        } else {
            let gate_idx = self.next;
            self.next += 1;
            vec![gate_idx]
        }
    }
}

impl NaiveGateScheduler {
    pub fn new(num_gates: usize) -> Self {
        Self { next: 0, num_gates }
    }
}
