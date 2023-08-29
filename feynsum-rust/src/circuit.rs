mod gate;

pub use gate::Gate;

pub struct Circuit {
    num_cubits: u32,
    gates: Vec<Gate>,
}
