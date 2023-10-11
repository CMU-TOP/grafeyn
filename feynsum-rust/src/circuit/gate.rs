use log::debug;

use crate::{
    types::{constants, BasisIdx, Complex, QubitIndex, Real},
    utility,
};

#[derive(Debug)]
pub enum PushApplyOutput {
    Nonbranching(BasisIdx, Complex),                     // bidx, weight
    Branching((BasisIdx, Complex), (BasisIdx, Complex)), // (bidx, weight), (bidx, weight)
}

#[derive(Debug)]
pub enum PullApplyOutput {
    Nonbranching(BasisIdx, Complex), // neighbor, multiplier
    Branching((BasisIdx, Complex), (BasisIdx, Complex)), // (neighbor, multiplier), (neighbor, multiplier)
}

#[derive(Debug, Eq, PartialEq)]
enum BranchingType {
    Nonbranching,
    Branching,
    MaybeBranching,
}

#[derive(Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum GateDefn {
    CCX {
        control1: QubitIndex,
        control2: QubitIndex,
        target: QubitIndex,
    },
    CPhase {
        control: QubitIndex,
        target: QubitIndex,
        rot: Real,
    },
    CSwap {
        control: QubitIndex,
        target1: QubitIndex,
        target2: QubitIndex,
    },
    CX {
        control: QubitIndex,
        target: QubitIndex,
    },
    CZ {
        control: QubitIndex,
        target: QubitIndex,
    },
    FSim {
        left: QubitIndex,
        right: QubitIndex,
        theta: Real,
        phi: Real,
    },
    Hadamard(QubitIndex),
    PauliY(QubitIndex),
    PauliZ(QubitIndex),
    Phase {
        rot: Real,
        target: QubitIndex,
    },
    RX {
        rot: Real,
        target: QubitIndex,
    },
    RY {
        rot: Real,
        target: QubitIndex,
    },
    RZ {
        rot: Real,
        target: QubitIndex,
    },
    S(QubitIndex),
    Sdg(QubitIndex),
    SqrtW(QubitIndex),
    SqrtX(QubitIndex),
    SqrtXdg(QubitIndex),
    Swap {
        target1: QubitIndex,
        target2: QubitIndex,
    },
    T(QubitIndex),
    Tdg(QubitIndex),
    U {
        target: QubitIndex,
        theta: Real,
        phi: Real,
        lambda: Real,
    },
    X(QubitIndex),
    Other {
        name: String,
        params: Vec<Real>,
        args: Vec<QubitIndex>,
    },
}

pub trait PushApplicable {
    fn push_apply(&self, bidx: BasisIdx, weight: Complex) -> PushApplyOutput;
}

pub trait PullApplicable {
    fn pull_apply(&self, bidx: BasisIdx) -> PullApplyOutput;
}

#[derive(Debug)]
pub struct Gate {
    defn: GateDefn,
    pub touches: Vec<QubitIndex>,
}

impl Gate {
    pub fn new(defn: GateDefn) -> Self {
        let touches = match &defn {
            &GateDefn::Hadamard(qi)
            | &GateDefn::PauliY(qi)
            | &GateDefn::PauliZ(qi)
            | &GateDefn::Phase { target: qi, .. }
            | &GateDefn::S(qi)
            | &GateDefn::Sdg(qi)
            | &GateDefn::SqrtW(qi)
            | &GateDefn::SqrtX(qi)
            | &GateDefn::SqrtXdg(qi)
            | &GateDefn::T(qi)
            | &GateDefn::Tdg(qi)
            | &GateDefn::X(qi) => vec![qi],
            &GateDefn::CPhase {
                control, target, ..
            }
            | &GateDefn::CZ { control, target }
            | &GateDefn::CX { control, target } => vec![control, target],
            &GateDefn::CCX {
                control1,
                control2,
                target,
            } => vec![control1, control2, target],
            &GateDefn::FSim { left, right, .. } => vec![left, right],
            &GateDefn::RX { target, .. }
            | &GateDefn::RY { target, .. }
            | &GateDefn::RZ { target, .. } => vec![target],
            &GateDefn::CSwap {
                control,
                target1,
                target2,
            } => vec![control, target1, target2],
            &GateDefn::Swap { target1, target2 } => vec![target1, target2],
            &GateDefn::U { target, .. } => vec![target],
            &GateDefn::Other { .. } => vec![],
        };
        Self { defn, touches }
    }

    fn branching_type(&self) -> BranchingType {
        match self.defn {
            GateDefn::CCX { .. }
            | GateDefn::CPhase { .. }
            | GateDefn::CSwap { .. }
            | GateDefn::CX { .. }
            | GateDefn::CZ { .. }
            | GateDefn::PauliY(_)
            | GateDefn::PauliZ(_)
            | GateDefn::Phase { .. }
            | GateDefn::RZ { .. }
            | GateDefn::S(_)
            | GateDefn::Sdg(_)
            | GateDefn::Swap { .. }
            | GateDefn::T(_)
            | GateDefn::Tdg(_)
            | GateDefn::X(_) => BranchingType::Nonbranching,
            GateDefn::Hadamard(_)
            | GateDefn::RY { .. }
            | GateDefn::SqrtW(_)
            | GateDefn::SqrtX(_)
            | GateDefn::SqrtXdg(_) => BranchingType::Branching,
            GateDefn::FSim { .. } | GateDefn::RX { .. } | GateDefn::U { .. } => {
                BranchingType::MaybeBranching
            }
            GateDefn::Other { .. } => unimplemented!(),
        }
    }

    pub fn is_branching(&self) -> bool {
        self.branching_type() != BranchingType::Nonbranching
        // NOTE: We assume MaybeBranching as Branching
    }

    // TODO: refactor to make this always consistent with pull_apply
    pub fn is_pullable(&self) -> bool {
        match self.defn {
            GateDefn::CX { .. }
            | GateDefn::CZ { .. }
            | GateDefn::Hadamard(_)
            | GateDefn::RX { .. }
            | GateDefn::RY { .. }
            | GateDefn::RZ { .. }
            | GateDefn::SqrtX(_)
            | GateDefn::SqrtXdg(_)
            | GateDefn::Swap { .. }
            | GateDefn::U { .. } => true,
            GateDefn::CCX { .. }
            | GateDefn::CPhase { .. }
            | GateDefn::CSwap { .. }
            | GateDefn::FSim { .. }
            | GateDefn::Phase { .. }
            | GateDefn::PauliY(_)
            | GateDefn::PauliZ(_)
            | GateDefn::S(_)
            | GateDefn::Sdg(_)
            | GateDefn::SqrtW(_)
            | GateDefn::T(_)
            | GateDefn::Tdg(_)
            | GateDefn::X(_) => {
                matches!(
                    (self.touches.len(), self.branching_type()),
                    // We have not yet implemented pull action for other gates
                    (1, BranchingType::Nonbranching)
                        | (2, BranchingType::Nonbranching)
                        | (1, BranchingType::Branching)
                )
            }
            GateDefn::Other { .. } => false,
        }
    }
}

impl PushApplicable for Gate {
    fn push_apply(&self, bidx: BasisIdx, weight: Complex) -> PushApplyOutput {
        match self.defn {
            GateDefn::CCX {
                control1,
                control2,
                target,
            } => {
                let new_bidx = if bidx.get(control1) && bidx.get(control2) {
                    bidx.flip(target)
                } else {
                    bidx
                };
                PushApplyOutput::Nonbranching(new_bidx, weight)
            }
            GateDefn::CPhase {
                control,
                target,
                rot,
            } => {
                let new_weight = if bidx.get(control) && bidx.get(target) {
                    weight * Complex::new(rot.cos(), rot.sin())
                } else {
                    weight
                };
                PushApplyOutput::Nonbranching(bidx, new_weight)
            }
            GateDefn::CSwap {
                control,
                target1,
                target2,
            } => {
                let new_bidx = if bidx.get(control) {
                    bidx.swap(target1, target2)
                } else {
                    bidx
                };
                PushApplyOutput::Nonbranching(new_bidx, weight)
            }
            GateDefn::CX { control, target } => {
                let new_bidx = if bidx.get(control) {
                    bidx.flip(target)
                } else {
                    bidx
                };
                PushApplyOutput::Nonbranching(new_bidx, weight)
            }
            GateDefn::CZ { control, target } => {
                let new_weight = if bidx.get(control) && bidx.get(target) {
                    -weight
                } else {
                    weight
                };
                PushApplyOutput::Nonbranching(bidx, new_weight)
            }
            GateDefn::FSim {
                left,
                right,
                theta,
                phi,
            } => match (bidx.get(left), bidx.get(right)) {
                (false, false) => PushApplyOutput::Nonbranching(bidx, weight),
                (true, true) => {
                    PushApplyOutput::Nonbranching(bidx, weight * Complex::new(phi.cos(), phi.sin()))
                }
                _ => {
                    let bidx0 = bidx.unset(left).set(right);
                    let bidx1 = bidx.unset(right).set(left);
                    let weight_a = weight * Complex::new(theta.cos(), 0.0);
                    let weight_b = weight * Complex::new(0.0, -theta.sin());

                    if bidx.get(left) {
                        PushApplyOutput::Branching((bidx0, weight_b), (bidx1, weight_a))
                    } else {
                        PushApplyOutput::Branching((bidx0, weight_a), (bidx1, weight_b))
                    }
                }
            },
            GateDefn::Hadamard(qi) => {
                let bidx0 = bidx.unset(qi);
                let bidx1 = bidx.set(qi);

                let new_weight = weight * constants::RECP_SQRT_2;

                if bidx.get(qi) {
                    PushApplyOutput::Branching((bidx0, new_weight), (bidx1, -new_weight))
                } else {
                    PushApplyOutput::Branching((bidx0, new_weight), (bidx1, new_weight))
                }
            }
            GateDefn::Phase { rot, target } => {
                let new_weight = if bidx.get(target) {
                    weight * Complex::new(rot.cos(), rot.sin())
                } else {
                    weight
                };
                PushApplyOutput::Nonbranching(bidx, new_weight)
            }
            GateDefn::RX { rot, target } => {
                let cos = Complex::new((rot / 2.0).cos(), 0.0);
                let sin = Complex::new((rot / 2.0).sin(), 0.0);
                let a = cos;
                let b = sin * Complex::new(0.0, -1.0);
                let c = b;
                let d = a;

                single_qubit_unitary_push(bidx, weight, target, a, b, c, d)
            }
            GateDefn::RY { rot, target } => {
                let bidx0 = bidx.unset(target);
                let bidx1 = bidx.set(target);

                if bidx.get(target) {
                    PushApplyOutput::Branching(
                        (bidx0, weight * -(rot / 2.0).sin()),
                        (bidx1, weight * (rot / 2.0).cos()),
                    )
                } else {
                    PushApplyOutput::Branching(
                        (bidx0, weight * (rot / 2.0).cos()),
                        (bidx1, weight * (rot / 2.0).sin()),
                    )
                }
            }
            GateDefn::RZ { rot, target } => {
                let new_weight = if bidx.get(target) {
                    weight * Complex::new((rot / 2.0).cos(), (rot / 2.0).sin())
                } else {
                    weight * Complex::new((rot / 2.0).cos(), -(rot / 2.0).sin())
                };

                PushApplyOutput::Nonbranching(bidx, new_weight)
            }
            GateDefn::S(qi) => {
                let new_weight = if bidx.get(qi) {
                    weight * Complex::new(0.0, 1.0)
                } else {
                    weight
                };
                PushApplyOutput::Nonbranching(bidx, new_weight)
            }
            GateDefn::Sdg(qi) => {
                let new_weight = if bidx.get(qi) {
                    weight * Complex::new(0.0, -1.0)
                } else {
                    weight
                };
                PushApplyOutput::Nonbranching(bidx, new_weight)
            }
            GateDefn::SqrtW(qi) => {
                let bidx0 = bidx.unset(qi);
                let bidx1 = bidx.set(qi);

                if bidx.get(qi) {
                    PushApplyOutput::Branching(
                        (bidx0, weight * Complex::new(0.0, constants::RECP_SQRT_2)),
                        (bidx1, weight * Complex::new(-0.5, -0.5)),
                    )
                } else {
                    PushApplyOutput::Branching(
                        (bidx0, weight * Complex::new(-0.5, -0.5)),
                        (bidx1, weight * Complex::new(-constants::RECP_SQRT_2, 0.0)),
                    )
                }
            }
            GateDefn::Swap { target1, target2 } => {
                let new_bidx = bidx.swap(target1, target2);
                PushApplyOutput::Nonbranching(new_bidx, weight)
            }
            GateDefn::SqrtX(qi) => {
                let bidx0 = bidx.unset(qi);
                let bidx1 = bidx.set(qi);

                let weight_a = weight * Complex::new(0.5, 0.5);
                let weight_b = weight * Complex::new(0.5, -0.5);

                if bidx.get(qi) {
                    PushApplyOutput::Branching((bidx0, weight_b), (bidx1, weight_a))
                } else {
                    PushApplyOutput::Branching((bidx0, weight_a), (bidx1, weight_b))
                }
            }
            GateDefn::SqrtXdg(qi) => {
                let bidx0 = bidx.unset(qi);
                let bidx1 = bidx.set(qi);

                let weight_a = weight * Complex::new(0.5, 0.5);
                let weight_b = weight * Complex::new(0.5, -0.5);

                if bidx.get(qi) {
                    PushApplyOutput::Branching((bidx0, weight_a), (bidx1, weight_b))
                } else {
                    PushApplyOutput::Branching((bidx0, weight_b), (bidx1, weight_a))
                }
            }
            GateDefn::T(qi) => {
                let new_weight = if bidx.get(qi) {
                    weight * Complex::new(constants::RECP_SQRT_2, constants::RECP_SQRT_2)
                } else {
                    weight
                };
                PushApplyOutput::Nonbranching(bidx, new_weight)
            }
            GateDefn::Tdg(qi) => {
                Complex::new(constants::RECP_SQRT_2, -constants::RECP_SQRT_2);

                let new_weight = if bidx.get(qi) {
                    weight * Complex::new(constants::RECP_SQRT_2, -constants::RECP_SQRT_2)
                } else {
                    weight
                };
                PushApplyOutput::Nonbranching(bidx, new_weight)
            }
            GateDefn::U {
                target,
                theta,
                phi,
                lambda,
            } => {
                let cos = Complex::new((theta / 2.0).cos(), 0.0);
                let sin = Complex::new((theta / 2.0).sin(), 0.0);

                let a = cos;
                let b = -sin * Complex::new(lambda.cos(), lambda.sin());
                let c = sin * Complex::new(phi.cos(), phi.sin());
                let d = cos * Complex::new((phi + lambda).cos(), (phi + lambda).sin());

                single_qubit_unitary_push(bidx, weight, target, a, b, c, d)
            }
            GateDefn::PauliY(qi) => {
                let new_bidx = bidx.flip(qi);
                let new_weight = if bidx.get(qi) {
                    weight * Complex::new(0.0, -1.0)
                } else {
                    weight * Complex::new(0.0, 1.0)
                };
                PushApplyOutput::Nonbranching(new_bidx, new_weight)
            }
            GateDefn::PauliZ(qi) => {
                let new_weight = if bidx.get(qi) { -weight } else { weight };
                PushApplyOutput::Nonbranching(bidx, new_weight)
            }
            GateDefn::X(qi) => {
                let new_bidx = bidx.flip(qi);
                PushApplyOutput::Nonbranching(new_bidx, weight)
            }
            GateDefn::Other { .. } => unimplemented!(),
        }
    }
}

fn single_qubit_unitary_push(
    bidx: BasisIdx,
    weight: Complex,
    target: QubitIndex,
    a: Complex,
    b: Complex,
    c: Complex,
    d: Complex,
) -> PushApplyOutput {
    assert!(!(utility::is_zero(a) && utility::is_zero(b)));
    assert!(!(utility::is_zero(c) && utility::is_zero(d)));

    if utility::is_zero(a) && utility::is_zero(d) {
        let new_bidx = bidx.flip(target);
        let new_weight = if bidx.get(target) {
            b * weight
        } else {
            c * weight
        };
        PushApplyOutput::Nonbranching(new_bidx, new_weight)
    } else if utility::is_zero(c) && utility::is_zero(b) {
        let new_weight = if bidx.get(target) {
            d * weight
        } else {
            a * weight
        };
        PushApplyOutput::Nonbranching(bidx, new_weight)
    } else {
        let bidx0 = bidx.unset(target);
        let bidx1 = bidx.set(target);
        let (mult0, mult1) = if bidx.get(target) { (b, d) } else { (a, c) };
        PushApplyOutput::Branching((bidx0, mult0 * weight), (bidx1, mult1 * weight))
    }
}

fn single_qubit_unitary_pull(
    bidx: BasisIdx,
    target: QubitIndex,
    a: Complex,
    b: Complex,
    c: Complex,
    d: Complex,
) -> PullApplyOutput {
    assert!(!(utility::is_zero(a) && utility::is_zero(b)));
    assert!(!(utility::is_zero(c) && utility::is_zero(d)));

    if utility::is_zero(a) && utility::is_zero(d) {
        let neighbor = bidx.flip(target);
        let multiplier = if bidx.get(target) { c } else { b };
        PullApplyOutput::Nonbranching(neighbor, multiplier)
    } else if utility::is_zero(c) && utility::is_zero(b) {
        let multiplier = if bidx.get(target) { d } else { a };
        PullApplyOutput::Nonbranching(bidx, multiplier)
    } else {
        let bidx0 = bidx.unset(target);
        let bidx1 = bidx.set(target);

        if bidx.get(target) {
            PullApplyOutput::Branching((bidx0, c), (bidx1, d))
        } else {
            PullApplyOutput::Branching((bidx0, a), (bidx1, b))
        }
    }
}

// NOTE: We may store partial computation of this
// to further optimize performance
macro_rules! push_to_pull {
    ($self:ident, $bidx:ident) => {{
        let touches = &$self.touches;
        debug!(
            "pulling touches: {}, {:?} gate: {:?}",
            touches.len(),
            $self.branching_type(),
            $self
        );
        match (touches.len(), $self.branching_type()) {
            (1, BranchingType::Nonbranching) => {
                let qi = touches[0];

                let (b0, m0) = match $self.push_apply(BasisIdx::zeros(), Complex::new(1.0, 0.0)) {
                    PushApplyOutput::Nonbranching(bidx, multiplier) => (bidx, multiplier),
                    _ => unreachable!("push_apply(BasisIdx::zeros(), Complex::new(1.0,0.0)) must return Nonbranching"),
                };

                let (_bidx2, m1) =
                    match $self.push_apply(BasisIdx::zeros().set(qi), Complex::new(1.0, 0.0)) {
                        PushApplyOutput::Nonbranching(bidx, multiplier) => (bidx, multiplier),
                    _ => unreachable!("push_apply(BasisIdx::zeros().set(qi), Complex::new(1.0,0.0)) must return Nonbranching"),
                    };

                if b0 == BasisIdx::zeros() {
                    PullApplyOutput::Nonbranching($bidx, if $bidx.get(qi) { m1 } else { m0 })
                } else {
                    PullApplyOutput::Nonbranching(
                        $bidx.flip(qi),
                        if $bidx.get(qi) { m0 } else { m1 },
                    )
                }
            }
            (2, BranchingType::Nonbranching) => {
                let qi = touches[0];
                let qj = touches[1];

                let a00 = BasisIdx::zeros();
                let a01 = BasisIdx::zeros().set(qj);
                let a10 = BasisIdx::zeros().set(qi);
                let a11 = BasisIdx::zeros().set(qi).set(qj);

                let (b00, m00) = match $self.push_apply(a00, Complex::new(1.0, 0.0)) {
                    PushApplyOutput::Nonbranching(bidx, multiplier) => (bidx, multiplier),
                    _ => unreachable!("push_apply(BasisIdx::zeros(), Complex::new(1.0,0.0)) must return Nonbranching"),
                };

                let (b01, m01) =
                    match $self.push_apply(a01, Complex::new(1.0, 0.0)) {
                        PushApplyOutput::Nonbranching(bidx, multiplier) => (bidx, multiplier),
                        _ => unreachable!("push_apply(BasisIdx::zeros().set(qj), Complex::new(1.0,0.0)) must return Nonbranching"),
                    };

                let (b10, m10) =
                    match $self.push_apply(a10, Complex::new(1.0, 0.0)) {
                        PushApplyOutput::Nonbranching(bidx, multiplier) => (bidx, multiplier),
                        _ => unreachable!("push_apply(BasisIdx::zeros().set(qi), Complex::new(1.0,0.0)) must return Nonbranching"),
                    };

                let (b11, m11) = match $self
                    .push_apply(a11, Complex::new(1.0, 0.0))
                {
                    PushApplyOutput::Nonbranching(bidx, multiplier) => (bidx, multiplier),
                    _ => unreachable!("push_apply(BasisIdx::zeros().set(qi).set(qj), Complex::new(1.0,0.0)) must return Nonbranching"),
                };

                let apply_match = |left: bool, right: bool, bb: &BasisIdx| -> bool {
                        left == bb.get(qi) && right == bb.get(qj)
                    };
                    let find = |left: bool, right: bool| -> (BasisIdx, Complex) {
                        if apply_match(left, right, &b00) {
                            (a00, m00.clone())
                        } else if apply_match(left, right, &b01) {
                            (a01, m01.clone())
                        } else if apply_match(left, right, &b10) {
                            (a10, m10.clone())
                        } else if apply_match(left, right, &b11) {
                            (a11, m11.clone())
                        } else {
                            unreachable!("apply_match must return true for one of the basis")
                        }
                    };


                let align_with = |bb: &BasisIdx, bidx: &BasisIdx| -> BasisIdx {
                    match (bb.get(qi), bb.get(qj)) {
                        (true, true) => bidx.set(qi).set(qj),
                        (true, false) => bidx.set(qi).unset(qj),
                        (false, true) => bidx.unset(qi).set(qj),
                        (false, false) => bidx.unset(qi).unset(qj),
                    }
                };

                match ($bidx.get(qi), $bidx.get(qj)) {
                    (true, true) => {
                        PullApplyOutput::Nonbranching(align_with(&b11, &$bidx), m11)
                    }
                    (true, false) => {
                        PullApplyOutput::Nonbranching(align_with(&b10, &$bidx), m10)
                    }
                    (false, true) => {
                        PullApplyOutput::Nonbranching(align_with(&b01, &$bidx), m01)
                    }
                    (false, false) => {
                        PullApplyOutput::Nonbranching(align_with(&b00, &$bidx), m00)
                    }
                }
            }
            (1, BranchingType::Branching) => {
                let qi = touches[0];

                let PushApplyOutput::Branching((b00, m00), (b01, m01)) =
                    $self.push_apply(BasisIdx::zeros(), Complex::new(1.0, 0.0))
                else {
                    unreachable!("push_apply(BasisIdx::zeros(), Complex::new(1.0,0.0)) must return Branching")
                };

                let PushApplyOutput::Branching((b10, m10), (b11, m11)) =
                    $self.push_apply(BasisIdx::zeros().set(qi), Complex::new(1.0, 0.0))
                else {
                    unreachable!("push_apply(BasisIdx::zeros(), Complex::new(1.0,0.0)) must return Branching")
                };

                let ((b00, m00), (b01, m01)) = if b00.get(qi) {
                    ((b01, m01), (b00, m00))
                } else {
                    ((b00, m00), (b01, m01))
                };

                let ((b10, m10), (b11, m11)) = if b10.get(qi) {
                    ((b11, m11), (b10, m10))
                } else {
                    ((b10, m10), (b11, m11))
                };

                assert!(!b00.get(qi) && !b10.get(qi) && b01.get(qi) && b11.get(qi));

                if $bidx.get(qi) {
                    PullApplyOutput::Branching(($bidx.unset(qi), m01), ($bidx, m11))
                } else {
                    let bidx2 = $bidx.set(qi);
                    PullApplyOutput::Branching(($bidx, m00), (bidx2, m10))
                }
            }

            _ => unimplemented!("pull action for {:?} not supported at this moment", $self),
        }
    }};
}

impl PullApplicable for Gate {
    fn pull_apply(&self, bidx: BasisIdx) -> PullApplyOutput {
        match self.defn {
            GateDefn::CCX { .. }
            | GateDefn::CPhase { .. }
            | GateDefn::CSwap { .. }
            | GateDefn::Swap { .. }
            | GateDefn::FSim { .. }
            | GateDefn::PauliY(_)
            | GateDefn::PauliZ(_)
            | GateDefn::S(_)
            | GateDefn::Sdg(_)
            | GateDefn::SqrtW(_)
            | GateDefn::T(_)
            | GateDefn::Tdg(_)
            | GateDefn::X(_) => {
                assert!(self.is_pullable());
                push_to_pull!(self, bidx)
            }

            GateDefn::CX { control, target } => {
                if bidx.get(control) {
                    PullApplyOutput::Nonbranching(bidx.flip(target), Complex::new(1.0, 0.0))
                } else {
                    PullApplyOutput::Nonbranching(bidx, Complex::new(1.0, 0.0))
                }
            }
            GateDefn::CZ { control, target } => {
                if bidx.get(control) && bidx.get(target) {
                    PullApplyOutput::Nonbranching(bidx, Complex::new(-1.0, 0.0))
                } else {
                    PullApplyOutput::Nonbranching(bidx, Complex::new(1.0, 0.0))
                }
            }
            GateDefn::Hadamard(qi) => {
                let bidx0 = bidx.unset(qi);
                let bidx1 = bidx.set(qi);

                if bidx.get(qi) {
                    PullApplyOutput::Branching(
                        (bidx0, Complex::new(constants::RECP_SQRT_2, 0.0)),
                        (bidx1, Complex::new(-constants::RECP_SQRT_2, 0.0)),
                    )
                } else {
                    PullApplyOutput::Branching(
                        (bidx0, Complex::new(constants::RECP_SQRT_2, 0.0)),
                        (bidx1, Complex::new(constants::RECP_SQRT_2, 0.0)),
                    )
                }
            }
            GateDefn::Phase { rot, target } => {
                if bidx.get(target) {
                    PullApplyOutput::Nonbranching(bidx, Complex::new(rot.cos(), rot.sin()))
                } else {
                    PullApplyOutput::Nonbranching(bidx, Complex::new(1.0, 0.0))
                }
            }
            GateDefn::RX { rot, target } => {
                let cos = Complex::new((rot / 2.0).cos(), 0.0);
                let sin = Complex::new((rot / 2.0).sin(), 0.0);
                let a = cos;
                let b = sin * Complex::new(0.0, -1.0);
                let c = b;
                let d = a;

                single_qubit_unitary_pull(bidx, target, a, b, c, d)
            }
            GateDefn::RY { rot, target } => {
                let bidx0 = bidx.unset(target);
                let bidx1 = bidx.set(target);

                let cos = (rot / 2.0).cos();
                let sin = (rot / 2.0).sin();

                if bidx.get(target) {
                    PullApplyOutput::Branching(
                        (bidx0, Complex::new(sin, 0.0)),
                        (bidx1, Complex::new(cos, 0.0)),
                    )
                } else {
                    PullApplyOutput::Branching(
                        (bidx0, Complex::new(cos, 0.0)),
                        (bidx1, Complex::new(-sin, 0.0)),
                    )
                }
            }
            GateDefn::RZ { rot, target } => {
                let cos = (rot / 2.0).cos();
                let sin = (rot / 2.0).sin();
                if bidx.get(target) {
                    PullApplyOutput::Nonbranching(bidx, Complex::new(cos, sin))
                } else {
                    PullApplyOutput::Nonbranching(bidx, Complex::new(cos, -sin))
                }
            }
            GateDefn::SqrtX(qi) => {
                let bidx0 = bidx.unset(qi);
                let bidx1 = bidx.set(qi);

                if bidx.get(qi) {
                    PullApplyOutput::Branching(
                        (bidx0, Complex::new(0.5, -0.5)),
                        (bidx1, Complex::new(0.5, 0.5)),
                    )
                } else {
                    PullApplyOutput::Branching(
                        (bidx0, Complex::new(0.5, 0.5)),
                        (bidx1, Complex::new(0.5, -0.5)),
                    )
                }
            }
            GateDefn::SqrtXdg(qi) => {
                let bidx0 = bidx.unset(qi);
                let bidx1 = bidx.set(qi);

                if bidx.get(qi) {
                    PullApplyOutput::Branching(
                        (bidx0, Complex::new(0.5, 0.5)),
                        (bidx1, Complex::new(0.5, -0.5)),
                    )
                } else {
                    PullApplyOutput::Branching(
                        (bidx0, Complex::new(0.5, -0.5)),
                        (bidx1, Complex::new(0.5, 0.5)),
                    )
                }
            }
            GateDefn::U {
                target,
                theta,
                phi,
                lambda,
            } => {
                let cos = Complex::new((theta / 2.0).cos(), 0.0);
                let sin = Complex::new((theta / 2.0).sin(), 0.0);

                let a = cos;
                let b = -sin * Complex::new(lambda.cos(), lambda.sin());
                let c = sin * Complex::new(phi.cos(), phi.sin());
                let d = cos * Complex::new((phi + lambda).cos(), (phi + lambda).sin());

                assert!(!(utility::is_zero(a) && utility::is_zero(b)));
                assert!(!(utility::is_zero(c) && utility::is_zero(d)));

                single_qubit_unitary_pull(bidx, target, a, b, c, d)
            }
            GateDefn::Other { .. } => {
                unimplemented!()
            }
        }
    }
}
