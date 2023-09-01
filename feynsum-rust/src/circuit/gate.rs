use crate::types::{constants, BasisIdx, Complex, QubitIndex, Real};

#[derive(Debug)]
pub enum MaybeBranchingOutput {
    OuptutOne((BasisIdx, Complex)),
    OutputTwo((BasisIdx, Complex), (BasisIdx, Complex)),
}

#[derive(Debug)]
pub enum GateDefn {
    PauliY(QubitIndex),
    PauliZ(QubitIndex),
    Hadamard(QubitIndex),
    T(QubitIndex),
    SqrtX(QubitIndex),
    SqrtY(QubitIndex),
    SqrtW(QubitIndex),
    X(QubitIndex),
    CX {
        control: QubitIndex,
        target: QubitIndex,
    },
    CZ {
        control: QubitIndex,
        target: QubitIndex,
    },
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
    FSim {
        left: QubitIndex,
        right: QubitIndex,
        theta: Real,
        phi: Real,
    },
    RZ {
        rot: Real,
        target: QubitIndex,
    },
    RY {
        rot: Real,
        target: QubitIndex,
    },
    CSwap {
        control: QubitIndex,
        target1: QubitIndex,
        target2: QubitIndex,
    },
    U {
        target: QubitIndex,
        theta: Real,
        phi: Real,
        lambda: Real,
    },
    Other {
        name: String,
        params: Vec<Real>,
        args: Vec<QubitIndex>,
    },
}

impl GateDefn {
    // TODO: move bidx and avoid clone if possible
    pub fn apply(&self, bidx: &BasisIdx, weight: &Complex) -> MaybeBranchingOutput {
        match *self {
            Self::PauliY(qi) => {
                let new_bidx = bidx.flip(qi);
                let multipler = if bidx.get(qi) {
                    Complex::new(0.0, -1.0)
                } else {
                    Complex::new(0.0, 1.0)
                };
                let new_weight = weight * multipler;
                MaybeBranchingOutput::OuptutOne((new_bidx, new_weight))
            }
            Self::PauliZ(qi) => {
                let new_weight = if bidx.get(qi) {
                    weight * Complex::new(-1.0, 0.0)
                } else {
                    weight.clone()
                };
                MaybeBranchingOutput::OuptutOne((bidx.clone(), new_weight))
            }
            Self::Hadamard(qi) => {
                let crs2 = Complex::new(constants::RECP_SQRT_2, 0.0);
                let cnrs2 = Complex::new(-constants::RECP_SQRT_2, 0.0);

                let bidx1 = bidx.unset(qi);
                let bidx2 = bidx.set(qi);

                if bidx.get(qi) {
                    MaybeBranchingOutput::OutputTwo((bidx1, weight * crs2), (bidx2, weight * cnrs2))
                } else {
                    MaybeBranchingOutput::OutputTwo((bidx1, crs2), (bidx2, crs2))
                }
            }
            Self::T(qi) => {
                let mult = Complex::new(constants::RECP_SQRT_2, constants::RECP_SQRT_2);

                let new_weight = if bidx.get(qi) {
                    weight * mult
                } else {
                    weight.clone()
                };

                MaybeBranchingOutput::OuptutOne((bidx.clone(), new_weight))
            }
            Self::SqrtX(qi) => {
                let bidx1 = bidx.unset(qi);
                let bidx2 = bidx.set(qi);

                let weight_a = weight * Complex::new(0.5, 0.5);
                let weight_b = weight * Complex::new(0.5, -0.5);

                if bidx.get(qi) {
                    MaybeBranchingOutput::OutputTwo((bidx1, weight_b), (bidx2, weight_a))
                } else {
                    MaybeBranchingOutput::OutputTwo((bidx1, weight_a), (bidx2, weight_b))
                }
            }
            Self::SqrtY(qi) => {
                let bidx1 = bidx.unset(qi);
                let bidx2 = bidx.set(qi);

                if bidx.get(qi) {
                    MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * Complex::new(0.5, -0.5)),
                        (bidx2, weight * Complex::new(0.5, -0.5)),
                    )
                } else {
                    MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * Complex::new(0.5, -0.5)),
                        (bidx2, weight * Complex::new(-0.5, 0.5)),
                    )
                }
            }
            Self::SqrtW(qi) => {
                let bidx1 = bidx.unset(qi);
                let bidx2 = bidx.set(qi);

                if bidx.get(qi) {
                    MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * Complex::new(0.0, constants::RECP_SQRT_2)),
                        (bidx2, weight * Complex::new(-0.5, -0.5)),
                    )
                } else {
                    MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * Complex::new(-0.5, -0.5)),
                        (bidx2, weight * Complex::new(-constants::RECP_SQRT_2, 0.0)),
                    )
                }
            }
            Self::X(qi) => {
                let new_bidx = bidx.flip(qi);
                MaybeBranchingOutput::OuptutOne((new_bidx, weight.clone()))
            }
            Self::CX { control, target } => {
                let new_bidx = if bidx.get(control) {
                    bidx.flip(target)
                } else {
                    bidx.clone()
                };
                MaybeBranchingOutput::OuptutOne((new_bidx, weight.clone()))
            }
            Self::CZ { control, target } => {
                let new_weight = if bidx.get(control) && bidx.get(target) {
                    weight * Complex::new(-1.0, 0.0)
                } else {
                    weight.clone()
                };

                MaybeBranchingOutput::OuptutOne((bidx.clone(), new_weight))
            }
            Self::CCX {
                control1,
                control2,
                target,
            } => {
                let new_bidx = if bidx.get(control1) && bidx.get(control2) {
                    bidx.flip(target)
                } else {
                    bidx.clone()
                };

                MaybeBranchingOutput::OuptutOne((new_bidx, weight.clone()))
            }
            Self::CPhase {
                control,
                target,
                rot,
            } => {
                let new_weight = if !bidx.get(control) || !bidx.get(target) {
                    weight.clone()
                } else {
                    weight * Complex::new(rot.cos(), rot.sin())
                };

                MaybeBranchingOutput::OuptutOne((bidx.clone(), new_weight))
            }
            Self::FSim {
                left,
                right,
                theta,
                phi,
            } => match (bidx.get(left), bidx.get(right)) {
                (false, false) => MaybeBranchingOutput::OuptutOne((bidx.clone(), weight.clone())),
                (true, true) => MaybeBranchingOutput::OuptutOne((
                    bidx.clone(),
                    weight * Complex::new((-phi).cos(), (-phi).sin()),
                )),
                _ => {
                    let bidx1 = bidx.clone();
                    let bidx2 = bidx.flip(left).flip(right);
                    let weight1 = weight * Complex::new(theta.cos(), 0.0);
                    let weight2 = weight * Complex::new(0.0, -theta.sin());

                    if bidx.get(left) {
                        MaybeBranchingOutput::OutputTwo((bidx1, weight2), (bidx2, weight1))
                    } else {
                        MaybeBranchingOutput::OutputTwo((bidx1, weight1), (bidx2, weight2))
                    }
                }
            },
            Self::RZ { rot, target } => {
                let x = rot / 2.0;

                let rot1 = Complex::new((-x).cos(), (-x).sin());
                let rot2 = Complex::new(x.cos(), x.sin());

                let new_weight = if bidx.get(target) {
                    weight * rot2
                } else {
                    weight * rot1
                };

                MaybeBranchingOutput::OuptutOne((bidx.clone(), new_weight))
            }
            Self::RY { rot, target } => {
                let x = rot / 2.0;

                let bidx1 = bidx.unset(target);
                let bidx2 = bidx.set(target);

                if bidx.get(target) {
                    MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * -x.sin()),
                        (bidx2, weight * x.cos()),
                    )
                } else {
                    MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * x.cos()),
                        (bidx2, weight * x.sin()),
                    )
                }
            }
            Self::CSwap {
                control,
                target1,
                target2,
            } => {
                let new_bidx = if bidx.get(control) {
                    bidx.swap(target1, target2)
                } else {
                    bidx.clone()
                };

                MaybeBranchingOutput::OuptutOne((new_bidx, weight.clone()))
            }
            Self::U {
                target: _,
                theta: _,
                phi: _,
                lambda: _,
            } => todo!(), // TODO
            Self::Other { .. } => unimplemented!(),
        }
    }
}
