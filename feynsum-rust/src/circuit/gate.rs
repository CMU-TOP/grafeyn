use std::collections::HashSet;

use crate::types::{constants, BasisIdx, BasisIdxErr, Complex, QubitIndex, Real};

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

#[derive(Debug)]
pub enum GateApplyErr {
    InvalidQubitIndex,
}

impl From<BasisIdxErr> for GateApplyErr {
    fn from(err: BasisIdxErr) -> Self {
        match err {
            BasisIdxErr::IndexOutOfBounds => Self::InvalidQubitIndex,
        }
    }
}

pub trait PushApplicable {
    fn push_apply(
        &self,
        bidx: &BasisIdx,
        weight: &Complex,
    ) -> Result<MaybeBranchingOutput, GateApplyErr>;
}

pub trait PullApplicable {
    fn pull_apply(
        &self,
        bidx: &BasisIdx,
        weight: &Complex,
    ) -> Result<MaybeBranchingOutput, GateApplyErr>;
}

#[derive(Debug)]
pub struct Gate {
    defn: GateDefn,
    pub touches: HashSet<QubitIndex>,
}

impl Gate {
    pub fn new(defn: GateDefn) -> Self {
        let touches = match &defn {
            &GateDefn::PauliY(qi)
            | &GateDefn::PauliZ(qi)
            | &GateDefn::SqrtY(qi)
            | &GateDefn::SqrtX(qi)
            | &GateDefn::SqrtW(qi)
            | &GateDefn::Hadamard(qi)
            | &GateDefn::T(qi)
            | &GateDefn::X(qi) => HashSet::from([qi]),
            &GateDefn::CX { control, target }
            | &GateDefn::CZ { control, target }
            | &GateDefn::CPhase {
                control, target, ..
            } => HashSet::from([control, target]),
            &GateDefn::CCX {
                control1,
                control2,
                target,
            } => HashSet::from([control1, control2, target]),
            &GateDefn::FSim { left, right, .. } => HashSet::from([left, right]),
            &GateDefn::RZ { target, .. } | &GateDefn::RY { target, .. } => HashSet::from([target]),
            &GateDefn::CSwap {
                control,
                target1,
                target2,
            } => HashSet::from([control, target1, target2]),
            &GateDefn::U { target, .. } => HashSet::from([target]),
            &GateDefn::Other { .. } => HashSet::new(),
        };
        Self { defn, touches }
    }
}

impl PushApplicable for Gate {
    fn push_apply(
        &self,
        bidx: &BasisIdx,
        weight: &Complex,
    ) -> Result<MaybeBranchingOutput, GateApplyErr> {
        match self.defn {
            GateDefn::PauliY(qi) => {
                let new_bidx = bidx.flip(qi)?;
                let multipler = if bidx.get(qi)? {
                    Complex::new(0.0, -1.0)
                } else {
                    Complex::new(0.0, 1.0)
                };
                let new_weight = weight * multipler;
                Ok(MaybeBranchingOutput::OuptutOne((new_bidx, new_weight)))
            }
            GateDefn::PauliZ(qi) => {
                let new_weight = if bidx.get(qi)? {
                    weight * Complex::new(-1.0, 0.0)
                } else {
                    weight.clone()
                };
                Ok(MaybeBranchingOutput::OuptutOne((bidx.clone(), new_weight)))
            }
            GateDefn::Hadamard(qi) => {
                let bidx1 = bidx.unset(qi);
                let bidx2 = bidx.set(qi);

                if bidx.get(qi)? {
                    Ok(MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * Complex::new(constants::RECP_SQRT_2, 0.0)),
                        (bidx2, weight * Complex::new(-constants::RECP_SQRT_2, 0.0)),
                    ))
                } else {
                    Ok(MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * Complex::new(constants::RECP_SQRT_2, 0.0)),
                        (bidx2, weight * Complex::new(constants::RECP_SQRT_2, 0.0)),
                    ))
                }
            }
            GateDefn::T(qi) => {
                let mult = Complex::new(constants::RECP_SQRT_2, constants::RECP_SQRT_2);

                let new_weight = if bidx.get(qi)? {
                    weight * mult
                } else {
                    weight.clone()
                };

                Ok(MaybeBranchingOutput::OuptutOne((bidx.clone(), new_weight)))
            }
            GateDefn::SqrtX(qi) => {
                let bidx1 = bidx.unset(qi);
                let bidx2 = bidx.set(qi);

                let weight_a = weight * Complex::new(0.5, 0.5);
                let weight_b = weight * Complex::new(0.5, -0.5);

                if bidx.get(qi)? {
                    Ok(MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight_b),
                        (bidx2, weight_a),
                    ))
                } else {
                    Ok(MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight_a),
                        (bidx2, weight_b),
                    ))
                }
            }
            GateDefn::SqrtY(qi) => {
                let bidx1 = bidx.unset(qi);
                let bidx2 = bidx.set(qi);

                if bidx.get(qi)? {
                    Ok(MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * Complex::new(0.5, -0.5)),
                        (bidx2, weight * Complex::new(0.5, -0.5)),
                    ))
                } else {
                    Ok(MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * Complex::new(0.5, -0.5)),
                        (bidx2, weight * Complex::new(-0.5, 0.5)),
                    ))
                }
            }
            GateDefn::SqrtW(qi) => {
                let bidx1 = bidx.unset(qi);
                let bidx2 = bidx.set(qi);

                if bidx.get(qi)? {
                    Ok(MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * Complex::new(0.0, constants::RECP_SQRT_2)),
                        (bidx2, weight * Complex::new(-0.5, -0.5)),
                    ))
                } else {
                    Ok(MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * Complex::new(-0.5, -0.5)),
                        (bidx2, weight * Complex::new(-constants::RECP_SQRT_2, 0.0)),
                    ))
                }
            }
            GateDefn::X(qi) => {
                let new_bidx = bidx.flip(qi)?;
                Ok(MaybeBranchingOutput::OuptutOne((new_bidx, weight.clone())))
            }
            GateDefn::CX { control, target } => {
                let new_bidx = if bidx.get(control)? {
                    bidx.flip(target)?
                } else {
                    bidx.clone()
                };
                Ok(MaybeBranchingOutput::OuptutOne((new_bidx, weight.clone())))
            }
            GateDefn::CZ { control, target } => {
                let new_weight = if bidx.get(control)? && bidx.get(target)? {
                    weight * Complex::new(-1.0, 0.0)
                } else {
                    weight.clone()
                };

                Ok(MaybeBranchingOutput::OuptutOne((bidx.clone(), new_weight)))
            }
            GateDefn::CCX {
                control1,
                control2,
                target,
            } => {
                let new_bidx = if bidx.get(control1)? && bidx.get(control2)? {
                    bidx.flip(target)?
                } else {
                    bidx.clone()
                };

                Ok(MaybeBranchingOutput::OuptutOne((new_bidx, weight.clone())))
            }
            GateDefn::CPhase {
                control,
                target,
                rot,
            } => {
                let new_weight = if !bidx.get(control)? || !bidx.get(target)? {
                    weight.clone()
                } else {
                    weight * Complex::new(rot.cos(), rot.sin())
                };

                Ok(MaybeBranchingOutput::OuptutOne((bidx.clone(), new_weight)))
            }
            GateDefn::FSim {
                left,
                right,
                theta,
                phi,
            } => match (bidx.get(left)?, bidx.get(right)?) {
                (false, false) => Ok(MaybeBranchingOutput::OuptutOne((
                    bidx.clone(),
                    weight.clone(),
                ))),
                (true, true) => Ok(MaybeBranchingOutput::OuptutOne((
                    bidx.clone(),
                    weight * Complex::new((-phi).cos(), (-phi).sin()),
                ))),
                _ => {
                    let bidx1 = bidx.clone();
                    let bidx2 = bidx.flip(left)?.flip(right)?;
                    let weight1 = weight * Complex::new(theta.cos(), 0.0);
                    let weight2 = weight * Complex::new(0.0, -theta.sin());

                    if bidx.get(left)? {
                        Ok(MaybeBranchingOutput::OutputTwo(
                            (bidx1, weight2),
                            (bidx2, weight1),
                        ))
                    } else {
                        Ok(MaybeBranchingOutput::OutputTwo(
                            (bidx1, weight1),
                            (bidx2, weight2),
                        ))
                    }
                }
            },
            GateDefn::RZ { rot, target } => {
                let x = rot / 2.0;

                let rot1 = Complex::new((-x).cos(), (-x).sin());
                let rot2 = Complex::new(x.cos(), x.sin());

                let new_weight = if bidx.get(target)? {
                    weight * rot2
                } else {
                    weight * rot1
                };

                Ok(MaybeBranchingOutput::OuptutOne((bidx.clone(), new_weight)))
            }
            GateDefn::RY { rot, target } => {
                let x = rot / 2.0;

                let bidx1 = bidx.unset(target);
                let bidx2 = bidx.set(target);

                if bidx.get(target)? {
                    Ok(MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * -x.sin()),
                        (bidx2, weight * x.cos()),
                    ))
                } else {
                    Ok(MaybeBranchingOutput::OutputTwo(
                        (bidx1, weight * x.cos()),
                        (bidx2, weight * x.sin()),
                    ))
                }
            }
            GateDefn::CSwap {
                control,
                target1,
                target2,
            } => {
                let new_bidx = if bidx.get(control)? {
                    bidx.swap(target1, target2)?
                } else {
                    bidx.clone()
                };

                Ok(MaybeBranchingOutput::OuptutOne((new_bidx, weight.clone())))
            }
            GateDefn::U {
                target: _,
                theta: _,
                phi: _,
                lambda: _,
            } => todo!(), // TODO
            GateDefn::Other { .. } => unimplemented!(),
        }
    }
}

impl PullApplicable for Gate {
    fn pull_apply(
        &self,
        _bidx: &BasisIdx,
        _weight: &Complex,
    ) -> Result<MaybeBranchingOutput, GateApplyErr> {
        unimplemented!()
    }
}
