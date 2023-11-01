fn main() {
    futhark_bindgen::build(
        futhark_bindgen::Backend::C,
        "futhark/lib.fut",
        "futhark_lib.rs",
    )
}
