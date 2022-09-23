use wacil_bindgen::interface;

pub extern "C" fn example_function() {}

#[test]
fn generate() {
    //let interface =

    let module = wacil_backend::Module {
        interfaces: &[],
        module_name: interface::TypeName::new(Default::default(), "HelloWacil"),
        wrapper_name: interface::TypeName::new(Default::default(), "HelloWacilWrapper"),
    };

    let do_it = || -> std::io::Result<()> {
        wacil_backend::generate(
            &module,
            std::fs::File::create(std::path::Path::new("./HelloWacil.cs"))?,
        )
    };

    do_it().unwrap();
}
