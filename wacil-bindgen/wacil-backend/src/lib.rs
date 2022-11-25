//! Used to generate C# wrapper code for a Rust WebAssembly module translated to a .NET assembly by `wacil`.

mod syntax;

use std::io::Write;
use wacil_bindgen::interface;

pub struct Module {
    pub interfaces: &'static [&'static interface::Interface],
    pub module_name: interface::TypeName,
    pub wrapper_name: interface::TypeName,
}

pub fn generate<W: Write>(module: &Module, mut destination: W) -> std::io::Result<()> {
    use syntax::*;

    let module_type = Type::Named {
        name: module.module_name,
        arguments: &[],
    };

    let runtime_namespace = Namespace::new(&["Wacil", "Runtime"]);

    let module_wrapper = Type::Named {
        name: TypeName::new(runtime_namespace, "ModuleWrapper"),
        arguments: &[module_type],
    };

    let constructor_parameters = [Parameter {
        name: "module",
        argument_type: &module_type,
    }];

    let members = vec![Member::Constructor(Constructor {
        access: AccessModifier::Private,
        parameters: &constructor_parameters,
        base_call: ConstructorBaseCall::Base(&[Expression::Identifier("module")]),
        body: &[],
    })];

    let tree = SourceCode {
        namespace: module.wrapper_name.namespace,
        type_definitions: &[TypeDefinition {
            access: AccessModifier::Public,
            kind: TypeDefinitionKind::Class,
            name: module.wrapper_name.name,
            sub_types: &[&module_wrapper],
            members: &members,
        }],
    };

    writeln!(destination, "// <auto-generated>Generated by wacil</auto-generated>")?;
    write!(destination, "{tree}")?;
    writeln!(destination)
}
