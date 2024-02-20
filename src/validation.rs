use std::collections::HashMap;

use crate::ast::*;

pub type FunctionMap = HashMap<String, usize>;

pub fn validate(program: &ASTProgram) -> Result<FunctionMap, ()> {
    let mut ok = true;
    let mut func_map: FunctionMap = HashMap::new();

    func_map.insert("putchar".to_string(), 1);

    for func in program.functions.iter() {
        let prev = func_map.insert(func.ime.clone(), func.params.len());

        if prev.is_some() {
            eprintln!("ERROR duplicate function definition for function '{}'", func.ime);
            ok = false;
        }
        if func.params.len() > 6 {
            eprintln!("ERROR function '{}' has more than 6 supported parameters", func.params.len());
            ok = false;
        }
    }

    if func_map.contains_key("main") == false {
        eprintln!("ERROR missing function 'main'");
        ok = false;
    }

    if ok {
        Ok(func_map)
    } else {
        Err(())
    }
}
