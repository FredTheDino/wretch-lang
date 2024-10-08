mod ast;
mod codegen;
mod lexer;
mod parser;
mod rst;
mod tyc;

fn main() -> Result<(), String> {
    let source = std::fs::read_to_string("example.wr").unwrap();
    let (errs, maybe_ast) = parser::parse(source.as_ref(), ast::FileId(0));
    for (i, e) in errs.iter().enumerate() {
        dbg!((i, e));
    }
    let ast = match maybe_ast {
        Some(_) if !errs.is_empty() => return Err("Failed to parse".into()),
        None => return Err("Failed to parse".into()),
        Some(ast) => ast,
    };
    let (rst, mapping, errs) = rst::resolve(&ast);
    for (i, e) in errs.iter().enumerate() {
        dbg!((i, e));
    }
    if !errs.is_empty() {
        return Err("Name resolution failed".into());
    }

    let graph = tyc::check(&rst, &mapping);
    for (i, e) in graph.1.iter().enumerate() {
        dbg!((i, e));
    }
    if !errs.is_empty() {
        return Err("Name resolution failed".into());
    }

    let f = std::fs::File::create("output.lua").unwrap();
    codegen::gen(f, rst, mapping).unwrap();
    Ok(())
}
