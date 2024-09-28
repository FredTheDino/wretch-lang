mod lexer;
mod parser;
mod ast;
mod rst;
mod codegen;

fn main() {
    let source = std::fs::read_to_string("example.wr").unwrap();
    let (errs, maybe_ast) = parser::parse(source.as_ref(), ast::FileId(0));
    for (i, e) in errs.iter().enumerate() {
        dbg!((i, e));
    }
    if let Some(ast) = maybe_ast {
        let (rst, mapping, errs) = rst::resolve(&ast);
        for (i, e) in errs.iter().enumerate() {
            dbg!((i, e));
        }
        let f = std::fs::File::create("output.lua").unwrap();
        codegen::gen(f, rst, mapping).unwrap();
    }
}
