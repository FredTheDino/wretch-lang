mod lexer;
mod parser;

fn main() {
    let source = std::fs::read_to_string("example.wr").unwrap();
    dbg!(parser::parse(source.as_ref(), parser::FileId(0)));
}
