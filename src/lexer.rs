use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'t> {
    #[token("mod")]
    Mod,

    #[token("def")]
    Def,

    #[token("typ")]
    Typ,

    // TODO:
    // Comments?
    // Floats
    // Strings
    // Case
    // dat?
    // Lua?

    #[regex("-{3,}")]
    Sep,

    #[regex("[[:upper:]][[:alnum:]]+", priority = 1)]
    Propper(&'t str),

    #[regex(
        r"[[:lower:]|([:punct:]&&[^\[\]\{\}\.])][[:alnum:]|[:punct:]]*",
        priority = 1
    )]
    Name(&'t str),

    #[regex(r"[0-9]*")]
    Int(&'t str),

    #[token("{", priority = 6)]
    LBrace,

    #[token("}", priority = 6)]
    RBrace,

    #[token("[", priority = 6)]
    LBracket,

    #[token("]", priority = 6)]
    RBracket,

    #[token("=", priority = 6)]
    Equal,

    #[token(",", priority = 6)]
    Comma,

    #[token(":", priority = 6)]
    Colon,

    #[token("->", priority = 6)]
    Arrow,

    #[token("if", priority = 6)]
    If,
    #[token("then", priority = 6)]
    Then,
    #[token("else", priority = 6)]
    Else,
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    fn expect(s: &'static str, o: &[Token]) {
        assert_eq!(Token::lexer(s).collect::<Result<Vec<_>, _>>(), Ok(o.into()));
    }

    #[test]
    fn some_tokens() {
        expect(
            "mod ABC def a = 1 2 +",
            &[
                Mod,
                Propper("ABC"),
                Def,
                Name("a"),
                Equal,
                Int("1"),
                Int("2"),
                Name("+"),
            ],
        )
    }

    #[test]
    fn example_program() {
        expect(
            r"
mod Example

typ 

def Int, Int -> Int
def add = +
            ",
            &[
                Mod,
                Propper("Example"),
                Typ,
                Def,
                Propper("Int"),
                Comma,
                Propper("Int"),
                Arrow,
                Propper("Int"),
                Def,
                Name("add"),
                Equal,
                Name("+"),
            ],
        )
    }
}
