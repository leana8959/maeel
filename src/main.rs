use hashbrown::HashMap;

use std::env::args;
use std::error::Error;
use std::fs::read_to_string;
use std::fs::File;
use std::io::Read;
use std::iter::once;
use std::process::exit;
use std::ptr;
use std::rc::Rc;

type TokenData /* Token Data (token and line) type */ = (Token, u16);
type Fun /* Function (token and inline descriptor) type */ = (Rc<[TokenData]>, bool);
type BinApp /* Binary VM application */ = fn(MaeelType, MaeelType) -> MaeelType;
type VMOutput<T> /* Default VM function output */ = Result<T, Box<dyn Error>>;

macro_rules! expect_token {
    ($token:tt, $tokens:expr, $line:expr) => {{
        match $tokens.pop() {
            Some((Token::$token(value), _)) => value,
            Some((other, other_line)) => {
                emit_error!(
                    other_line,
                    format!("Expected {:?}, got {other:?}", TokenRepr::$token)
                )
            }
            None => emit_error!($line, format!("Expected {:?}, got EOF", TokenRepr::$token)),
        }
    }};
}

macro_rules! expect_stack {
    ($tpe:tt, $stack:expr, $line:expr) => {{
        match $stack.pop() {
            Ok(MaeelType::$tpe(value)) => value,
            Ok(other) => {
                emit_error!(
                    $line,
                    format!(
                        "Expected {:?} on the stack, got {other:?}",
                        MaeelTypeRepr::$tpe
                    )
                )
            }
            Err(_) => emit_error!(
                $line,
                format!("Expected {:?}, got EOF", MaeelTypeRepr::$tpe)
            ),
        }
    }};
}

macro_rules! emit_error {
    ($line:expr, $message:expr) => {{
        println!("{}: {}", $line, $message);
        exit(1);
    }};
}

/* Build a binary operator anonymous function */
macro_rules! binary_op {
    ($operator:tt) => {
        Token::BinaryOP(|a /* RHS */, b /* LHS */| b $operator a)
    };
}

/* Take c into S while P(x) starting at c0 */
macro_rules! take_with_predicate {
    ($character:expr, $characters:expr, $p:expr) => {{
        let content = once($character)
            .chain($characters.clone().take_while($p))
            .collect::<String>();

        (1..content.len()).for_each(|_| {
            $characters.next();
        });

        content
    }};
}

/* MaeelVM true */
macro_rules! True {
    () => {
        MaeelType::Integer(1) /* True <-> 1 */
    };
}

/* MaeelVM false */
macro_rules! False {
    () => {
        MaeelType::Integer(0) /* False <-> 0 */
    };
}

#[derive(Debug /* For error messages */)]
enum MaeelType {
    Float(f32),                       /* Float type */
    Integer(i32),                     /* Integer type */
    String(String),                   /* String type */
    Array(Vec<Self>),                 /* Array of basic types */
    Function(Fun),                    /* Custom type for a function (using functions as objects) */
    Structure(HashMap<String, Self>), /* Custom type for a structure */
}

#[derive(Debug)]
#[allow(unused)]
enum MaeelTypeRepr {
    Float,     /* Float type */
    Integer,   /* Integer type */
    String,    /* String type */
    Array,     /* Array of basic types */
    Function,  /* Custom type for a function (using functions as objects) */
    Structure, /* Custom type for a structure */
}

#[derive(Clone, Debug /* For error messages */)]
enum Token {
    Block(Vec<TokenData>), /* (...) */
    String(String),        /* "abc"*/
    Identifier(String),    /* abc */
    Integer(i32),          /* 123 */
    Float(f32),            /* 123.123 */
    BinaryOP(BinApp),      /* T x T -> T */
    Colon,                 /* : */
    Dot,                   /* . */
    Call,                  /* &, ! */
    Assignment,            /* -> */
    Then,                  /* => */
    ArrayStart,            /* { */
    ArrayEnd,              /* } */
    BlockStart,            /* ( */
    BlockEnd,              /* ) */
    MaeelAssignment,       /* Only MaeelVM(rs) can use this */
}

#[derive(Debug)]
#[allow(unused)]
enum TokenRepr {
    Block,      /* (...) */
    String,     /* "abc"*/
    Identifier, /* abc */
    Integer,    /* 123 */
    Float,      /* 123.123 */
    BinaryOP,   /* T x T -> T */
    Colon,      /* : */
    Dot,        /* . */
    Call,       /* &, ! */
    Assignment, /* -> */
    Then,       /* => */
    ArrayStart, /* { */
    ArrayEnd,   /* } */
    BlockStart, /* ( */
    BlockEnd,   /* ) */
}

/* A Node on the stack */
struct Guitar<T> {
    value: T,             /* Node type */
    next: *mut Guitar<T>, /* Raw pointer to the next node */
}

impl<T> Guitar<T> {
    fn new(value: T) -> *mut Self {
        Box::into_raw(Box::new(Guitar {
            value,
            next: ptr::null_mut(), /* Mutable null pointer */
        }))
    }
}

/* Maeel Stack */
struct BocchiVM {
    head: *mut Guitar<MaeelType>, /* Raw pointer to the head node */
    included: Vec<String>,
}

impl Default for BocchiVM {
    fn default() -> Self {
        BocchiVM {
            head: ptr::null_mut(),
            included: Vec::default(),
        }
    }
}

impl BocchiVM {
    /// Perform a binary operation
    fn binary_op(&mut self, app: BinApp) -> VMOutput<()> {
        let output = app(
            self.pop()?, /* Pop the RHS */
            self.pop()?, /* Pop the LHS */
        );

        self.push(output /* Push the output */)
    }

    /// Parse an array and its content
    fn parse_array(
        &mut self,
        tokens: &mut Vec<TokenData>,
        vars: &mut HashMap<String, MaeelType>,
    ) -> VMOutput<()> {
        let mut xs /* Array content */ = Vec::default();

        while let Some(temporary_token_data) = tokens.pop() {
            let (temporary_token, temporary_line) =
                (temporary_token_data.0, temporary_token_data.1);

            match temporary_token {
                Token::ArrayEnd =>
                /* Stop parsing the array */
                {
                    break
                }

                Token::ArrayStart =>
                /* Parse an array inside an array (recursive) */
                {
                    self.parse_array(tokens, vars)?;
                    xs.push(self.pop()?);
                }

                Token::String(_) | Token::Integer(_) | Token::Float(_) | Token::Block(_) =>
                {
                    xs.push(temporary_token.into())
                }

                Token::Identifier(identifier) =>
                {
                    match vars.get(&identifier) {
                        Some(value) => xs.push(value.clone()),

                        _ => emit_error!(
                            temporary_line,
                            format!("unknown identifier found while parsing array: {identifier} (maybe store it inside a variable ?)")
                        )
                    }
                },

                _ => emit_error!(temporary_line, "unknown token found while parsing array"),
            }
        }

        /* Finally, push the array on the stack */
        self.push(MaeelType::Array(xs))?;

        Ok(())
    }

    fn process_tokens(
        &mut self,
        tokens: &mut Vec<TokenData>,                 /* Program tokens */
        vars: &mut HashMap<String, MaeelType>,       /* Global vars */
        funs: &mut HashMap<String, Fun>,             /* Global funs */
        structs: &mut HashMap<String, Rc<[String]>>, /* Global structs */
    ) -> VMOutput<()> {
        /* Using tokens vec like a stack, so we can edit it while we iterate through it */
        tokens.reverse();

        while let Some(token_data) = tokens.pop() {
            let (token, line) = (token_data.0 /* Token */, token_data.1 /* Line */);

            match token {
                Token::BlockStart | Token::BlockEnd | Token::ArrayEnd =>
                /* Should not be there... */
                {
                    emit_error!(line, "syntax error")
                }

                Token::ArrayStart =>
                /* Parse arrays */
                {
                    self.parse_array(tokens, vars)?
                }

                Token::BinaryOP(app) =>
                /* Perform a binary operation */
                {
                    self.binary_op(app)?
                }

                Token::String(_) | Token::Float(_) | Token::Integer(_) => {
                    self.push(token.into())?
                }

                Token::Dot =>
                /* Access structures members */
                {
                    let structure_member_name = expect_token!(Identifier, tokens, line);

                    let structure = expect_stack!(Structure, self, line);
                    let structure_member = structure.get(&structure_member_name);

                    if let Some(MaeelType::Function(fun)) = structure_member {
                        if fun.1
                        /* Inline function */
                        {
                            /* We just push functions tokens on
                            the current tokens stack and continue */
                            fun.0.iter().for_each(|token| tokens.push(token.clone()));
                        } else {
                            /* We create a new stack (functions, variables and structures are shared) */
                            self.process_tokens(
                                &mut fun.0.to_vec(), /* Function tokens */
                                &mut vars.clone(),   /* Clone the variables */
                                funs,
                                structs,
                            )?
                        }

                        continue;
                    }

                    self.push(structure_member.unwrap().clone())?
                }

                Token::Block(mut block) =>
                /* Use code blocks as first class objects */
                {
                    block.reverse();

                    /* Push the codeblock */
                    self.push(MaeelType::Function((
                        block.as_slice().into(), /* Function tokens */
                        true,                    /* Function inline descriptor */
                    )))?
                }

                Token::Colon =>
                /* Use functions as first class objects */
                {
                    let fun_name /* Function name */ = expect_token!(Identifier, tokens, line);

                    let fun /* Function object */ = funs.get(&fun_name).unwrap_or_else(|| {
                        emit_error!(line, format!("unknown function: {fun_name:?}"))
                    });

                    /* Push the function codeblock */
                    self.push(MaeelType::Function((
                        fun.0.clone(), /* Function tokens */
                        fun.1,         /* Function inline descriptor */
                    )))?
                }

                Token::Call =>
                /* Manually call a function */
                {
                    let fun /* Function object */ = expect_stack!(Function, self, line);

                    if fun.1
                    /* Inline function */
                    {
                        /* We just push functions tokens on
                        the current tokens stack and continue */
                        fun.0.iter().for_each(|token| tokens.push(token.clone()));
                    } else {
                        /* We create a new stack (functions, variables and structures are shared) */
                        self.process_tokens(
                            &mut fun.0.to_vec(), /* Function tokens */
                            &mut vars.clone(),   /* Clone the variables */
                            funs,
                            structs,
                        )?
                    }
                }

                Token::Then =>
                /* Execute a code block iff there is a 1 on the stack */
                {
                    let temporary_token = tokens.pop();

                    match self.pop() {
                        Ok(True!()) => match temporary_token {
                            Some((Token::Block(temporary_tokens), _)) => temporary_tokens
                                .iter()
                                .rev()
                                .for_each(|token| tokens.push(token.clone())),
                            Some(temporary_token) => tokens.push(temporary_token),
                            None => emit_error!(line, "expected something after '=>'"),
                        },

                        Ok(False!()) => { /* Do nothing */ }

                        Ok(other) => emit_error!(
                            line,
                            format!("'=>' expects a boolean (0 or 1) on the stack; got {other:?} instead.")
                        ),

                        Err(_) => emit_error!(line, "'=>' expects a boolean (0 or 1) on the stack."),
                    }
                }

                Token::MaeelAssignment =>
                /* MaeelType <-> identifier */
                {
                    vars.insert(
                        expect_token!(Identifier, tokens, line), /* Variable name */
                        self.pop()?,                             /* Variable value */
                    );
                }

                Token::Assignment =>
                /* MaeelType <-> identifier */
                {
                    let name = expect_token!(Identifier, tokens, line);

                    if name.starts_with("__") {
                        panic!()
                    }

                    vars.insert(
                        name,        /* Variable name */
                        self.pop()?, /* Variable value */
                    );
                }

                Token::Identifier(identifier) =>
                /* Functions/keywords that cannot be defined in maeel*/
                {
                    match identifier.as_str() {
                        "print" =>
                        /* Print the top token */
                        {
                            print!("{}", self.peek()?)
                        }

                        "clear" =>
                        /* Process "clear" VM operation */
                        {
                            self.clear()
                                .unwrap_or_else(|_| emit_error!(line, "failed to clear stack!"))
                        }

                        /* For loop implementation */
                        "for" => {
                            let temporary_tokens /* Code block to execute at each iteration */ = expect_token!(Block, tokens, line);

                            match self.pop() /* Determine what to iterate through */ {
                                Ok(MaeelType::Array(xs)) =>
                                /* Iterate through an array */
                                {
                                    xs.iter().for_each(|x| {
                                        self.push(x.clone()).unwrap();

                                        self.process_tokens(
                                            &mut temporary_tokens.clone(),
                                            vars,
                                            funs,
                                            structs,
                                        )
                                        .unwrap();
                                    });
                                }

                                Ok(MaeelType::String(string)) =>
                                /* Iterate through a string */
                                {
                                    string.chars().for_each(|x| {
                                        self.push(MaeelType::String(x.to_string())).unwrap();

                                        self.process_tokens(
                                            &mut temporary_tokens.clone(),
                                            vars,
                                            funs,
                                            structs,
                                        )
                                        .unwrap();
                                    });
                                }

                                other => emit_error!(
                                    line,
                                    format!("can't iterate through {other:?}")
                                )

                            }
                        }

                        "struct" => {
                            let struct_name /* Structure name */ = expect_token!(Identifier, tokens, line);
                            let mut struct_fields /* Structure attributes */ = Vec::default();

                            while let Some(temporary_tokens) = tokens.pop() {
                                match temporary_tokens {
                                    (Token::Dot, _) =>
                                    /* Stop parsing structure fields on '.' */
                                    {
                                        break;
                                    }

                                    (Token::Identifier(identifier), _) =>
                                    /* New structure attribute */
                                    {
                                        struct_fields.push(identifier);
                                    }

                                    (other, other_line) => emit_error!(
                                        other_line,
                                        format!("expected identifier(s) or dot after 'struct {struct_name}'; got {other:?} instead.")
                                    )
                                }
                            }

                            struct_fields.reverse();
                            structs.insert(struct_name, struct_fields.as_slice().into());
                        }

                        "fun" => {
                            let mut fun_name = expect_token!(Identifier, tokens, line);
                            let mut is_inline = false;

                            if fun_name == "inline" {
                                is_inline = true;
                                fun_name = expect_token!(Identifier, tokens, line);
                            }

                            let mut fun_tokens = Vec::default(); /* Final tokens */

                            while let Some(temporary_token) = tokens.pop() {
                                match temporary_token {
                                    (Token::Block(temporary_tokens), _) => {
                                        fun_tokens.reverse(); /* First reverse */
                                        fun_tokens.extend(temporary_tokens);
                                        fun_tokens.reverse(); /* Second reverse */

                                        break;
                                    }

                                    (Token::Identifier(_), line) => {
                                        fun_tokens.push(temporary_token);
                                        fun_tokens.push((Token::MaeelAssignment, line));
                                    }

                                    (other, other_line) => {
                                        emit_error!(other_line, format!("expected identifier(s) or a code block after 'fun {fun_name}'; got {other:?} instead."))
                                    }
                                }
                            }

                            funs.insert(
                                fun_name.clone(),
                                (fun_tokens.as_slice().into(), is_inline),
                            );
                        }

                        "get" => {
                            let index = expect_stack!(Integer, self, line) as usize;

                            match self.pop() {
                                Ok(MaeelType::Array(xs)) => self.push(
                                    xs.get(index)
                                        .unwrap_or_else(|| {
                                            emit_error!(line, format!("unknown index: {index}"))
                                        })
                                        .clone(),
                                ),

                                Ok(MaeelType::String(string)) => self.push(MaeelType::String(
                                    string
                                        .chars()
                                        .nth(index)
                                        .unwrap_or_else(|| {
                                            emit_error!(line, format!("unknown index: {index}"))
                                        })
                                        .to_string(),
                                )),

                                Ok(other) => emit_error!(line, format!("unindexable: {other:?}")),

                                _ => emit_error!(line, format!("unindexable: EOF")),
                            }?
                        }

                        "read" => {
                            let bytes = expect_stack!(Integer, self, line);
                            let path = expect_stack!(String, self, line);

                            assert!(bytes >= 0);

                            let mut buf = vec![0u8; bytes as usize]; /* empty buffer */

                            File::open(path)?.read_exact(&mut buf)?; /* file content -> buffer */

                            let content_bytes = buf
                                .iter()
                                .map(|byte| MaeelType::Integer(*byte as i32))
                                .collect();

                            self.push(MaeelType::Array(content_bytes))?
                        }

                        "include" => {
                            let target = expect_stack!(String, self, line);

                            if self.included.contains(&target) {
                                continue;
                            }

                            self.included.push(target.clone());

                            let content = match target.as_str() {
                                /* Standard library is included at compile time. We prefer memory
                                usage over CPU usage... */
                                "std" => include_str!("../stdlib/std.maeel").to_string(),

                                "core" => include_str!("../stdlib/core.maeel").to_string(),

                                "logic" => include_str!("../stdlib/logic.maeel").to_string(),

                                "array" => include_str!("../stdlib/array.maeel").to_string(),

                                "fp" => include_str!("../stdlib/fp.maeel").to_string(),

                                "math" => include_str!("../stdlib/math.maeel").to_string(),

                                "string" => include_str!("../stdlib/string.maeel").to_string(),

                                "unix" => include_str!("../stdlib/unix.maeel").to_string(),

                                _ => read_to_string(target).unwrap_or_else(|_| {
                                    emit_error!(line, "failed to include file")
                                }),
                            };

                            lex_into_tokens(&content)
                                .iter()
                                .rev()
                                .for_each(|token| tokens.push(token.clone()))
                        }

                        identifier => {
                            if let Some(value) = vars.get(identifier)
                            /* Identifier is a variable */
                            {
                                self.push(value.clone())?;

                                continue;
                            }

                            if let Some(fun) = funs.get(identifier)
                            /* Identifier is a function */
                            {
                                if fun.1
                                /* Inline function */
                                {
                                    fun.0.iter().for_each(|token| tokens.push(token.clone()));
                                    continue;
                                }

                                /* Turn fun tokens into a vec so we can use reverse() later */
                                let mut fun_tokens = fun.0.clone().to_vec();

                                /* ...here lol */
                                fun_tokens.reverse();

                                self.process_tokens(
                                    &mut fun_tokens,
                                    &mut vars.clone(),
                                    funs,
                                    structs,
                                )?;

                                continue;
                            }

                            if let Some(fields) = structs.get(identifier)
                            /* Identifier is a structure */
                            {
                                let mut structure /* Future structure */ = HashMap::with_capacity(fields.len());

                                /* Map each field to a value of the stack */
                                fields.iter().for_each(|key| {
                                    structure.insert(key.clone(), self.pop().unwrap());
                                });

                                /* Finally, push the structure */
                                self.push(MaeelType::Structure(structure))?;

                                continue;
                            }

                            emit_error!(line, format!("unknown identifier {identifier}"))
                        }
                    }
                }
            };
        }

        Ok(())
    }

    fn push(&mut self, value: MaeelType) -> VMOutput<()> {
        let future_head /* Create a new node that contains `value` */ = Guitar::new(value);

        if !self.head.is_null() {
            unsafe {
                (*future_head).next /* Set head as future_head next node */ = self.head;
            }
        }

        self.head /* Replace head with future_head */ = future_head;

        Ok(())
    }

    fn pop(&mut self) -> VMOutput<MaeelType> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        let current_head = unsafe { Box::from_raw(self.head) };

        self.head /* Replace the current head with her next node */ = current_head.next;

        Ok(current_head.value)
    }

    fn peek(&self) -> VMOutput<&MaeelType> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        Ok(unsafe { &(*self.head).value })
    }

    fn clear(&mut self) -> VMOutput<()> {
        while !self.head.is_null()
        /* Dropping all the next_node until head is not null */
        {
            self.head = unsafe { Box::from_raw(self.head) }.next
        }

        Ok(())
    }
}

impl Clone for MaeelType {
    fn clone(&self) -> Self {
        match self {
            Self::Float(a) => Self::Float(*a),
            Self::Integer(a) => Self::Integer(*a),
            Self::String(a) => Self::String(a.clone()),
            Self::Array(a) => Self::Array(a.clone()),
            Self::Function(a) => Self::Function(a.clone()),
            Self::Structure(a) => Self::Structure(a.clone()),
        }
    }
}

impl std::fmt::Display for MaeelType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(x) => write!(f, "{}", x),
            Self::Function(_) => write!(f, "Function"),
            Self::Float(x) => write!(f, "{}", x),
            Self::Integer(x) => write!(f, "{}", x),
            Self::Array(xs) => {
                write!(f, "{{")?;

                xs.iter().enumerate().for_each(|(i, x)| {
                    if i > 0 {
                        write!(f, " ").unwrap();
                    }

                    write!(f, "{}", x).unwrap();
                });

                write!(f, "}}")
            }

            Self::Structure(x) => {
                write!(f, "{{")?;
                x.iter().enumerate().for_each(|(i, (k, v))| {
                    if i > 0 {
                        write!(f, " ").unwrap();
                    }

                    write!(f, "{}: {}", k, v).unwrap();
                });

                write!(f, "}}")
            }
        }
    }
}

/// Perform lexical parsing over code.
fn lex_into_tokens(code: &str) -> Vec<TokenData> {
    let mut depth = 0;
    let mut line = 1;
    let mut tokens = Vec::default();
    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            '|' =>
            /* Parse comments */
            {
                for character in characters.by_ref() {
                    if character == '\n'
                    /* Comment ends at end-of-line */
                    {
                        break;
                    }
                }
            }

            '\n' => line += 1,

            ' ' | '\t' =>
            /* Ignore whitespaces */
            {
                continue
            }

            '(' =>
            /* Code block start */
            {
                tokens.push((Token::BlockStart, line));
                depth += 1;
            }

            ')' =>
            /* Code block end */
            {
                tokens.push((Token::BlockEnd, line));
                depth -= 1;
            }

            '"' =>
            /* Parse strings */
            {
                let content_vector = characters
                    .by_ref()
                    .take_while(|&character| character != '"')
                    .collect::<Vec<char>>();

                let mut index = 0;
                let mut content = String::with_capacity(content_vector.len());

                while index < content_vector.len() {
                    let character = content_vector[index];

                    index += 1;

                    content.push(match (character, content_vector.get(index)) {
                        ('\\', Some(next_character)) => {
                            index += 1;

                            match next_character {
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                '\\' => '\\',
                                '"' => '"',
                                _ => {
                                    panic!(
                                        "{line}: invalid escape \
                                                 sequence: \\{}",
                                        next_character
                                    )
                                }
                            }
                        }

                        ('\\', None) => {
                            emit_error!(line, "incomplete escape sequence")
                        }

                        _ => character,
                    });
                }

                tokens.push((Token::String(content), line))
            }

            'a'..='z' | 'A'..='Z' | '_' =>
            /* Parse identifiers */
            {
                tokens.push((
                    Token::Identifier(take_with_predicate!(character, characters, |&character| {
                        character.is_alphanumeric() || character == '_'
                    })),
                    line,
                ))
            }

            '0'..='9' =>
            /* Parse numerics (float/integers) */
            {
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_ascii_digit() || character == '.' || character == '_'
                });

                tokens.push((
                    if content.contains('.') {
                        assert_eq!(content.matches('.').count(), 1);

                        Token::Float(content.parse().unwrap())
                    } else {
                        Token::Integer(content.parse().unwrap())
                    },
                    line,
                ));
            }

            '=' => match characters.peek()
            /* Parse equal/then, Just using peek() because of the _ case */
            {
                Some('>') =>
                /* Then (=>) */
                {
                    tokens.push((Token::Then, line));
                    characters.next(); /* Using next() because we used peek() before */
                }

                Some(':') =>
                /* Assignment (=:) */
                {
                    tokens.push((Token::Assignment, line));
                    characters.next(); /* Using next() because we used peek() before */
                }

                _ =>
                /* Equal (=) */
                {
                    tokens.push((
                        Token::BinaryOP(|a, b| MaeelType::Integer((b == a) as i32)),
                        line,
                    ))
                }
            },

            '-' =>
            /* Parse minus/assignment */
            {
                match characters.peek() {
                    Some('>') =>
                    /* Assignment (->) */
                    {
                        tokens.push((Token::Assignment, line));
                        characters.next();
                    }

                    _ =>
                    /* Minus (-) */
                    {
                        tokens.push((Token::BinaryOP(|a, b| b - a), line))
                    }
                }
            }

            _ => tokens.push((
                match character {
                    '+' => binary_op!(+),
                    '*' => binary_op!(*),
                    '/' => binary_op!(/),
                    '%' => binary_op!(%),
                    '<' => Token::BinaryOP(|a, b| MaeelType::Integer((b < a) as i32)),
                    '>' => Token::BinaryOP(|a, b| MaeelType::Integer((b > a) as i32)),
                    '(' => Token::BlockStart,
                    ')' => Token::BlockEnd,
                    '{' => Token::ArrayStart,
                    '}' => Token::ArrayEnd,
                    '!' => Token::Call,
                    '.' => Token::Dot,
                    ':' => Token::Colon,

                    character => emit_error!(line, format!("found unknown char: {character}")),
                },
                line,
            )),
        }
    }

    assert_eq!(depth, 0);

    let mut stack = Vec::default();
    let mut output = Vec::default();
    let mut temporary_tokens = Vec::default();

    for token in tokens.iter() {
        match token {
            (Token::BlockStart, _) => {
                stack.push(temporary_tokens);
                temporary_tokens /* Clear the temporary tokens */ = Vec::default();
            }

            (Token::BlockEnd, _) => {
                let nested_tokens = temporary_tokens.clone();

                match stack.pop() {
                    Some(previous_tokens) => {
                        temporary_tokens = previous_tokens;
                        temporary_tokens.push((Token::Block(nested_tokens), line));
                    }

                    _ => output.push((Token::Block(nested_tokens), line)),
                }
            }

            _ => temporary_tokens.push(token.clone()),
        }
    }

    output.append(&mut temporary_tokens);
    output
}

impl PartialOrd for MaeelType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => Some(a.cmp(b)),
            (Self::Float(a), Self::Float(b)) => Some(a.total_cmp(b)),
            (Self::Integer(a), Self::Float(b)) | (Self::Float(b), Self::Integer(a)) => {
                Some(b.total_cmp(&(*a as f32)))
            }

            (a, b) => panic!("Cannot compare {a} and {b}"),
        }
    }
}

impl PartialEq for MaeelType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Array(a), Self::Array(b)) => a == b,
            (Self::Integer(a), Self::Float(b)) | (Self::Float(b), Self::Integer(a)) => {
                (*a as f32) == *b
            }
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,

            _ => false,
        }
    }
}

impl std::ops::Sub for MaeelType {
    type Output = MaeelType;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m - n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x - y),
            (Self::Float(x), Self::Integer(m)) | (Self::Integer(m), Self::Float(x)) => {
                Self::Float(m as f32 - x)
            }

            (a, b) => panic!("Cannot substract {a} and {b}"),
        }
    }
}

impl std::ops::Mul for MaeelType {
    type Output = MaeelType;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m * n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x * y),
            (Self::Float(x), Self::Integer(m)) | (Self::Integer(m), Self::Float(x)) => {
                Self::Float(x * m as f32)
            }
            (Self::Integer(m), Self::String(s)) | (Self::String(s), Self::Integer(m)) => {
                Self::String(s.repeat(m as usize))
            }

            (a, b) => panic!("Cannot multiply {a} and {b}"),
        }
    }
}

impl std::ops::Add for MaeelType {
    type Output = MaeelType;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::String(a), Self::String(b)) => Self::String(a + &b),
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m + n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x + y),
            (Self::Integer(m), Self::Float(x)) | (Self::Float(x), Self::Integer(m)) => {
                Self::Float(m as f32 + x)
            }
            (other, Self::Array(mut xs)) | (Self::Array(mut xs), other) => {
                xs.push(other);
                Self::Array(xs)
            }

            (a, b) => panic!("Cannot add {a} and {b}"),
        }
    }
}

impl std::ops::Rem for MaeelType {
    type Output = MaeelType;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Integer(m % n),
            (Self::Float(x), Self::Float(y)) => Self::Float(x % y),
            (Self::Integer(m), Self::Float(x)) => Self::Float(m as f32 % x),
            (Self::Float(x), Self::Integer(m)) => Self::Float(x % m as f32),
            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl std::ops::Div for MaeelType {
    type Output = MaeelType;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(m), Self::Integer(n)) => Self::Float(m as f32 / n as f32),
            (Self::Float(x), Self::Float(y)) => Self::Float(x / y),
            (Self::Integer(m), Self::Float(x)) => Self::Float(m as f32 / x),
            (Self::Float(x), Self::Integer(m)) => Self::Float(x / m as f32),
            (a, b) => panic!("Cannot divide {a} and {b}"),
        }
    }
}

impl From<Token> for MaeelType {
    fn from(val: Token) -> Self {
        match val {
            Token::String(x) => MaeelType::String(x),
            Token::Integer(x) => MaeelType::Integer(x),
            Token::Float(x) => MaeelType::Float(x),
            Token::Block(x) => MaeelType::Function((x.as_slice().into(), false)),
            _ => panic!(),
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    BocchiVM::default().process_tokens(
        &mut lex_into_tokens(&read_to_string(args().nth(1).unwrap())?),
        &mut HashMap::default(),
        &mut HashMap::default(),
        &mut HashMap::default(),
    )
}
