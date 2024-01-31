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

type TokenData /* Token and its file name, line */ = (Token, String, u16);

macro_rules! expect_token {
    ($token:tt, $tokens:expr, $fl:expr, $line:expr) => {{
        match $tokens.pop() {
            Some((Token::$token(value), _, _)) => value,
            Some((other, other_file, other_line)) => {
                emit_error!(
                    other_file,
                    other_line,
                    format!("Expected {:?}, got {other:?}", TokenRepr::$token)
                )
            }
            None => emit_error!(
                $fl,
                $line,
                format!("Expected {:?}, got EOF", TokenRepr::$token)
            ),
        }
    }};
}

macro_rules! expect_stack {
    ($tpe:tt, $stack:expr, $fl:expr, $line:expr) => {{
        match $stack.pop() {
            Ok(MaeelType::$tpe(value)) => value,
            Ok(other) => {
                emit_error!(
                    $fl,
                    $line,
                    format!(
                        "Expected {:?} on the stack, got {other:?}",
                        MaeelTypeRepr::$tpe
                    )
                )
            }
            Err(_) => {
                emit_error!(
                    $fl,
                    $line,
                    format!("Expected {:?}, got EOF", MaeelTypeRepr::$tpe)
                )
            }
        }
    }};
}

macro_rules! emit_error {
    ($fl:expr, $line:expr, $message:expr) => {{
        println!("\n{}:{} {}", $fl, $line, $message);
        exit(1);
    }};
}

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

#[derive(Debug)]
enum MaeelType {
    Float(f32),
    Integer(i32),
    String(String),
    Array(Vec<Self>),
    Function((Rc<[TokenData]>, bool)),
}

#[derive(Debug)]
#[allow(unused)]
enum MaeelTypeRepr {
    Float,
    Integer,
    String,
    Array,
    Function,
}

#[derive(Clone, Debug)]
enum Token {
    Block(Vec<TokenData>),
    String(String),
    Identifier(String),
    Integer(i32),
    Float(f32),
    BinaryOP(fn(MaeelType, MaeelType) -> MaeelType),
    Colon,
    Call,
    Assignment,
    Then,
    ArrayStart,
    ArrayEnd,
    BlockStart,
    BlockEnd,
    MaeelAssignment,
    At,
}

#[derive(Debug)]
#[allow(unused)]
enum TokenRepr {
    Block,
    String,
    Identifier,
    Integer,
    Float,
    BinaryOP,
    Colon,
    Call,
    Assignment,
    Then,
    ArrayStart,
    ArrayEnd,
    BlockStart,
    BlockEnd,
    At,
}

struct Guitar<T> {
    value: T,
    next: *mut Guitar<T>,
}

impl<T> Guitar<T> {
    fn new(value: T) -> *mut Self {
        Box::into_raw(Box::new(Guitar {
            value,
            next: ptr::null_mut(),
        }))
    }
}

struct BocchiVM {
    head: *mut Guitar<MaeelType>,
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
    fn parse_array(
        &mut self,
        tokens: &mut Vec<TokenData>,
        vars: &mut HashMap<String, MaeelType>,
    ) -> Result<(), Box<dyn Error>> {
        let mut xs = Vec::default();

        while let Some(temporary_token_data) = tokens.pop() {
            match temporary_token_data.0 {
                Token::ArrayEnd =>
                {
                    break
                }

                Token::ArrayStart => {
                    self.parse_array(tokens, vars)?;
                    xs.push(self.pop()?);
                }

                Token::String(_) | Token::Integer(_) | Token::Float(_) => {
                    xs.push(temporary_token_data.0.into())
                }

                Token::Block(mut block) => {
                    block.reverse();

                    xs.push(MaeelType::Function((
                        block.as_slice().into(),
                        true,
                    )))
                }

                Token::Identifier(identifier) => {
                    match vars.get(&identifier) {
                        Some(value) => xs.push(value.clone()),

                        _ => emit_error!(
                            temporary_token_data.1,
                            temporary_token_data.2,
                            format!("unknown identifier found while parsing array: {identifier} (maybe store it inside a variable ?)")
                        )
                    }
                },

                _ => emit_error!(
                    temporary_token_data.1,
                    temporary_token_data.2,
                    "unknown token found while parsing array"
                )
            }
        }

        self.push(MaeelType::Array(xs))?;

        Ok(())
    }

    fn process_tokens(
        &mut self,
        tokens: &mut Vec<TokenData>,
        vars: &mut HashMap<String, MaeelType>,
        funs: &mut HashMap<String, (Rc<[TokenData]>, bool)>,
    ) -> Result<(), Box<dyn Error>> {
        tokens.reverse();

        while let Some(token_data) = tokens.pop() {
            let (token, file, line) = (token_data.0, token_data.1, token_data.2);

            match token {
                Token::BlockStart | Token::BlockEnd | Token::ArrayEnd => {
                    emit_error!(file, line, "syntax error")
                }

                Token::ArrayStart => self.parse_array(tokens, vars)?,

                Token::BinaryOP(app) => {
                    let output = app(self.pop()?, self.pop()?);
                    self.push(output)?
                }

                Token::String(_) | Token::Float(_) | Token::Integer(_) => {
                    self.push(token.into())?
                }

                Token::Block(mut block) => {
                    block.reverse();

                    self.push(MaeelType::Function((block.as_slice().into(), true)))?
                }

                Token::At => {
                    let mut block = expect_token!(Block, tokens, file, line);
                    block.reverse();

                    self.push(MaeelType::Function((block.as_slice().into(), false)))?
                }

                Token::Colon => {
                    let fun_name = expect_token!(Identifier, tokens, file, line);

                    let fun = funs.get(&fun_name).unwrap_or_else(|| {
                        emit_error!(file, line, format!("unknown function: {fun_name:?}"))
                    });

                    self.push(MaeelType::Function((fun.0.clone(), fun.1)))?
                }

                Token::Call => {
                    let fun = expect_stack!(Function, self, file, line);

                    if fun.1 {
                        fun.0.iter().for_each(|token| tokens.push(token.clone()));
                        continue
                    }

                    self.process_tokens(&mut fun.0.to_vec(), &mut vars.clone(), funs)?
                }

                Token::Then => {
                    let temporary_token = tokens.pop();

                    match self.pop() {
                        Ok(MaeelType::Integer(1)) => match temporary_token {
                            Some((Token::Block(temporary_tokens), _, _)) => temporary_tokens
                                .iter()
                                .rev()
                                .for_each(|token| tokens.push(token.clone())),
                            Some(temporary_token) => tokens.push(temporary_token),
                            None => emit_error!(file, line, "expected something after '=>'"),
                        },

                        Ok(MaeelType::Integer(0)) => { }

                        Ok(other) => emit_error!(
                            file,
                            line,
                            format!("'=>' expects a boolean (0 or 1) on the stack; got {other:?} instead.")
                        ),

                        Err(_) => emit_error!(file, line, "'=>' expects a boolean (0 or 1) on the stack."),
                    }
                }

                Token::MaeelAssignment => {
                    vars.insert(expect_token!(Identifier, tokens, file, line), self.pop()?);
                }

                Token::Assignment => {
                    let name = expect_token!(Identifier, tokens, file, line);

                    if name.starts_with("__") {
                        panic!()
                    }

                    vars.insert(name, self.pop()?);
                }

                Token::Identifier(identifier) => match identifier.as_str() {
                    "print" => print!("{}", self.peek()?),

                    "clear" => self
                        .clear()
                        .unwrap_or_else(|_| emit_error!(file, line, "failed to clear stack!")),

                    "fun" => {
                        let mut fun_name = expect_token!(Identifier, tokens, file, line);
                        let mut is_inline = false;

                        if fun_name == "inline" {
                            is_inline = true;
                            fun_name = expect_token!(Identifier, tokens, file, line);
                        }

                        let mut fun_tokens = Vec::default();

                        while let Some(temporary_token) = tokens.pop() {
                            match temporary_token.clone() {
                                (Token::Block(temporary_tokens), _, _) => {
                                    fun_tokens.reverse();
                                    fun_tokens.extend(temporary_tokens);
                                    fun_tokens.reverse();

                                    break;
                                }

                                (Token::Identifier(_), file, line) => {
                                    fun_tokens.push(temporary_token);
                                    fun_tokens.push((Token::MaeelAssignment, file, line));
                                }

                                (other, other_file, other_line) => {
                                    emit_error!(other_file, other_line, format!("expected identifier(s) or a code block after 'fun {fun_name}'; got {other:?} instead."))
                                }
                            }
                        }

                        funs.insert(fun_name.clone(), (fun_tokens.as_slice().into(), is_inline));
                    }

                    "len" => {
                        let output = match self.pop() {
                            Ok(MaeelType::String(string)) => string.len(),
                            Ok(MaeelType::Array(xs)) => xs.len(),
                            Ok(other) => emit_error!(
                                file,
                                line,
                                format!("expected string or array, got {other:?}")
                            ),
                            Err(_) => emit_error!(file, line, "expected string or array, got EOS."),
                        };

                        self.push(MaeelType::Integer(output as i32))?
                    }

                    "get" => {
                        let index = expect_stack!(Integer, self, file, line) as usize;

                        match self.pop() {
                            Ok(MaeelType::Array(xs)) => self.push(
                                xs.get(index)
                                    .unwrap_or_else(|| {
                                        emit_error!(file, line, format!("unknown index: {index}"))
                                    })
                                    .clone(),
                            ),

                            Ok(MaeelType::String(string)) => self.push(MaeelType::String(
                                string
                                    .chars()
                                    .nth(index)
                                    .unwrap_or_else(|| {
                                        emit_error!(file, line, format!("unknown index: {index}"))
                                    })
                                    .to_string(),
                            )),

                            Ok(other) => emit_error!(file, line, format!("unindexable: {other:?}")),

                            _ => emit_error!(file, line, format!("unindexable: EOF")),
                        }?
                    }

                    "read" => {
                        let mut buf = vec![0u8; expect_stack!(Integer, self, file, line) as usize];

                        File::open(expect_stack!(String, self, file, line))?
                            .read_exact(&mut buf)?;

                        let content_bytes = buf
                            .iter()
                            .map(|byte| MaeelType::Integer(*byte as i32))
                            .collect();

                        self.push(MaeelType::Array(content_bytes))?
                    }

                    "include" => {
                        let target = expect_stack!(String, self, file, line);

                        if self.included.contains(&target) {
                            continue
                        }

                        self.included.push(target.clone());

                        let content = match target.clone().as_str() {
                            "std" => include_str!("../stdlib/std.maeel").to_string(),
                            "core" => include_str!("../stdlib/core.maeel").to_string(),
                            "logic" => include_str!("../stdlib/logic.maeel").to_string(),
                            "array" => include_str!("../stdlib/array.maeel").to_string(),
                            "fp" => include_str!("../stdlib/fp.maeel").to_string(),
                            "math" => include_str!("../stdlib/math.maeel").to_string(),
                            "string" => include_str!("../stdlib/string.maeel").to_string(),
                            "unix" => include_str!("../stdlib/unix.maeel").to_string(),
                            _ => read_to_string(&target).unwrap_or_else(|_| {
                                emit_error!(file, line, "failed to include file")
                            }),
                        };

                        lex_into_tokens(&content, &target)
                            .iter()
                            .rev()
                            .for_each(|token| tokens.push(token.clone()))
                    }

                    identifier => {
                        if let Some(value) = vars.get(identifier) {
                            self.push(value.clone())?;
                            continue
                        }

                        if let Some(fun) = funs.get(identifier) {
                            if fun.1 {
                                fun.0.iter().for_each(|token| tokens.push(token.clone()));
                                continue
                            }

                            let mut fun_tokens = fun.0.clone().to_vec();
                            fun_tokens.reverse();

                            self.process_tokens(&mut fun_tokens, &mut vars.clone(), funs)?;
                            continue
                        }

                        emit_error!(file, line, format!("unknown identifier {identifier}"))
                    }
                },
            };
        }

        Ok(())
    }

    fn push(&mut self, value: MaeelType) -> Result<(), Box<dyn Error>> {
        let future_head = Guitar::new(value);

        if !self.head.is_null() {
            unsafe {
                (*future_head).next = self.head;
            }
        }

        self.head = future_head;

        Ok(())
    }

    fn pop(&mut self) -> Result<MaeelType, Box<dyn Error>> {
        if self.head.is_null() {
            return Err("Stack is empty".into());
        }
        let current_head = unsafe { Box::from_raw(self.head) };
        self.head = current_head.next;
        Ok(current_head.value)
    }

    fn peek(&self) -> Result<&MaeelType, Box<dyn Error>> {
        if self.head.is_null() {
            return Err("Stack is empty".into());
        }

        Ok(unsafe { &(*self.head).value })
    }

    fn clear(&mut self) -> Result<(), Box<dyn Error>> {
        while !self.head.is_null() {
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
                        write!(f, " ").unwrap()
                    }

                    write!(f, "{}", x).unwrap()
                });

                write!(f, "}}")
            }
        }
    }
}

fn lex_into_tokens(code: &str, file: &str) -> Vec<TokenData> {
    let mut depth = 0;
    let mut line = 1;
    let mut tokens = Vec::default();
    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            '|' => {
                for character in characters.by_ref() {
                    if character == '\n' {
                        break
                    }
                }
            }

            '\n' => line += 1,

            ' ' | '\t' => continue,

            '(' => {
                tokens.push((Token::BlockStart, file, line));
                depth += 1
            }

            ')' => {
                tokens.push((Token::BlockEnd, file, line));
                depth -= 1
            }

            '"' => {
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
                                    emit_error!(
                                        file,
                                        line,
                                        format!("invalid escape sequence: \\{next_character}")
                                    )
                                }
                            }
                        }

                        ('\\', None) => emit_error!(file, line, "incomplete escape sequence"),

                        _ => character,
                    });
                }

                tokens.push((Token::String(content), file, line))
            }

            'a'..='z' | 'A'..='Z' | '_' => tokens.push((
                Token::Identifier(take_with_predicate!(character, characters, |&character| {
                    character.is_alphanumeric() || character == '_'
                })),
                file,
                line,
            )),

            '0'..='9' => {
                let content = take_with_predicate!(character, characters, |&character| {
                    character.is_ascii_digit() || character == '.' || character == '_'
                });

                if content.contains('.') {
                    tokens.push((Token::Float(content.parse().unwrap()), file, line));
                    continue
                }

                tokens.push((Token::Integer(content.parse().unwrap()), file, line))
            }

            _ => tokens.push((
                match character {
                    '+' => Token::BinaryOP(|a, b| b + a),
                    '-' => Token::BinaryOP(|a, b| b - a),
                    '*' => Token::BinaryOP(|a, b| b * a),
                    '/' => Token::BinaryOP(|a, b| b / a),
                    '%' => Token::BinaryOP(|a, b| b % a),
                    '=' => Token::BinaryOP(|a, b| MaeelType::Integer((b == a) as i32)),
                    '<' => Token::BinaryOP(|a, b| MaeelType::Integer((b < a) as i32)),
                    '>' => Token::BinaryOP(|a, b| MaeelType::Integer((b > a) as i32)),
                    '(' => Token::BlockStart,
                    ')' => Token::BlockEnd,
                    '{' => Token::ArrayStart,
                    '}' => Token::ArrayEnd,
                    '!' => Token::Call,
                    ':' => Token::Colon,
                    '@' => Token::At,
                    '?' => Token::Then,
                    '~' => Token::Assignment,
                    character => {
                        emit_error!(file, line, format!("found unknown char: {character}"))
                    }
                },
                file,
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
            (Token::BlockStart, _, _) => {
                stack.push(temporary_tokens);
                temporary_tokens = Vec::default();
            }

            (Token::BlockEnd, _, _) => {
                let nested_tokens = temporary_tokens.clone();

                match stack.pop() {
                    Some(previous_tokens) => {
                        temporary_tokens = previous_tokens;
                        temporary_tokens.push((Token::Block(nested_tokens), file.into(), line));
                    }

                    _ => output.push((Token::Block(nested_tokens), file.to_string(), line)),
                }
            }

            (other_token, other_file, other_line) => {
                temporary_tokens.push((other_token.clone(), other_file.to_string(), *other_line))
            }
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
    let file = args().nth(1).unwrap();
    BocchiVM::default().process_tokens(
        &mut lex_into_tokens(&read_to_string(&file)?, &file),
        &mut HashMap::default(),
        &mut HashMap::default(),
    )
}
