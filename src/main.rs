use hashbrown::HashMap;

use std::env::args;
use std::error::Error;
use std::fs::read_to_string;
use std::fs::File;
use std::io::Read;
use std::iter::once;
use std::ptr;
use std::rc::Rc;

/* A token and its line */
type TokenData = (Token /* Token */, u16 /* Line */);

/* Function type */
type Fun = (
    Rc<[TokenData]>, /* (Token, Line)s */
    bool,            /* Inline descriptor */
);

/* Binary VM application */
type BinApp = fn(MaeelType /* LHS */, MaeelType /* RHS */) -> MaeelType;

/* Default VM function output */
type VMOutput<T> = Result<T, Box<dyn Error>>;

/* Build an empty hashmap (1) */
macro_rules! empty_hashmap {
    () => {
        hashbrown::HashMap::default()
    };
}

/* Build an empty vec (1) */
macro_rules! empty_vec {
    () => {
        Vec::default()
    };
}

/* So we don't need to change every line (future usage of libraries?) */

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

#[derive(Clone, Debug /* For error messages */)]
enum Token {
    Block(Vec<TokenData>), /* (...) */
    String(String),        /* "abc" */
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
}

impl Default for BocchiVM {
    fn default() -> Self {
        BocchiVM {
            head: ptr::null_mut(),
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

    fn parse_array(
        &mut self,
        tokens: &mut Vec<TokenData>,
        vars: &mut HashMap<String, MaeelType>,
    ) -> VMOutput<()> {
        /* Array content */
        let mut xs = empty_vec!();

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

                Token::String(_) | Token::Integer(_) | Token::Float(_) | Token::Block(_) => {
                    xs.push(temporary_token.into())
                }

                Token::Identifier(identifier) => match vars.get(&identifier) {
                    Some(value) => xs.push(value.clone()),

                    _ => {
                        panic!("{temporary_line}: unknown identifier found while parsing array: {identifier} (maybe store it inside a variable ?)")
                    }
                },

                _ => panic!("{temporary_line}: unknown token found while parsing array"),
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
            let (token, line) = (token_data.0, token_data.1);

            match token {
                Token::BlockStart | Token::BlockEnd | Token::ArrayEnd =>
                /* Should not be there... */
                {
                    panic!("{line}: syntax error")
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

                Token::String(_) | Token::Block(_) | Token::Float(_) | Token::Integer(_) => {
                    self.push(token.into())?
                }

                Token::Dot =>
                /* Access structures members */
                {
                    match self.pop() {
                        Ok(MaeelType::Structure(structure)) => {
                            let structure_member = structure.get(&match tokens.pop() {
                            Some((Token::Identifier(value), _)) => value,
                            Some((other, other_line)) => {
                                panic!("{other_line}: expected an identifier after dot; got {other:?} instead.")
                            }
                            None => panic!("{line}: expected an identifier after dot!"),
                        });

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

                        other => panic!(
                        "{line}: found a 'dot' but no structure on the stack; got {other:?} instead"
                    ),
                    }
                }

                Token::Colon =>
                /* Use functions as first class objects */
                {
                    /* Function name */
                    let fun_name = match tokens.pop() {
                        Some((Token::Identifier(value), _)) => value,
                        Some((other, other_line)) => panic!("{other_line}: every colon must be followed by a function name; got {other:?} instead"),
                        None => panic!("{line}: every colon must be followed by a function name."),
                    };

                    /* Function object */
                    let fun = funs.get(&fun_name).unwrap();

                    /* Push the function codeblock */
                    self.push(MaeelType::Function((
                        fun.0.clone(), /* Function tokens */
                        fun.1,         /* Function inline descriptor */
                    )))?
                }

                Token::Call =>
                /* Manually call a function */
                {
                    /* Function object */
                    let fun = match self.pop() {
                        Ok(MaeelType::Function(value)) => value,
                        _ => panic!("{line}: tried to call something else than a function!"),
                    };

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
                            None => panic!("{line}: expected something after '=>'"),
                        },
                        Ok(False!()) => { /* Do nothing */ }
                        Ok(other) => panic!("{line}: '=>' expects a boolean (0 or 1) on the stack; got {other:?} instead."),
                        Err(_) => panic!("{line}: '=>' expects a boolean (0 or 1) on the stack.")
                    }
                }

                Token::Assignment =>
                /* MaeelType <-> identifier */
                {
                    let name = match tokens.pop() {
                        Some((Token::Identifier(value), _)) => value,
                        Some((other, other_line)) => panic!("{other_line}: expected an identifier after '->'; got {other:?} instead."),
                        None => panic!("{line}: expected an identifier after '->'."),
                    };

                    vars.insert(name, self.pop()?);
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

                        "break" =>
                        /* Stop processing the tokens */
                        {
                            break
                        } /* TODO: rebuild this */

                        "clear" =>
                        /* Process "clear" VM operation */
                        {
                            self.clear()
                                .unwrap_or_else(|_| panic!("{line}: failed to clear stack!"))
                        }

                        "drop" =>
                        /* Process "fastpop" VM operation */
                        {
                            self.fastpop()
                                .unwrap_or_else(|_| panic!("{line}: failed to drop"))
                        }

                        "dup" =>
                        /* Process "dup" VM operation */
                        {
                            self.dup()
                                .unwrap_or_else(|_| panic!("{line}: failed to dup"))
                        }

                        "swap" =>
                        /* Process "swap" VM operation */
                        {
                            self.swap()
                                .unwrap_or_else(|_| panic!("{line}: failed to swap"))
                        }

                        "over" =>
                        /* Process "over" VM operation */
                        {
                            self.over()
                                .unwrap_or_else(|_| panic!("{line}: failed to over"))
                        }

                        "rot" =>
                        /* Process "rotate" VM operation */
                        {
                            self.rot()
                                .unwrap_or_else(|_| panic!("{line}: failed to rotate"))
                        }

                        /* For loop implementation */
                        "for" => {
                            /* Code block to execute at each iteration */
                            let temporary_tokens = match tokens.pop() {
                                Some((Token::Block(value), _)) => value,
                                Some((other, other_line)) => {
                                    panic!("{other_line}: expected a code block after 'for'; got {other:?} instead.")
                                }
                                None => {
                                    panic!("{line}: expected a code block after 'for'!")
                                }
                            };

                            /* Determine what to iterate through */
                            match self.pop() {
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

                                _ => panic!(),
                            }
                        }

                        /* While loop implementation */
                        "while" => {
                            /* Code block to execute at each iteration */
                            let temporary_tokens = match tokens.pop() {
                                Some((Token::Block(value), _)) => value,
                                Some((other, other_line)) => {
                                    panic!("{other_line}: expected a code block after 'while'; got {other:?} instead.")
                                }
                                None => {
                                    panic!("{line}: expected a code block after 'while'!")
                                }
                            };

                            /* Determine if we continue looping or not */
                            while match self.pop() {
                                Ok(True!()) => true,                             /* Continue looping */
                                Ok(False!()) => false,                           /* Stop looping */
                                _ => panic!("{line}: no boolean on the stack!"), /* No boolean on the stack */
                            } {
                                self.process_tokens(
                                    &mut temporary_tokens.clone(),
                                    vars,
                                    funs,
                                    structs,
                                )?
                            }
                        }

                        "struct" => {
                            let struct_name = match tokens.pop() {
                                Some((Token::Identifier(value), _)) => value,
                                Some((other, other_line)) => panic!("{other_line}: expected identifier after 'struct'; got {other:?} instead."),
                                _ => panic!("{line}: expected identifier after 'struct'."),
                            };

                            /* Structure attributes */
                            let mut struct_fields = empty_vec!();

                            while let Some(temporary_tokens) = tokens.pop() {
                                match temporary_tokens {
                                (Token::Dot, _) =>
                                /* Stop parsing structure fields on '.' */
                                {
                                    break;
                                }

                                (Token::Identifier(identifier), _) => {
                                    struct_fields.push(identifier);
                                }

                                (other, other_line) => panic!("{other_line}: expected identifier(s) or dot after 'struct {struct_name}'; got {other:?} instead."),
                            }
                            }

                            struct_fields.reverse();

                            structs.insert(struct_name, struct_fields.as_slice().into());
                        }

                        "fun" => {
                            let mut fun_name = match tokens.pop() {
                                Some((Token::Identifier(value), _)) => value,
                                Some((other, other_line)) => {
                                    panic!("{other_line}: expected an identifier after 'fun'; got {other:?} instead.")
                                }
                                None => {
                                    panic!("{line}: expected an identifier after 'fun'.")
                                }
                            };

                            let mut is_inline = false;

                            if fun_name == "inline" {
                                is_inline = true;

                                fun_name = match tokens.pop() {
                                    Some((Token::Identifier(value), _)) => value,
                                    Some((other, other_line)) => {
                                        panic!("{other_line}: expected an identifier after 'fun inline'; got {other:?} instead.")
                                    }
                                    None => {
                                        panic!("{line}: expected an identifier after 'fun inline'.")
                                    }
                                }
                            }

                            let mut fun_tokens = empty_vec!(); /* Final tokens */

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
                                        fun_tokens.push((Token::Assignment, line));
                                    }

                                    (other, other_line) => {
                                        panic!("{other_line}: expected identifier(s) or a code block after 'fun {fun_name}'; got {other:?} instead.")
                                    }
                                }
                            }

                            funs.insert(
                                fun_name.clone(),
                                (fun_tokens.as_slice().into(), is_inline),
                            );
                        }

                        "get" => {
                            let index = match self.pop() {
                                Ok(MaeelType::Integer(value)) => value as usize,
                                _ => panic!(),
                            };

                            match self.pop() {
                                Ok(MaeelType::Array(xs)) => {
                                    self.push(xs.get(index).unwrap().clone())
                                }

                                Ok(MaeelType::String(string)) => self.push(MaeelType::String(
                                    string.chars().nth(index).unwrap().to_string(),
                                )),

                                Ok(other) => panic!("{other} is not indexable!"),

                                _ => panic!("Nothing to index!"),
                            }?
                        }

                        "read" => {
                            let bytes = match self.pop() {
                                Ok(MaeelType::Integer(value)) => value,

                                _ => panic!(),
                            };

                            let path = match self.pop() {
                                Ok(MaeelType::String(value)) => value,

                                _ => panic!(),
                            };

                            assert!(bytes >= 0);

                            let mut buf = vec![0u8; bytes as usize]; /* empty buffer */

                            File::open(path)?.read_exact(&mut buf)?; /* file content -> buffer */

                            self.push(MaeelType::Array(
                                buf.iter()
                                    .map(|byte| MaeelType::Integer(*byte as i32))
                                    .collect(),
                            ))?
                        }

                        "include" => {
                            let target = match self.pop() {
                                Ok(MaeelType::String(value)) => value,
                                _ => panic!(),
                            };

                            let content = match target.as_str() {
                                /* Standard library is included at compile time. We prefer memory
                                usage over CPU usage... */
                                "std" => include_str!("../stdlib/std.maeel").to_string(),

                                _ => read_to_string(target)
                                    .unwrap_or_else(|_| panic!("{line}: failed to include file")),
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

                                /* Turn fun tokens into a vec so we can use
                                reverse() later */
                                let mut fun_tokens = fun.0.clone().to_vec();

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
                                /* Future structure */
                                let mut structure = HashMap::with_capacity(fields.len());

                                /* Map each field to a value of the stack */
                                fields.iter().for_each(|key| {
                                    structure.insert(key.clone(), self.pop().unwrap());
                                });

                                /* Finally, push the structure */
                                self.push(MaeelType::Structure(structure))?;

                                continue;
                            }

                            panic!("{line}: unknown identifier {identifier}")
                        }
                    }
                }
            };
        }

        Ok(())
    }

    fn push(&mut self, value: MaeelType) -> VMOutput<()> {
        let future_head = Guitar::new(value); /* Create a new node that contains `value` */

        if !self.head.is_null() {
            unsafe {
                /* Set head as future_head next node */
                (*future_head).next = self.head;
            }
        }

        self.head = future_head; /* Replace head with future_head */

        Ok(())
    }

    fn pop(&mut self) -> VMOutput<MaeelType> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        let current_head = unsafe { Box::from_raw(self.head) };

        /* Replace the current head with her next node */
        self.head = current_head.next;

        Ok(current_head.value)
    }

    fn fastpop(&mut self) -> VMOutput<()> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        let current_head = unsafe { Box::from_raw(self.head) };

        /* Replace the current head with her next node */
        self.head = current_head.next;

        Ok(())
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

    fn swap(&mut self) -> VMOutput<()> {
        if self.head.is_null() || unsafe { (*self.head).next.is_null() }
        /* Making sure the stack contains at least two values */
        {
            return Err("Not enough elements on the stack".into());
        }

        unsafe {
            ptr::swap(&mut (*self.head).value, &mut (*(*self.head).next).value);
        }

        Ok(())
    }

    fn dup(&mut self) -> VMOutput<()> {
        if self.head.is_null()
        /* Making sure the stack contains at least one value */
        {
            return Err("Stack is empty".into());
        }

        self.push(unsafe { (*self.head).value.clone() })
    }

    fn over(&mut self) -> VMOutput<()> {
        if self.head.is_null() || unsafe { (*self.head).next.is_null() }
        /* Making sure the stack contains at least two values */
        {
            return Err("Stack has less than two elements".into());
        }

        self.push(
            /* Get the value under the stack top */
            unsafe { (*(*self.head).next).value.clone() },
        )
    }

    fn rot(&mut self) -> VMOutput<()> {
        if self.head.is_null()
            || unsafe { (*self.head).next.is_null() }
            || unsafe { (*(*self.head).next).next.is_null() }
        /* Making sure the stack contains at least three values */
        {
            return Err("Stack has less than three elements".into());
        }

        unsafe {
            let node1 /* Top node */ = &mut *self.head;
            let node2 /* Mid node */ = &mut *(*self.head).next;
            let node3 /* Bot node */ = &mut *(*(*self.head).next).next;

            /* Store the node1 value in a temp variable,
            as we update its value first */
            let temp = ptr::read(&node1.value);

            ptr::swap /* V(top) <- V(mid) */ (&mut node1.value, &mut node2.value);
            ptr::swap /* V(mid) <- V(bot) */ (&mut node2.value, &mut node3.value);
            ptr::write /* V(bot) <- V(top) */ (&mut node3.value, temp);
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
fn lex_into_tokens(code: &str) -> Vec<TokenData> {
    let mut depth = 0;
    let mut line = 1;
    let mut tokens = empty_vec!();
    let mut characters = code.chars().peekable();

    while let Some(character) = characters.next() {
        match character {
            /* Parse comments */
            '|' => {
                for character in characters.by_ref() {
                    if character == '\n'
                    /* Comment ends at end-of-line */
                    {
                        break;
                    }
                }
            }

            '\n' => line += 1,

            /* Ignore whitespaces */
            ' ' | '\t' => continue,

            /* Code block start */
            '(' => {
                tokens.push((Token::BlockStart, line));
                depth += 1;
            }

            /* Code block end */
            ')' => {
                tokens.push((Token::BlockEnd, line));
                depth -= 1;
            }

            /* Parse strings */
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
                                    panic!(
                                        "{line}: invalid escape \
                                                 sequence: \\{}",
                                        next_character
                                    )
                                }
                            }
                        }

                        ('\\', None) => {
                            panic!("{line}: incomplete escape sequence")
                        }

                        _ => character,
                    });
                }

                tokens.push((Token::String(content), line))
            }

            /* Parse identifiers */
            'a'..='z' | 'A'..='Z' | '_' => tokens.push((
                Token::Identifier(take_with_predicate!(character, characters, |&character| {
                    character.is_alphanumeric() || character == '_'
                })),
                line,
            )),

            /* Parse numerics (float/integers) */
            '0'..='9' => {
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

            /* Parse equal/then */
            '=' => match characters.peek() {
                Some('>') =>
                /* Then */
                {
                    tokens.push((Token::Then, line));
                    characters.next();
                }

                Some(':') =>
                /* Assignment */
                {
                    tokens.push((Token::Assignment, line));
                    characters.next();
                }

                _ =>
                /* Equal */
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
                    /* Assignment*/
                    {
                        tokens.push((Token::Assignment, line));
                        characters.next();
                    }

                    _ =>
                    /* Minus */
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

                    character => panic!("{line}: found unknown char: {character}"),
                },
                line,
            )),
        }
    }

    assert_eq!(depth, 0);

    let mut stack = empty_vec!();
    let mut output = empty_vec!();
    let mut temporary_tokens = empty_vec!();

    for token in tokens.iter() {
        match token {
            (Token::BlockStart, _) => {
                stack.push(temporary_tokens);

                /* Clear the temporary tokens */
                temporary_tokens = empty_vec!();
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
        &mut empty_hashmap!(),
        &mut empty_hashmap!(),
        &mut empty_hashmap!(),
    )
}
