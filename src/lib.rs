use std::collections::HashMap;

pub type Value = i32;
pub type ForthResult = Result<(), Error>;

#[derive(Debug, PartialEq, Clone, Copy)]
enum ArithWord { Add, Sub, Mul, Div }

#[derive(Debug, PartialEq, Clone, Copy)]
enum StackWord { Dup, Drop, Swap, Over }

#[derive(Debug, PartialEq, Clone, Copy)]
enum Symbol { Colon, SemiColon }

#[derive(Debug, PartialEq, Clone, Copy)]
enum Item {
    Exec_(Exec),
    Symbol_(Symbol),
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Exec {
    Arith_(ArithWord),
    Stack_(StackWord),
    Value_(Value),
}

fn make_word_map() -> HashMap<String, Vec<Item>> {
    let mut m = HashMap::new();
        m.insert("DUP".to_owned(),  vec![Item::Exec_(Exec::Stack_(StackWord::Dup))]);
        m.insert("DROP".to_owned(), vec![Item::Exec_(Exec::Stack_(StackWord::Drop))]);
        m.insert("SWAP".to_owned(), vec![Item::Exec_(Exec::Stack_(StackWord::Swap))]);
        m.insert("OVER".to_owned(), vec![Item::Exec_(Exec::Stack_(StackWord::Over))]);
        m.insert("+".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Add))]);
        m.insert("-".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Sub))]);
        m.insert("*".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Mul))]);
        m.insert("/".to_owned(),    vec![Item::Exec_(Exec::Arith_(ArithWord::Div))]);
        m.insert(":".to_owned(),    vec![Item::Symbol_(Symbol::Colon)]);
        m.insert(";".to_owned(),    vec![Item::Symbol_(Symbol::SemiColon)]);
        m
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum ParseState {
    Normal,         // Parse into existing words
    CustomInit,     // This item is the name of re-defined word
    Custom,         // This item is the body of re-defined word
}


pub struct Forth {
    word_map: HashMap<String, Vec<Item>>,
    stack: Vec<Value>,
}

impl Forth {
    pub fn new() -> Forth {
        Forth {
            word_map: make_word_map(),
            stack: vec![],
        }
    }

    pub fn format_stack(&self) -> String {
        let mut stack_str = String::new();
        for v in self.stack.iter() {
            stack_str.push_str(&v.to_string());
            stack_str.push_str(" ");
        }
        stack_str = stack_str.trim().to_owned();
        stack_str
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        self.input_parse(input)
            .and_then(|v| v.into_iter().map(|i| {
                match i {
                    Item::Exec_(Exec::Arith_(o)) => 
                        self.stack.pop()
                            .and_then(|a| self.stack.pop().map(|b|(a,b)))
                            .ok_or(Error::StackUnderflow)
                            .and_then(|(a,b)| eval_oper(a, b, o))
                            .map(|v| self.stack.push(v)),
                    Item::Exec_(Exec::Stack_(c)) =>
                        eval_command(&mut self.stack, c),
                    Item::Exec_(Exec::Value_(v)) =>
                        Ok(self.stack.push(v)),
                    _ => Ok(())
                }
            }).fold(Ok(()), Result::and))
    }

    fn input_parse(&mut self, input: &str) -> Result<Vec<Item>, Error> {
        let mut items = vec![];
        let mut state = ParseState::Normal;
        let mut curr_custom_word = String::new();

        let input = input.chars()
            .map(|c| if c.is_control() { ' ' } else { c })
            .flat_map(char::to_uppercase)
            .collect::<String>();
        let input_split = input.split_whitespace()
            .collect::<Vec<_>>();

        for item_str in input_split.iter() {
            match state {
                ParseState::Normal => {
                    match self.str_to_item(item_str.clone().to_owned()) {
                        Ok(v) => {
                            let first_item = try!(v.last().clone().ok_or(Error::InvalidWord));

                            if first_item == &Item::Symbol_(Symbol::Colon) {
                                state = ParseState::CustomInit;
                            } else {
                                for i in v.iter() {
                                    items.push((*i).clone());
                                }
                            }
                        },
                        Err(e) => return Err(e),
                    }
                },
                ParseState::CustomInit => {
                    // Cannot re-define numbers
                    match self.str_to_item(item_str.clone().to_owned()) {
                        Ok(v) => {
                            let first_item = try!(v.last().clone().ok_or(Error::InvalidWord));

                            match first_item {
                                &Item::Exec_(Exec::Value_(_)) => return Err(Error::InvalidWord),
                                _ => (),
                            }
                        },
                        _ => (),
                    }

                    curr_custom_word = item_str.clone().to_owned();
                    self.word_map.insert(curr_custom_word.clone(), vec![]);

                    state = ParseState::Custom;
                },
                ParseState::Custom => {
                    match self.str_to_item(item_str.clone().to_owned()) {
                        Ok(v) => {
                            let first_item = try!(v.last().clone().ok_or(Error::InvalidWord));

                            if first_item == &Item::Symbol_(Symbol::SemiColon) {
                                state = ParseState::Normal;
                            } else {
                                match self.word_map.get_mut(&curr_custom_word.clone()) {
                                    Some(w) => {
                                        for i in v.iter() {
                                            w.push((*i).clone());
                                        }
                                    },
                                    None => (),
                                }
                            }
                        },
                        Err(e) => return Err(e),
                    }
                },
            }
        }

        match state {
            ParseState::Normal => Ok(items),
            _ => Err(Error::InvalidWord),
        }
    }

    fn str_to_item(&self, s: String) -> Result<Vec<Item>, Error> {
        s.parse()
            .map(Exec::Value_)
            .map(Item::Exec_)
            .map(|e|vec![e])
            .ok()
            .or_else(|| self.word_map.get(&s.to_uppercase()).cloned())
            .ok_or(Error::UnknownWord)
    }
}

fn eval_oper(a: Value, b: Value, o: ArithWord) -> Result<Value, Error> {
    match o {
        ArithWord::Add => Ok(b + a),
        ArithWord::Sub => Ok(b - a),
        ArithWord::Mul => Ok(b * a),
        ArithWord::Div => {
            match a {
                0 => Err(Error::DivisionByZero),
                a => Ok(b / a),
            }
        },
    }
}

fn eval_command(stack: &mut Vec<Value>, c: StackWord) -> ForthResult {
    match c {
        StackWord::Dup =>
            stack.pop()
                .ok_or(Error::StackUnderflow)
                .map(|a| {
                    stack.push(a);
                    stack.push(a);
                }),
        StackWord::Drop =>
            stack.pop()
                .ok_or(Error::StackUnderflow)
                .map(drop),
        StackWord::Swap =>
            stack.pop()
                .and_then(|b| stack.pop().map(|a|(a,b)))
                .ok_or(Error::StackUnderflow)
                .map(|(a,b)| { stack.push(b); stack.push(a) }),
        StackWord::Over =>
            stack.pop()
                .and_then(|b| stack.pop().map(|a|(a,b)))
                .ok_or(Error::StackUnderflow)
                .map(|(a,b)| {
                    stack.push(a);
                    stack.push(b);
                    stack.push(a);
                })
    }
}
