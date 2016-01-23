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
        match self.input_parse(input) {
            Ok(v) => {
                for i in v.into_iter() {
                    match i {
                        Item::Exec_(s) => match s {
                            Exec::Arith_(o) => {
                                let (a, b) = match (self.stack.pop(), self.stack.pop()) {
                                    (Some(a), Some(b)) => (a, b),
                                    (_, _) => return Err(Error::StackUnderflow),
                                };
                                match eval_oper(a, b, o) {
                                    Ok(v) => self.stack.push(v),
                                    Err(e) => return Err(e),
                                }
                            },
                            Exec::Stack_(c) => {
                                try!(eval_command(&mut self.stack, c));
                            },
                            Exec::Value_(v) => {
                                self.stack.push(v);
                            },
                        },
                        _ => (),
                    }
                }
            },
            Err(e) => return Err(e),
        }
        Ok(())
    }

    fn input_parse(&mut self, input: &str) -> Result<Vec<Item>, Error> {
        let mut items = vec![];
        let mut state = ParseState::Normal;
        let mut curr_custom_word = String::new();

        let input_uppercased = &input.to_uppercase() as &str;
        let input_separated = to_space_separated(input_uppercased.clone());
        let input_split = input_separated.split_whitespace().collect::<Vec<&str>>();

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
        match s.parse::<Value>() {
            Ok(v) => Ok(vec![Item::Exec_(Exec::Value_(v))].into_iter().collect()),
            Err(_) => {
                match self.word_map.get(&s.to_uppercase()) {
                    Some(w) => Ok((*w).clone()),
                    None    => Err(Error::UnknownWord),
                }
            }
        }
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
        StackWord::Dup => {
            let a = match stack.last() {
                Some(&a) => a,
                _ => return Err(Error::StackUnderflow),
            };
            stack.push(a);
        },
        StackWord::Drop => {
            match stack.pop() {
                Some(_) => (),
                _ => return Err(Error::StackUnderflow),
            }
        },
        StackWord::Swap => {
            let (a, b) = match (stack.pop(), stack.pop()) {
                (Some(a), Some(b)) => (a, b),
                (_, _) => return Err(Error::StackUnderflow),
            };
            stack.push(a);
            stack.push(b);
        },
        StackWord::Over => {
            let len = stack.len();
            if len < 2 { return Err(Error::StackUnderflow) };
            let a = match stack.get(len - 2) {
                Some(&a) => a,
                _ => return Err(Error::StackUnderflow),
            };
            stack.push(a);
        },
    }
    Ok(())
}

fn to_space_separated(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_control() { ' ' } else { c })
        .collect()
}
