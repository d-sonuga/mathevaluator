use std::{vec, iter, fmt, process, collections::HashMap};
use std::{cell::RefCell, rc::Rc};
use std::io::{Write, self};

const ABOUT: &'static str = "
Evaluator
A simple math expression evaluator
Enter help or h for help
Enter \\q or exit to exit.
";

const HELP_MESSAGE: &'static str = "
This is a simple math expression evaluator that parses and evaluates
math expressions with (, ), *, ^, /, +, - and numbers.
You can also use variables with alphanumeric names that don't start
with numbers.
";

fn main(){
    println!("{}", ABOUT);
    let mut symtable: SymbolTable = HashMap::new();
    loop {
        prompt();
        let input: String = accept_input();
        if input == "exit" || input == "exi" || input == "\\q" {
            process::exit(0);
        }
        if input == "help" || input == "h" {
            println!("{}", HELP_MESSAGE);
            continue;
        }
        match tokenize(input.clone()) {
            Ok(tokens) => {
                if tokens.len() == 0 {
                    continue;
                }
                let mut parser = Parser::new(tokens);
                match parser.parse() {
                    Ok(expr) => {
                        let symtable = Rc::new(RefCell::new(&mut symtable));
                        match expr.evaluate(symtable){
                            Ok(eval) => println!("{:?}", eval),
                            Err(err) => println!("{:?}", err)
                        }
                    },
                    Err(err) => println!("{:?}", err)
                }
            },
            Err(err) => println!("{:?}", err)
        }
    }
}

fn prompt(){
    print!(">>> ");
    io::stdout().flush().unwrap();
}

fn accept_input() -> String {
    let mut input = String::new();
    match io::stdin().read_line(&mut input){
        Ok(_) => (),
        Err(_) => {
            println!("An unexpected error occured");
            process::exit(1);
        }
    };
    input.trim().to_string()
}

#[derive(Clone, PartialEq)]
enum Token {
    Plus,
    Minus,
    Slash,
    Star,
    Caret,
    OpenParen,
    CloseParen,
    NumLit(f64),
    Name(String),
    Assign
}

impl Token {
    fn get_type(&self) -> &'static str {
        match self {
            Token::Plus => T_PLUS,
            Token::Minus => T_MINUS,
            Token::Slash => T_SLASH,
            Token::Star => T_STAR,
            Token::Caret => T_CARET,
            Token::OpenParen => T_OPENPAREN,
            Token::CloseParen => T_CLOSEPAREN,
            Token::NumLit(_) => T_NUMLIT,
            Token::Name(_) => T_NAME,
            Token::Assign => T_ASSIGN
        }
    }
    fn binop_binding_power(&self) -> i32 {
        match self {
            Token::Plus | Token::Minus => ptable::BP_SUM,
            Token::Slash | Token::Star => ptable::BP_PRODUCT,
            Token::Caret => ptable::BP_EXPONENT,
            Token::Assign => ptable::BP_ASSIGN,
            _ => 0
        }
    }
    fn is_right_associative(&self) -> bool {
        match self {
            Token::Caret | Token::Assign => true,
            _ => false
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, _f: &mut fmt::Formatter) -> PrintResult {
        match self {
            Token::NumLit(num) => print!("{}", num),
            Token::Name(name) => print!("{}", name),
            _ => print!("{}", self.get_type())
        };
        Ok(())
    }
}

const T_PLUS: &'static str = "+";
const T_MINUS: &'static str = "-";
const T_SLASH: &'static str = "/";
const T_STAR: &'static str = "*";
const T_CARET: &'static str = "^";
const T_OPENPAREN: &'static str = "(";
const T_CLOSEPAREN: &'static str = ")";
const T_NUMLIT: &'static str = "numlit";
const T_NAME: &'static str = "name";
const T_ASSIGN: &'static str = "assign";

fn tokenize(input: String) -> Result<Vec<Token>, String> {
    let mut input = input.chars().peekable();
    let mut char_pos = 0;
    let mut tokens: Vec<Token> = vec![];
    let mut is_num = false;
    let mut curr_num = String::new();
    let mut curr_num_has_dot = false;
    let mut is_name = false;
    let mut curr_name = String::new();
    let summarize_num = |
        tokens: &mut Vec<Token>,
        curr_num: &mut String,
        is_num: &mut bool,
        curr_num_has_dot: &mut bool
    | {
        if !*is_num {
            return;
        }
        let num = curr_num.parse::<f64>().unwrap();
        tokens.push(Token::NumLit(num));
        *is_num = false;
        *curr_num = String::new();
        *curr_num_has_dot = false;
    };
    let summarize_name = |
        tokens: &mut Vec<Token>,
        curr_name: &mut String,
        is_name: &mut bool
    | {
        if !*is_name {
            return;
        }
        tokens.push(Token::Name(curr_name.to_string()));
        *is_name = false;
        *curr_name = String::new();
    };
    while input.peek().is_some() {
        char_pos += 1;
        let c = input.next().unwrap();
        match c {
            ' ' | '\n' | '\t' => continue,
            '-' => {
                summarize_num(&mut tokens, &mut curr_num, &mut is_num, &mut curr_num_has_dot);
                summarize_name(&mut tokens, &mut curr_name, &mut is_name);
                tokens.push(Token::Minus);
            },
            '+' => {
                summarize_num(&mut tokens, &mut curr_num, &mut is_num, &mut curr_num_has_dot);
                summarize_name(&mut tokens, &mut curr_name, &mut is_name);
                tokens.push(Token::Plus);
            },
            '/' => {
                summarize_num(&mut tokens, &mut curr_num, &mut is_num, &mut curr_num_has_dot);
                summarize_name(&mut tokens, &mut curr_name, &mut is_name);
                tokens.push(Token::Slash);
            },
            '*' => {
                summarize_num(&mut tokens, &mut curr_num, &mut is_num, &mut curr_num_has_dot);
                summarize_name(&mut tokens, &mut curr_name, &mut is_name);
                tokens.push(Token::Star);
            },
            '^' => {
                summarize_num(&mut tokens, &mut curr_num, &mut is_num, &mut curr_num_has_dot);
                summarize_name(&mut tokens, &mut curr_name, &mut is_name);
                tokens.push(Token::Caret);
            },
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                if is_name {
                    curr_name.push(c);
                } else {
                    if input.peek().is_some() && input.peek().unwrap().is_alphabetic(){
                        return Err(errors::err_invalid_name(char_pos));
                    } else {
                        is_num = true;
                        curr_num.push(c);
                    }
                }
            },
            '.' => {
                if is_num && !curr_num_has_dot && input.peek().is_some() {
                    match input.peek().unwrap() {
                        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                            curr_num.push(c);
                            curr_num_has_dot = true;
                        },
                        _ => {
                            return Err(format!("Invalid number at pos {}", char_pos));
                        }
                    }
                } else {
                    return Err(format!("Invalid number at pos {}", char_pos));
                }
            },
            '(' => {
                summarize_num(&mut tokens, &mut curr_num, &mut is_num, &mut curr_num_has_dot);
                summarize_name(&mut tokens, &mut curr_name, &mut is_name);
                tokens.push(Token::OpenParen);
            },
            ')' => {
                summarize_num(&mut tokens, &mut curr_num, &mut is_num, &mut curr_num_has_dot);
                summarize_name(&mut tokens, &mut curr_name, &mut is_name);
                tokens.push(Token::CloseParen);
            },
            '=' => {
                summarize_name(&mut tokens, &mut curr_name, &mut is_name);
                tokens.push(Token::Assign);
            }
            _ => {
                if c.is_alphabetic(){
                    is_name = true;
                    curr_name.push(c);
                } else {
                    return Err(format!("Unrecognized token at pos {}", char_pos));
                }
            }
        }
    }
    summarize_num(&mut tokens, &mut curr_num, &mut is_num, &mut curr_num_has_dot);
    summarize_name(&mut tokens, &mut curr_name, &mut is_name);
    Ok(tokens)
}

struct Parser {
    tokens: iter::Peekable<vec::IntoIter<Token>>,
    prefix_parselets: HashMap<&'static str, Box<dyn PrefixParselet>>,
    infix_parselets: HashMap<&'static str, Box<dyn InfixParselet>>,
    pos: u32
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        let mut parser = Parser {
            tokens: tokens.into_iter().peekable(),
            prefix_parselets: HashMap::new(),
            infix_parselets: HashMap::new(),
            pos: 1u32
        };
        parser.reg_prefix_parselet(T_NUMLIT, NumberParselet::new(ptable::BP_PREFIX));
        parser.reg_prefix_parselet(T_NAME, NameParselet::new(ptable::BP_PREFIX));
        parser.reg_prefix_parselet(T_MINUS, PrefixOpParselet::new(ptable::BP_PREFIX));
        parser.reg_prefix_parselet(T_PLUS, PrefixOpParselet::new(ptable::BP_PREFIX));

        parser.reg_infix_parselet(T_PLUS, BinOpParselet::new(ptable::BP_SUM));
        parser.reg_infix_parselet(T_MINUS, BinOpParselet::new(ptable::BP_SUM));
        parser.reg_infix_parselet(T_SLASH, BinOpParselet::new(ptable::BP_PRODUCT));
        parser.reg_infix_parselet(T_STAR, BinOpParselet::new(ptable::BP_PRODUCT));
        parser.reg_infix_parselet(T_CARET, BinOpParselet::new(ptable::BP_EXPONENT));
        parser.reg_infix_parselet(T_ASSIGN, BinOpParselet::new(ptable::BP_ASSIGN));

        parser
    }
    fn reg_prefix_parselet(&mut self, token_type: &'static str, parselet: Box<dyn PrefixParselet>){
        self.prefix_parselets.insert(token_type, parselet);
    }
    fn reg_infix_parselet(&mut self, token_type: &'static str, parselet: Box<dyn InfixParselet>){
        self.infix_parselets.insert(token_type, parselet);
    }
    fn parse(&mut self) -> PResult {
        self.parse_expression(0)
    }
    fn parse_expression(&mut self, binding_power: i32) -> PResult {
        let first_token;
        match self.tokens.next(){
            Some(token) => {
                first_token = token;
                self.pos += 1;
            },
            None => return Err(errors::err_expected_token(self.pos))
        };
        let mut left_expr;
        if first_token == Token::OpenParen {
            match self.parse_expression(0){
                Ok(expr) => left_expr = expr,
                Err(err) => return Err(err)
            }
        } else {
            let prefix_parselet;
            match self.prefix_parselets.get_mut(first_token.get_type()){
                Some(parselet) => prefix_parselet = (*parselet).clone(),
                None => return Err(errors::err_invalid_expr(self.pos))
            };
            match prefix_parselet.parse(self, first_token){
                Ok(expr) => left_expr = expr,
                Err(err) => return Err(err)
            };
        }
        loop {
            let next_token = self.tokens.peek();
            if next_token.is_none() {
                break;
            }
            if *next_token.unwrap() == Token::CloseParen {
                self.tokens.next();
                self.pos += 1;
                break;
            }
            if next_token.unwrap().binop_binding_power() > binding_power {
                let operator = self.tokens.next().unwrap();
                self.pos += 1;
                let infix_parselet;
                match self.infix_parselets.get_mut(operator.get_type()){
                    Some(parselet) => infix_parselet = (*parselet).clone(),
                    None => return Err(errors::err_invalid_bin_op(self.pos))
                };
                match infix_parselet.parse(self, left_expr, operator){
                    Ok(expr) => left_expr = expr,
                    Err(err) => return Err(err)
                };
            } else {
                break;
            }
        }
        Ok(left_expr)
    }
}

trait PrefixParselet: PrefixParseletClone {
    fn parse(&self, parser: &mut Parser, token: Token) -> PResult;
}

trait PrefixParseletClone {
    fn clone_parselet(&self) -> Box<dyn PrefixParselet>;
}

impl<T> PrefixParseletClone for T where T: 'static + Clone + PrefixParselet {
    fn clone_parselet(&self) -> Box<dyn PrefixParselet> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn PrefixParselet> {
    fn clone(&self) -> Box<dyn PrefixParselet> {
        self.clone_parselet()
    }
}

trait InfixParselet: InfixParseletClone {
    fn parse(&self, parser: &mut Parser, left: Box<dyn Expression>, token: Token) -> PResult;
}

trait InfixParseletClone {
    fn clone_parselet(&self) -> Box<dyn InfixParselet>;
}

impl<T> InfixParseletClone for T where T: 'static + Clone + InfixParselet {
    fn clone_parselet(&self) -> Box<dyn InfixParselet> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn InfixParselet> {
    fn clone(&self) -> Box<dyn InfixParselet> {
        self.clone_parselet()
    }
}

trait Expression: fmt::Debug + ExpressionClone {
    fn evaluate(&self, symtable: Rc<RefCell<&mut SymbolTable>>) -> EvalResult;
    fn repr(&self) -> String;
}

trait ExpressionClone {
    fn clone_expression(&self) -> Box<dyn Expression>;
}

impl<T> ExpressionClone for T where T: 'static + Expression + Clone {
    fn clone_expression(&self) -> Box<dyn Expression> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Expression> {
    fn clone(&self) -> Box<dyn Expression> {
        self.clone_expression()
    }
}

#[derive(Clone)]
struct NumberParselet {}

impl PrefixParselet for NumberParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> PResult {
        if let Token::NumLit(num) = token {
            Ok(Box::new(NumberExpression {
                num
            }))
        } else {
            Err(format!("Invalid number"))
        }
    }
}

impl NumberParselet {
    fn new(_binding_power: i32) -> Box<dyn PrefixParselet> {
        Box::new(NumberParselet {})
    }
}

#[derive(Clone)]
struct NameParselet {
    binding_power: i32
}

impl PrefixParselet for NameParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> PResult {
        if let Token::Name(name) = token {
            Ok(Box::new(NameExpression {
                name
            }))
        } else {
            Err(errors::err_invalid_name(parser.pos))
        }
    }
}

impl NameParselet {
    fn new(binding_power: i32) -> Box<NameParselet> {
        Box::new(NameParselet {
            binding_power
        })
    }
}

#[derive(Clone)]
struct PrefixOpParselet {
    binding_power: i32
}

impl PrefixParselet for PrefixOpParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> PResult {
        match parser.parse_expression(self.binding_power){
            Ok(operand) => Ok(Box::new(PrefixOpExpression::new(token, operand))),
            Err(err) => Err(err)
        }
    }
}

impl PrefixOpParselet {
    fn new(binding_power: i32) -> Box<PrefixOpParselet> {
        Box::new(PrefixOpParselet {
            binding_power
        })
    }
}

#[derive(Clone)]
struct BinOpParselet {
    binding_power: i32
}

impl InfixParselet for BinOpParselet {
    fn parse(&self, parser: &mut Parser, left: Box<dyn Expression>, operator: Token) -> PResult {
        match parser.parse_expression(self.associate(&operator)) {
            Ok(right_expr) => Ok(Box::new(BinOpExpression {
                left,
                right: right_expr,
                operator
            })),
            Err(err) => Err(err)
        }
    }
}

impl BinOpParselet {
    fn new(binding_power: i32) -> Box<BinOpParselet> {
        Box::new(BinOpParselet{
            binding_power
        })
    }
    fn associate(&self, operator: &Token) -> i32 {
        if operator.is_right_associative(){
            self.binding_power - 1
        } else {
            self.binding_power
        }
    }
}

#[derive(Clone)]
struct NumberExpression {
    num: f64
}

impl Expression for NumberExpression {
    fn evaluate(&self, _symtable: Rc<RefCell<&mut SymbolTable>>) -> EvalResult {
        Ok(self.num)
    }
    fn repr(&self) -> String {
        format!("(num: {})", self.num)
    }
}

impl fmt::Debug for NumberExpression {
    fn fmt(&self, _f: &mut fmt::Formatter) -> PrintResult {
        print!("(num: {})", self.num);
        Ok(())
    }
}

#[derive(Clone)]
struct NameExpression {
    name: String
}

impl Expression for NameExpression {
    fn evaluate(&self, symtable: Rc<RefCell<&mut SymbolTable>>) -> EvalResult {
        match symtable.borrow().get(&self.repr()){
            Some(val) => Ok(*val),
            None => Err(errors::err_undeclared_ident())
        }
    }
    fn repr(&self) -> String {
        format!("{}", self.name)
    }
}

impl fmt::Debug for NameExpression {
    fn fmt(&self, _f: &mut fmt::Formatter) -> PrintResult {
        print!("{:?}", self.name);
        Ok(())
    }
}

#[derive(Clone)]
struct PrefixOpExpression {
    operand: Box<dyn Expression>,
    operator: Token
}

impl Expression for PrefixOpExpression {
    fn evaluate(&self, symtable: Rc<RefCell<&mut SymbolTable>>) -> EvalResult {
        let operand_eval;
        match self.operand.evaluate(Rc::clone(&symtable)){
            Ok(eval) => operand_eval = eval,
            Err(err) => return Err(err)
        };
        Ok(match self.operator {
            Token::Minus => -operand_eval,
            Token::Plus => operand_eval,
            _ => {
                print!("It's impossible to reach this point because it");
                println!("has been covered in the parsing phase");
                0.0
            }
        })
    }
    fn repr(&self) -> String {
        format!("(operator: {:?}, operand: {:?})", self.operator, self.operand)
    }
}

impl fmt::Debug for PrefixOpExpression {
    fn fmt(&self, _f: &mut fmt::Formatter) -> PrintResult {
        print!("(operator: {:?}, operand: {:?})", self.operator, self.operand);
        Ok(())
    }
}

impl PrefixOpExpression {
    fn new(token: Token, operand: Box<dyn Expression>) -> PrefixOpExpression {
        PrefixOpExpression {
            operand,
            operator: token
        }
    }
}

#[derive(Clone)]
struct BinOpExpression {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
    operator: Token
}

impl Expression for BinOpExpression {
    fn evaluate(&self, symtable: Rc<RefCell<&mut SymbolTable>>) -> EvalResult {
        let left_operand;
        let right_operand;
        if self.operator != Token::Assign {
            match self.left.evaluate(Rc::clone(&symtable)){
                Ok(eval) => left_operand = eval,
                Err(err) => return Err(err)
            };
        } else {
            // Assignment operators don't use the left operand, so it doesn't matter what's here
            left_operand = 0.0;
        }
        match self.right.evaluate(Rc::clone(&symtable)){
            Ok(eval) => right_operand = eval,
            Err(err) => return Err(err)
        };
        Ok(match self.operator {
            Token::Plus => left_operand + right_operand,
            Token::Minus => left_operand - right_operand,
            Token::Slash => left_operand / right_operand,
            Token::Star => left_operand * right_operand,
            Token::Caret => left_operand.powf(right_operand),
            Token::Assign => {
                // evaluate what's on the right before assigning
                symtable.borrow_mut().insert(
                    self.left.repr(),
                    right_operand
                );
                right_operand
            },
            _ => {
                print!("It's impossible to reach this point because it");
                println!("has been covered in the parsing phase");
                0.0
            }
        })
    }
    fn repr(&self) -> String {
        format!("(left: ({:?}), operator: ({:?}), right: ({:?}))", self.left, self.operator, self.right)
    }
}

impl fmt::Debug for BinOpExpression {
    fn fmt(&self, _f: &mut fmt::Formatter) -> PrintResult {
        print!("(left: ({:?}), operator: ({:?}), right: ({:?}))", self.left, self.operator, self.right);
        Ok(())
    }
}

type PResult = Result<Box<dyn Expression>, String>;
type EvalResult = Result<f64, String>;
type PrintResult = Result<(), fmt::Error>;
type SymbolTable = HashMap<String, f64>;

mod ptable {
    pub const BP_PREFIX: i32 = 50;
    pub const BP_EXPONENT: i32 = 40;
    pub const BP_PRODUCT: i32 = 30;
    pub const BP_SUM: i32 = 20;
    pub const BP_ASSIGN: i32 = 10;
}

mod errors {
    pub fn err_expected_token(pos: u32) -> String {
        format!("Expected a token at pos {}", pos)
    }
    pub fn err_invalid_expr(pos: u32) -> String {
        format!("Invalid expression at pos {}", pos)
    }
    pub fn err_invalid_bin_op(pos: u32) -> String {
        format!("Invalid binary operator at pos {}", pos)
    }
    pub fn err_invalid_name(pos: u32) -> String {
        format!("Invalid name at pos {}", pos)
    }
    pub fn err_undeclared_ident() -> String {
        format!("Undeclared identifier")
    }
}