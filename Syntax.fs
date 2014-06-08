module Syntax

type Identifier = string

type ArithExp =
    | Var of Identifier
    | Int of int
    | Addition of ArithExp * ArithExp
    | Subtraction of ArithExp * ArithExp
    | Multiply of ArithExp * ArithExp

type BooleanExp =
    | True
    | False
    | Equals of ArithExp * ArithExp
    | LTEquals of ArithExp * ArithExp
    | Not of BooleanExp
    | And of BooleanExp * BooleanExp

type Statement =
    | Assignment of Identifier * ArithExp
    | Skip
    | StatementConcat of Statement * Statement
    | If of BooleanExp * Statement * Statement
    | While of BooleanExp * Statement
