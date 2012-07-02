type real_struct = { mantissa: int, exponent: int }
(* { mantissa=n, exponent=e } stands for n * 10^e *)

datatype expr =
  BoolLiteral of bool
| IntLiteral of int
| RealLiteral of real_struct
| ApplyFunc of string * expr list
| VarExpr of var
and var =
  Var of string
| ArrayVar of string * range list
and range =
  FullRange
| PointRange of expr
| Range of expr * expr

datatype primitive_model_type = Real | Int | Bool

datatype model_type =
  PrimitiveType of primitive_model_type
| ArrayType of primitive_model_type * expr list

type decl = { name: string, typ: model_type }

datatype distribution =
  Distr of string * expr list
| TruncDistr of distribution * expr option * expr option

datatype relation = 
  StochRelation of var * distribution
| DetermRelation of var * expr
| DetermLinkRelation of string * var * expr
| ForLoop of string * expr * expr * relation list

type model = { inp: decl list, var: decl list, rel: relation list } 

