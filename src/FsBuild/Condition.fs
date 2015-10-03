[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FsBuild.Condition

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open System.Text

// MSBuild Conditions
// https://msdn.microsoft.com/en-us/library/7szfhaft.aspx

let squote s = String.Concat["'";s;"'"]
let (|SCall|_|) = (|SpecificCall|_|)

let condstr expr = 
    let sb = StringBuilder()
    let inline append (s:^a) = sb.Append s |> ignore
    let inline strappend x = (string>>append) x
    
    let rec inline loopls ls  = List.iter loop ls

    /// If necessary surround the expression with parentheses
    and inline paren ex   = 
        match ex with
        | Value(vl,_) -> strappend vl
        | SCall<@not@>(_,_,[Value(vl,_)]) -> append "!";strappend vl
        | SCall<@not@>(_,_,SCall<@(=)@>(_,_,_)::[]) -> loop ex
        | _ -> append "("; loop ex; append ")"
        
    and inline parenls ls = append "("; loopls ls; append ")"
    /// paren + append + loop list
    and inline pals hd str tl = paren hd; append str; loopls tl
    and loop expr =
        match expr with
        | Value( value,_) -> strappend value
        | SCall<@not@>(_,_,[x]) -> append "!"; paren x
        | SCall<@not@>(_,_,els) -> append "!"; parenls els 
        | SCall<@(=)@>(_,_,le::[re]) -> paren le; append" == "; paren re
        | SCall<@(=)@>(_,_,hd::tl) -> pals hd " == " tl
        | SCall<@(>)@>(_,_,hd::tl) ->  pals hd " &gt; " tl
        | SCall<@(<)@>(_,_,hd::tl) ->  pals hd " &lt; " tl
        | SCall<@(>=)@>(_,_,hd::tl) -> pals hd " &gt;= "  tl
        | SCall <@(<=)@>(_,_,hd::tl) -> pals hd " &lt;= " tl
        | _ -> (string >> append) expr
    loop expr
    string sb

   
type Condition = private { Expr:bool Expr }
type hex = Hex 
let  hex = Hex
    
type EqualOp = EqualOp with
    static member inline (?<-)(_, a:decimal, b:decimal)         = {Expr = <@ a = b @>}
    static member inline (?<-)(_, a:string, b:string)           = {Expr = <@ a = b @>}
    static member inline (?<-)(_, a:int, b:int)                 = {Expr = <@ a = b @>}
    static member inline (?<-)(_, (a,_):int*hex, (b,_):int*hex) = {Expr = <@(sprintf "%x" a) = (sprintf "%x" b)@>}
    static member inline (?<-)(_, a:int, (b,_):int*hex)         = {Expr = <@(sprintf "%d" a) = (sprintf "%x" b)@>}
    static member inline (?<-)(_, (a,_):int*hex, b:int)         = {Expr = <@(sprintf "%x" a) = (sprintf "%d" b)@>}

type NotEqualOp = NotEqualOp with
    static member inline (?<-)(_, a:decimal, b:decimal)         = {Expr = <@ a <> b @>}
    static member inline (?<-)(_, a:string, b:string )          = {Expr = <@ a <> b @>}
    static member inline (?<-)(_, (a,_):int*hex, (b,_):int*hex) = {Expr = <@(sprintf "%x" a) <> (sprintf "%x" b)@>}
    static member inline (?<-)(_, a:int, (b,_):int*hex)         = {Expr = <@(sprintf "%d" a) <> (sprintf "%x" b)@>}
    static member inline (?<-)(_, (a,_):int*hex, b:int)         = {Expr = <@(sprintf "%x" a) <> (sprintf "%d" b)@>}

type LessOp = LessOp with
    static member inline (?<-)(_,a:decimal,b:decimal)           = {Expr = <@ a < b @>}
    static member inline (?<-)(_,a:int, b:int)                  = {Expr = <@ a < b @>}
    static member inline (?<-)(_, (a,_):int*hex, (b,_):int*hex) = {Expr = <@(sprintf "%x" a) < (sprintf "%x" b)@>}
    static member inline (?<-)(_, a:int, (b,_):int*hex)         = {Expr = <@(sprintf "%d" a) < (sprintf "%x" b)@>}
    static member inline (?<-)(_, (a,_):int*hex, b:int)         = {Expr = <@(sprintf "%x" a) < (sprintf "%d" b)@>}

type GreaterOp = GreaterOp with
    static member inline (?<-)(_,a:decimal,b:decimal)           = {Expr = <@ a > b @>}
    static member inline (?<-)(_,a:int    ,b:int    )           = {Expr = <@ a > b @>}
    static member inline (?<-)(_, (a,_):int*hex, (b,_):int*hex) = {Expr = <@(sprintf "%x" a) > (sprintf "%x" b)@>}
    static member inline (?<-)(_, a:int, (b,_):int*hex)         = {Expr = <@(sprintf "%d" a) > (sprintf "%x" b)@>}
    static member inline (?<-)(_, (a,_):int*hex, b:int)         = {Expr = <@(sprintf "%x" a) > (sprintf "%d" b)@>}

type LessEqualsOp = LessEqualsOp with
    static member inline (?<-)(_,a:decimal,b:decimal)           = {Expr = <@ a <= b @>}
    static member inline (?<-)(_,a:int, b:int)                  = {Expr = <@ a <= b @>}
    static member inline (?<-)(_, (a,_):int*hex, (b,_):int*hex) = {Expr = <@(sprintf "%x" a) <= (sprintf "%x" b)@>}
    static member inline (?<-)(_, a:int, (b,_):int*hex)         = {Expr = <@(sprintf "%d" a) <= (sprintf "%x" b)@>}
    static member inline (?<-)(_, (a,_):int*hex, b:int)         = {Expr = <@(sprintf "%x" a) <= (sprintf "%d" b)@>}
    
type GreaterEqualsOp = GreaterEqualsOp with
    static member inline (?<-)(_,a:decimal,b:decimal)           = {Expr = <@ a >= b @>}
    static member inline (?<-)(_,a:int, b:int)                  = {Expr = <@ a >= b @>}
    static member inline (?<-)(_, (a,_):int*hex, (b,_):int*hex) = {Expr = <@(sprintf "%x" a) >= (sprintf "%x" b)@>}
    static member inline (?<-)(_, a:int, (b,_):int*hex)         = {Expr = <@(sprintf "%d" a) >= (sprintf "%x" b)@>}
    static member inline (?<-)(_, (a,_):int*hex, b:int)         = {Expr = <@(sprintf "%x" a) >= (sprintf "%d" b)@>}

let inline (|=|)  a b = (?<-) EqualOp a b
let inline (|<>|) a b = (?<-) NotEqualOp a b
let inline (|>|)  a b = (?<-) LessOp a b
let inline (|<|)  a b = (?<-) GreaterOp a b
let inline (|>=|) a b = (?<-) LessEqualsOp a b
let inline (|<=|) a b = (?<-) GreaterEqualsOp a b

let (|&|) (c1:Cond) (c2:Cond) = {Expr = <@ %c1.Expr && %c2.Expr @>}
let (|!|) (c1:Cond) (c2:Cond) = {Expr = <@ %c1.Expr || %c2.Expr @>}
let  Not  (cd:Cond)           = {Expr = <@ not %cd.Expr @>}

