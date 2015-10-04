[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FsBuild.Condition

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open System.Text

// MSBuild Conditions
// https://msdn.microsoft.com/en-us/library/7szfhaft.aspx

//    static member inline (?<-)(_, (a,_):int*hex, (b,_):int*hex) = let x,y = sprintf "%x" a, sprintf "%x" b in {Expr = <@ x = y @>}


   
type Condition = { Expr: bool Expr }
type hex = Hex 
let  hex = Hex

/// Ensure strings are in proper formatting for msbuild xml
let private fixstr s =
    if String.IsNullOrWhiteSpace s then "\' \'"
    elif String.bookends '\'' '\'' s then s
    else sprintf "\'%s\'" s
    
type EqualOp = EqualOp with 
    static member inline (?<-)(_, a:string, b:string)   = let x = fixstr  a
                                                          let y = fixstr  b in {Expr = <@ x = y @>}
    static member inline (?<-)(_, a:decimal, b:decimal) = {Expr = <@ a = b @>}
    static member inline (?<-)(_, a:int, b:int)         = {Expr = <@ a = b @>}
    static member inline (?<-)(_, a:int, b:decimal)     = let d = decimal a in {Expr = <@ d = b @>}
    static member inline (?<-)(_, a:decimal, b:int)     = let d = decimal b in {Expr = <@ a = d @>}

type NotEqualOp = NotEqualOp with
    static member inline (?<-)(_, a:string, b:string )  = let x = fixstr  a
                                                          let y = fixstr  b in {Expr = <@ x = y @>}
    static member inline (?<-)(_, a:decimal, b:decimal) = {Expr = <@ a <> b @>}
    static member inline (?<-)(_, a:int, b:int)         = {Expr = <@ a <> b @>}
    static member inline (?<-)(_, a:int, b:decimal)     = let d = decimal a in {Expr = <@ d <> b @>}
    static member inline (?<-)(_, a:decimal, b:int)     = let d = decimal b in {Expr = <@ a <> d @>}

type LessOp = LessOp with
    static member inline (?<-)(_, a:decimal,b:decimal)  = {Expr = <@ a < b @>}
    static member inline (?<-)(_, a:int, b:int)         = {Expr = <@ a < b @>}
    static member inline (?<-)(_, a:int, b:decimal)     = let d = decimal a in {Expr = <@ d < b @>}
    static member inline (?<-)(_, a:decimal, b:int)     = let d = decimal b in {Expr = <@ a < d @>}

type GreaterOp = GreaterOp with
    static member inline (?<-)(_, a:decimal,b:decimal)  = {Expr = <@ a > b @>}
    static member inline (?<-)(_, a:int    ,b:int    )  = {Expr = <@ a > b @>}
    static member inline (?<-)(_, a:int, b:decimal)     = let d = decimal a in {Expr = <@ d > b @>}
    static member inline (?<-)(_, a:decimal, b:int)     = let d = decimal b in {Expr = <@ a > d @>}

type LessEqualsOp = LessEqualsOp with
    static member inline (?<-)(_, a:decimal,b:decimal)  = {Expr = <@ a <= b @>}
    static member inline (?<-)(_, a:int, b:int)         = {Expr = <@ a <= b @>}
    static member inline (?<-)(_, a:int, b:decimal)     = let d = decimal a in {Expr = <@ d <= b @>}
    static member inline (?<-)(_, a:decimal, b:int)     = let d = decimal b in {Expr = <@ a <= d @>}

type GreaterEqualsOp = GreaterEqualsOp with
    static member inline (?<-)(_, a:decimal,b:decimal)  = {Expr = <@ a >= b @>}
    static member inline (?<-)(_, a:int, b:int)         = {Expr = <@ a >= b @>}
    static member inline (?<-)(_, a:int, b:decimal)     = let d = decimal a in {Expr = <@ d >= b @>}
    static member inline (?<-)(_, a:decimal, b:int)     = let d = decimal b in {Expr = <@ a >= d @>}

let inline (|=|)  a b = (?<-) EqualOp a b
let inline (|<>|) a b = (?<-) NotEqualOp a b
let inline (|>|)  a b = (?<-) LessOp a b
let inline (|<|)  a b = (?<-) GreaterOp a b
let inline (|<=|) a b = (?<-) LessEqualsOp a b
let inline (|>=|) a b = (?<-) GreaterEqualsOp a b

let (.&&.) (c1:Condition) (c2:Condition) = {Expr = <@ %c1.Expr && %c2.Expr @>}
let (.||.) (c1:Condition) (c2:Condition) = {Expr = <@ %c1.Expr || %c2.Expr @>}
let  NOT   (cd:Condition)                = {Expr = <@ not %cd.Expr @>}

let squote s = String.Concat["'";s;"'"]
let (|SCall|_|) = (|SpecificCall|_|)

let condstr (cond:Condition) = 
    let sb = StringBuilder()
    let inline append (s:^a) = sb.Append s |> ignore
    let inline appendq s = sprintf "\'%s\'" s |> append
    //let inline strappend x = (string>>append) x
    let inline strappend x = (string>>append) x
    
    let rec inline loopls ls  = List.iter loop ls

    /// If necessary surround the expression with parentheses
    and inline paren ex   = 
        match ex with
        | Value(vl,s) when s = typeof<String> -> appendq (vl :?> string)
        | Value(vl,_) -> strappend vl
        | SCall<@not@>(_,_,[Value(vl,_)]) -> append "!";strappend vl
        | SCall<@not@>(_,_,SCall<@(=)@>(_,_,_)::[]) -> loop ex
        | _ -> append "("; loop ex; append ")"
        
    and inline parenls ls = append "("; loopls ls; append ")"
    /// paren + append + loop list
    and inline pals lex str rex = paren lex; append str; loopls rex
    /// paren + append + paren
    and inline pap lex str rex = paren lex; append str; paren rex
    and loop expr =
        match expr with
        | Value(value,_) -> strappend value
        | SCall<@not@> (_,_,[x]) -> append "!"; paren x
        | SCall<@not@> (_,_,els) -> append "!"; parenls els 
        | SCall<@(=)@> (_,_,lex::[rex]) -> pap  lex " == "    rex
        | SCall<@(=)@> (_,_,lex::rex)   -> pals lex " == "    rex
        | SCall<@(>)@> (_,_,lex::rex)   -> pals lex " &gt; "  rex
        | SCall<@(>)@> (_,_,lex::[rex]) -> pap  lex " == "    rex
        | SCall<@(<)@> (_,_,lex::rex)   -> pals lex " &lt; "  rex
        | SCall<@(<)@> (_,_,lex::[rex]) -> pap  lex " == "    rex
        | SCall<@(>=)@>(_,_,lex::rex)   -> pals lex " &gt;= " rex
        | SCall<@(>=)@>(_,_,lex::[rex]) -> pap  lex " == "    rex
        | SCall<@(<=)@>(_,_,lex::rex)   -> pals lex " &lt;= " rex
        | SCall<@(<=)@>(_,_,lex::[rex]) -> pap  lex " == "    rex
        | SCall<@(<>)@>(_,_,lex::rex)   -> pals lex " != "    rex
        | SCall<@(<>)@>(_,_,lex::[rex]) -> pap  lex " != "    rex        
        | IfThenElse   (lex,_, rex)     -> pap  lex " || "    rex  // Call expr,Value true,Call expr is the pattern for OR
        | IfThenElse   (lex,rex,_)      -> pap  lex " && "    rex  // Call expr,Call expr,Value false is the pattern for AND 
        | SCall<@(&&)@>(_,_,lex::rex)   -> pals lex " && "    rex
        | SCall<@(&&)@>(_,_,lex::[rex]) -> pap  lex " && "    rex  
        | SCall<@(||)@>(_,_,lex::rex)   -> pals lex " || "    rex
        | SCall<@(||)@>(_,_,lex::[rex]) -> pap  lex " || "    rex  
        | _ -> (string>>append) expr
    loop cond.Expr
    string sb