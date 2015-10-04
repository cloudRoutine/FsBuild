#r "System.Xml.Linq"
#load "Prelude.fs" "Condition.fs" "Extensions.fs" "Structure.fs" "Cexprs.fs" "Metadata.fs"

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open System.Text
open FsBuild.Condition
open FsBuild.Structure
open FsBuild.Cexprs



let squote s = String.Concat["'";s;"'"]
let (|SCall|_|) = (|SpecificCall|_|)

let qstr expr = 
    let sb = StringBuilder()
    let inline append (s:^a) = sb.Append s |> ignore
    //let inline strappend x = (string>>append) x
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
    and inline pals lex str rex = paren lex; append str; loopls rex
    /// paren + append + paren
    and inline pap lex str rex = paren lex; append str; paren rex
    and loop expr =
        match expr with
        | Value(value,t) -> strappend value
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
        | IfThenElse(l,Value p, r) -> paren l; append " || ";paren r
        | IfThenElse(l,r,Value p)  -> paren l; append " && ";paren r
        | SCall<@(&&)@>(_,_,lex::rex)   -> pals lex " && "    rex
        | SCall<@(&&)@>(_,_,lex::[rex]) -> pap  lex " && "    rex  
        | SCall<@(||)@>(_,_,lex::rex)   -> pals lex " || "    rex
        | SCall<@(||)@>(_,_,lex::[rex]) -> pap  lex " || "    rex  
        | _ -> (string>>append) expr
    loop expr
    string sb

let condstr (cond:Condition) = qstr cond.Expr
let pfn s = printfn "%A" s
let inline cpn x = (condstr>>printfn "%A") x
let inline qpn x = (qstr>>printfn "%A") x
;;

let condq = ("$(Configuration)"|=|"Debug")|!|(10.5m|>=|45.05m)|&|("Proj2"|<>|"")|!|(5|<|10)|&|(0x11|<=|0x5)|&|(NOT(5|>|3.0m));;
condq |> cpn



//;;
//cpn ("$(Configuration)"|=|"Debug") ;;
//cpn (10.5m|>=|45.05m) ;;
//cpn (("Proj2"|<>|"")|&|(10.5m|>=|45.05m));;
//pfn "AND"
//pfn (<@ (0=1) && (2=5)@>);;
//qpn (<@ (0=1) && (2=5)@>);;
//pfn "OR"
//pfn (<@ (0=1) || (6=4) @>);;
//qpn (<@ (4=5) || (6=4)@>);;
//
//(|IfThenElse|_|) <@ (4=5) || (6=4)@>;;
////qpn (<@ ("proj2"<>"") && (10.5m>=45.05m) @>);;
