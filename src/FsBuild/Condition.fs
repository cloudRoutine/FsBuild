namespace FsBuild

open System
open System.Xml
open System.Xml.Linq
open Microsoft.Build
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Text
// MSBUILD project file schema reference
// https://msdn.microsoft.com/en-us/library/5dy88c2e.aspx



            

// VSIX Extension Schema 2.0 Reference 
// https://msdn.microsoft.com/en-us/library/hh696828.aspx
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Condition =

// MSBuild Conditions
// https://msdn.microsoft.com/en-us/library/7szfhaft.aspx

    let squote (str:string) =
        String.Concat["'";str;"'"]

    let condstr expr = 
        let sb = StringBuilder()
        let inline append (s:^a) = sb.Append s |> ignore
        let inline appendstr x   = string x |> append

        let rec loop expr =
            let inline loopls ls  = List.iter loop ls

            /// If necessary surround the expression with parentheses
            let inline paren ex   = 
                match ex with
                | Value(vl,_)               ->  appendstr vl
                | SpecificCall <@ not @> 
                    (_,_,[ Value(vl,_)] )   ->  append "!";appendstr vl
                | SpecificCall <@ not @> 
                    (_,_, SpecificCall<@(=)@>
                            (_,_,expls)::[])  
                                            ->  loop ex
                | _                         ->  append "("; loop ex; append ")"

            let inline parenls ls = append "("; loopls ls; append ")"

            match expr with
            | Value( value,_)           ->  appendstr value
            | SpecificCall <@ not @> 
                (_,_, [x])              ->  append "!"; paren x
            | SpecificCall <@ not @> 
                (_,_, exprls)           ->  append "!"; parenls exprls 
            | SpecificCall <@(=)@> 
                (_, _,lexp::[rexp])     ->  paren lexp; append " == "; paren rexp
            | SpecificCall <@(=)@> 
                (_, _, hd::tl)          ->  paren hd; append " == "; loopls tl

            | SpecificCall <@(>)@>
                (_, _, hd::tl)          ->  paren hd; append " &gt; "; loopls tl
            | SpecificCall <@(<)@>
                (_, _, hd::tl)          ->  paren hd; append " &lt; "; loopls tl
            | SpecificCall <@(>=)@>
                (_, _, hd::tl)          ->  paren hd; append " &gt;= "; loopls tl
            | SpecificCall <@(<=)@>
                (_, _, hd::tl)          ->  paren hd; append " &lt;= "; loopls tl
            | _                         ->  string expr |> append 
        loop expr
        string sb





//
//
//    let (.<>.) (c1:string)  (c2:string)  =  Condition(<@ c1 <> c2 @>)
//    let (.>=.) (c1:decimal) (c2:decimal) =  Condition(<@ c1 >= c2 @>)
//    let (.<=.) (c1:decimal) (c2:decimal) =  Condition(<@ c1 <= c2 @>)
//  
//
//    // where condition is a cexpr
//    //condition { (%"ProjectName" .=. "FirstProject") .||. (%"ProjectName" .=. "SecondProject") }
//
//
//
//
//
//    let (==)   (c1:Condition)(c2:Condition) = Condition <@ %c1.Expr = %c2.Expr @>
//    let (.||.) (c1:Condition)(c2:Condition) = Condition <@ %c1.Expr || %c2.Expr @>
//  
//        static member inline (!=) (c1:Condition, c2:Condition) =
//            Condition(<@ %c1.Expr <> %c2.Expr @>)
//
//        static member op_Explicit (cond:Condition) : string =
//            condstr cond.Expr
//
//        override self.ToString() =
//           condstr self.Expr 

//
//    let (.=.)  (c1:string) (c2:string) =  
//        let c1', c2' = squote c1, squote c2
//        Condition(<@ c1' = c2' @>)
//
//
//
//
//    let (.&&.) (c1:Condition)(c2:Condition) =
//            Condition(<@ %c1.Expr && %c2.Expr @>)
//
//
//    let makecondition (combo:'a->'a->Condition) (arg1:'a) (arg2:'a) =
//        combo arg1 arg2
//
//    let z = makecondition (.<>.) "one" "two"
//    let z1 = makecondition (.>=.) 23.0m  233.m
//    let z2 = makecondition (.||.)   z z1
//

//    type ConditionBuilder () =
//        let data = xattr "Condition" []
//        member __.Zero () = data
//
//    let cond expr = Condition expr
//        
//    let x = ((cond <@ 2 = 3 @>) !=  (cond <@ not (10 <> 4) @>) ) |> string




