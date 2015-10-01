open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open System
open System.Text

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
        | SpecificCall <@(+)@> 
            (_, _,lexp::[rexp])     ->  paren lexp; append " !+! "; paren rexp
        | SpecificCall <@(+)@> 
            (_, _, hd::tl)          ->  paren hd; append " !+! "; loopls tl
        | _                         ->  string expr |> append 
    loop expr
    string sb

condstr <@ 2+3 = 5 @>


//condstr <@ not ("FSharp.Core.v$(Version)" = "FSharp.Core.v2.0")  @>

