namespace FsBuild

open System.Xml
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Text
// MSBUILD project file schema reference
// https://msdn.microsoft.com/en-us/library/5dy88c2e.aspx



module Common =

    let getGenericMethodDefinition (mi:MethodInfo) =
        match mi.IsGenericMethod with
        | true -> mi.GetGenericMethodDefinition()
        | false -> mi

    let rec getGenericMethodInfo q = 
        match q with
        | Patterns.Lambda(_, q)
        | Patterns.Let(_, _, q)
        | Patterns.Coerce(q, _)
        | Patterns.LetRecursive(_, q)   ->  getGenericMethodInfo q
        | Patterns.Call(_, mi, _)       ->  getGenericMethodDefinition mi
        | _                             ->  failwithf "Unexpected method %A" q

    let idMi         = getGenericMethodInfo <@@ id @@>
    let plusMi       = getGenericMethodInfo <@@ (+) @@>
    let minusMi      = getGenericMethodInfo <@@ (-) @@>
    let unaryMinusMi = getGenericMethodInfo <@@ fun x -> -x @@>
    let timesMi      = getGenericMethodInfo <@@ (*) @@>
    let divMi        = getGenericMethodInfo <@@ (/) @@>
    let modMi        = getGenericMethodInfo <@@ (%) @@>
    let eqMi         = getGenericMethodInfo <@@ (=) @@>
    let neqMi        = getGenericMethodInfo <@@ (<>) @@>
    let gtMi         = getGenericMethodInfo <@@ (>) @@>
    let geqMi        = getGenericMethodInfo <@@ (>=) @@>
    let ltMi         = getGenericMethodInfo <@@ (<) @@>
    let leqMi        = getGenericMethodInfo <@@ (<=) @@>
//    let strconcatMi  = getGenericMethodInfo <@@ (^) @@>
 //   let likeMi       = getGenericMethodInfo <@@ fun x y -> System.Data.Linq.SqlClient.SqlMethods.Like(x, y) @@>
    let andMi        = getGenericMethodInfo <@@ (&&) @@>
    let orMi         = getGenericMethodInfo <@@ (||) @@>
    let notMi        = getGenericMethodInfo <@@ (not) @@>
    let apprMi       = getGenericMethodInfo <@@ (|>) @@>
    let applMi       = getGenericMethodInfo <@@ (<|) @@>

    let c = (|Call|_|)

// In XML, the characters < and > must be escaped. 
// The symbol < is represented as &lt;. 
// The symbol > is represented as &gt;.
                                                                 

    let println expr =
        let rec print expr =
            match expr with
            | Application(expr1, expr2) ->
                // Function application.
                print expr1
                printf " "
                print expr2
            | SpecificCall <@@ (+) @@> (_, _, exprList) ->
                // Matches a call to (+). Must appear before Call pattern.
                print exprList.Head
                printf " + "
                print exprList.Tail.Head
            | Call(exprOpt, methodInfo, exprList) ->
                // Method or module function call. 
                match exprOpt with
                | Some expr -> print expr
                | None -> printf "%s" methodInfo.DeclaringType.Name
                printf ".%s(" methodInfo.Name
                if (exprList.IsEmpty) then printf ")" else
                print exprList.Head
                for expr in exprList.Tail do
                    printf ","
                    print expr
                printf ")"
            | Int32(n) ->
                printf "%d" n
            | Lambda(param, body) ->
                // Lambda expression.
                printf "fun (%s:%s) -> " param.Name (param.Type.ToString())
                print body
            | Let(var, expr1, expr2) ->
                // Let binding. 
                if (var.IsMutable) then
                    printf "let mutable %s = " var.Name
                else
                    printf "let %s = " var.Name
                print expr1
                printf " in "
                print expr2
            | PropertyGet(_, propOrValInfo, _) ->
                printf "%s" propOrValInfo.Name
            | String(str) ->
                printf "%s" str
            | Value(value, typ) ->
                printf "%s" (value.ToString())
            | Var(var) ->
                printf "%s" var.Name
            | _ -> printf "%s" (expr.ToString())
        print expr
        printfn ""


// VSIX Extension Schema 2.0 Reference 
// https://msdn.microsoft.com/en-us/library/hh696828.aspx
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Condition' =

// MSBuild Conditions
// https://msdn.microsoft.com/en-us/library/7szfhaft.aspx

    let equals (a:string) (b:string) = <@ a = b @>
    let equalsE (a:string) (b:string) = 
        let a' = <@a@>
        let b' = <@b@>
        <@ a' = b' @>

    let ex0 (a) (b) = <@ %a = %b @>
    let ex1 = <@"pete"@>

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
            | SpecificCall <@@ (=) @@> 
                (_, _,lexp::[rexp])     ->  paren lexp; append " == "; paren rexp
            | SpecificCall <@@ (=) @@> 
                (_, _, hd::tl)          ->  paren hd; append " == "; loopls tl
            | _                         ->  string expr |> append 
        loop expr
        string sb

    let ex2 (a) (b) c = <@ %a = %b |> c @>
    let ex3 = <@"they still work after"@>

module ProjectDefn =


    type Attribute =
        | Condition
        | ItemName
        | PropertyName
        | TaskParameter
        | Output
        | ParameterType
        | Required
        | DefaultTargets
        | InitialTarets
        | ToolsVersopm
        | TreatAsLocalProperty


    type MSBuild =
        | Choose
        | Import
        | ImportGroup
        | Item
        | ItemDefinitionGroup
        | ItemGroup
        | ItemMetadata
        | OnError
        | Otherwise
        | Output
        | Parameter
        | ParameterGroup
        | Project
        | ProjectExtensions
        | Property
        | PropertyGroup
        | Target
        | Task
        | TaskBody
        | UsingTask
        | When

