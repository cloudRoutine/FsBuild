module FsBuild.Cexprs

open System
open FsBuild.Structure

type CustOp = CustomOperationAttribute

type ImportBuilder() =
    member __.Yield (_) =  Import.empty
    member __.Zero  () =   Import.empty
    /// Set the Condition for the ImportGroup being generated
    [<CustOp "Condition">]
    member __.Condition (x:Import, cnd) =
        { x with Condition = Some cnd }

let import = ImportBuilder()
// ^ turns into the keyword to use the cexpr

let z = import{()}


type ImportGroupBuilder() =
    // produce an empty importgroup if nothing is set in the cexpr
    member __.Yield (_) = ImportGroup.empty
  
    /// Set the Condition for the ImportGroup being generated
    [<CustOp "Condition">]   
    member __.Condition (x:ImportGroup, cnd) =
        { x with Condition = Some cnd }

    [<CustOp "Import">]
    member __.Import (x:ImportGroup, imp)  = x.Cons imp

    [<CustOp "ImportProject">]
    member __.ImportProject (x:ImportGroup, proj:string)  =
        x.Cons { Project = proj; Condition = None}

    [<CustOp "ImportSeq">]
    member __.ImportSeq (x:ImportGroup, col: #seq<Import> )  =
        x.Append (Seq.toList col)










