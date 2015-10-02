module FsBuild.Cexprs

open System
open FsBuild.Structure



type ImportGroupBuilder() =

    // produce an empty importgroup if nothing is set in the cexpr
    member __.Yield (_) = ImportGroup.empty

    [<CustomOperation("Condition")>]
    /// Set the Condition for the ImportGroup being generated
    member __.Condition (x:ImportGroup, cnd) =
     { x with Condition = Some cnd }

    [<CustomOperation("Import")>]
    member __.Import (x:ImportGroup, imp)  = x.Cons imp

    [<CustomOperation("ImportProject")>]
    member __.ImportProject (x:ImportGroup, proj:string)  =
        x.Cons { Project = proj; Condition = None}

    [<CustomOperation("ImportSeq")>]
    member __.ImportSeq (x:ImportGroup, col: #seq<Import> )  =
        x.Append (Seq.toList col)




