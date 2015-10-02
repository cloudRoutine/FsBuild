#r "System.Xml.Linq"
#load "Prelude.fs" "Extensions.fs" "Structure.fs" "Cexprs.fs" "Metadata.fs"
open FsBuild.Structure
open FsBuild.Cexprs


let importGroup = ImportGroupBuilder()      


let imp x = import x None    

let igg = 
    importGroup {  
        Condition "$(VsVerion) >= 10.0"
        Import (imp "Proj1")
        ImportProject "System.Error"
        Import (imp "Proj3")
        ImportSeq [for x in 0..20 -> "Project" + string x |> imp  ] 
        Import (imp "Proj5")
    } 

igg |> printfn "%A";;
igg |> printfn "%O";;
igg |> string |> printfn "%s"
