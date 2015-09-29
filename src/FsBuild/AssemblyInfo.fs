namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsBuild")>]
[<assembly: AssemblyProductAttribute("FsBuild")>]
[<assembly: AssemblyDescriptionAttribute("EDSL for .fsproj files and MSBuild Tasks")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
