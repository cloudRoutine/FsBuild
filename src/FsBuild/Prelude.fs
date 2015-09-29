namespace FsBuild
open System.IO

[<AutoOpen>]
module Prelude =

    let (+/) s1 s2 = Path.Combine (s1,s2)  