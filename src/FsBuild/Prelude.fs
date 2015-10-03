namespace FsBuild
open System

open System.IO

[<AutoOpen>]
module Prelude =


    /// Unary operator to add parentheses around a string  
    /// to create msbuild metadata properties              
    /// e.g. `%(FullPath)` , `%(Directory)`                
    //let inline ( ~% ) str =  sprintf "%%(%s)" str
    // ^ this is fucked if you use quotations

    /// Property names are limited to ASCII chars only. Property values 
    /// are referenced in the project by placing the property name between 
    /// "$(" and ")". For example, $(builddir)\classes would resolve to
    ///  "build\classes", if the builddir property had the value build. 
    let inline ( !& ) str =  sprintf "$(%s)" str


    /// Combines 2 strings into a path
    let (+/) path1 path2 = Path.Combine (path1, path2)  


    let inline getChildren x = (^a : (member Children : 'c list ) x)
    let inline getValue x    = (^a : (member Value : 'v ) x)

    let inline isNone x = Option.isNone x
    let inline isSome x = Option.isSome x


