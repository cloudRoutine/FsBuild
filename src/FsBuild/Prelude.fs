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
    let inline getValue x = (^a : (member Value : 'v ) x)



    let inline getOption key this =
        let mutable v = Unchecked.defaultof<'v>
        let scc = ( ^a : (member TryGetValue : 'k * ('v byref) -> bool) this, key, &v)
        if scc then Some v else None

    let inline getOrDefault key defaultValue this =
        let mutable v = Unchecked.defaultof<'v>
        let scc = ( ^a : (member TryGetValue : 'k * ('v byref) -> bool) this, key, &v)
        if scc then v else defaultValue

    let inline bind key f this =
        let mutable v = Unchecked.defaultof<'v>
        let scc = ( ^a : (member TryGetValue : 'k * ('v byref) -> bool) this, key, &v)
        if scc then f v else None

    let inline return'< ^a,'k,'v when 'a : (member TryGetValue : 'k * ('v byref) -> bool)>(v : 'v) ( _ : ^a)  = Some v

    open System.Runtime.CompilerServices
    [<Extension>]
    type ExtensionMethods() =
        [<Extension>] static member inline GetOption    (this, key)      = getOption  key this
        [<Extension>] static member inline GetOrDefault (this, key,dval) = getOrDefault key dval this 
        [<Extension>] static member inline Bind         (this, key, f)   = bind key f this 
        [<Extension>] static member inline Return       (this,v)         = return' v this













    let inline isNone x = Option.isNone x
    let inline isSome x = Option.isSome x


    open System.Text
    type System.Text.StringBuilder with
        member private self.Yield (_) = self 
        [<CustomOperation("append")>]
        member __.append (sb:StringBuilder, str:string) = sb.Append str
        [<CustomOperation("appendLine")>]
        member __.appendLine (sb:StringBuilder, str:string) = sb.AppendLine str
        member private self.Run sb = string sb

    let sb = StringBuilder()

    let s = StringBuilder(){ 
                append "\nsome line"
                appendLine "\n\n#2"
                appendLine "great success"
            }






    let inline GetOption< ^a,'k,'v when 'a : (member TryGetValue : 'k * ('v byref) -> bool)>(this : ^a, key : 'k) =
        let mutable v = Unchecked.defaultof<'v>
        let scc = ( ^a : (member TryGetValue : 'k * ('v byref) -> bool) this, key, &v)
        if scc then
            Some v
        else
            None

    open System.Collections.Generic


    let inline hasName  (x : ^a when ^a : (member Name : ^c )) = x
    let inline hasBind  (x : ^a when ^a : (member Bind : ^a * ^b -> ^c)) = x
    let inline hasDelay (x : ^a when ^a : (member Delay: ^a * ^b -> ^c)) = x
    let inline hasYield (x : ^a when ^a : (member Name : string )) = x
    let inline hasReturn(x : ^a when ^a : (member Name : string )) = x
    let inline hasZero  (x : ^a when ^a : (member Name : string )) = x



    type TagΑ = class end
    type TagΓ = class end
    type TagΔ = class end
    type TagΕ = class end

    type ifaceΑ = interface end
    type ifaceΓ = interface end
    type ifaceΔ = interface end
    type ifaceΕ = interface end

    type stΑ = struct end
    type stΓ = struct end
    type stΔ = struct end
    type stΕ = struct end

    type Tagβ = class end
    type ifaceβ = interface end
    type stβ = struct end
    type [<Measure>] β


    type [<Measure>] Α = class end
    type [<Measure>] Γ = class end
    type [<Measure>] Δ = class end
    type [<Measure>] Ε = class end


//    type 'Phantom String(str:string ) =  
//       
//        static member inline (?)(a,b) : ^c = 
//            str?b 
