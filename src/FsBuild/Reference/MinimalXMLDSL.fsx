﻿// [omit:(assembly references and open statements)]
#r "System.Xml.Linq"
open System
open System.Xml.Linq
// [/omit]
// [snippet:DSL for XML literals]
let (!) s = XName.Get(s)
let (@=) xn value = XAttribute(xn, value)
let (@?=) xn value = match value with Some s -> XAttribute(xn, s) | None -> null
type XName with 
    member xn.Item 
        with get([<ParamArray>] objs: obj[]) = 
            if objs = null then null else XElement(xn, objs)
// [/snippet]
// [snippet:DSL for XML matching]
type XElement with 
    member e.Item 
        with get(xn: XName) = 
            match e.Attribute(xn) with null -> "" | a -> a.Value

type XMatch = { 
    Filter: XElement seq -> XElement seq 
    Pick: XElement seq -> string
    Step: XName -> XElement -> XElement seq 
} with
    static member (/) (this: XMatch, xn: XName) = 
        { this with Filter = this.Filter >> Seq.collect (this.Step xn) }

    static member (/) (this: XMatch, xa: XAttribute) = 
        { this with Filter = this.Filter >> Seq.filter (fun e -> e.[xa.Name] = xa.Value) }

    static member (/@) (this: XMatch, xn: XName) =
        { this with 
            Pick = Seq.choose(fun e -> 
                match e.[xn] with "" -> None | s -> Some(s)) >> String.Concat }

    static member (/?) (this: XMatch, e: XElement) = 
        this.Filter(Seq.singleton(e))

    static member (/=) (this: XMatch, e: XElement) = 
        not(this.Filter(Seq.singleton(e)) |> Seq.isEmpty)

    static member (/!) (this: XMatch, e: XElement) = 
        this.Filter(Seq.singleton(e)) |> this.Pick


let (!/) (xn: XName) = {
        Filter = Seq.filter(fun e -> e.Name = xn)
        Pick = Seq.map(fun e -> e.Value) >> String.Concat
        Step = (fun xn e -> e.Elements(xn)) }
//[/snippet]
//[snippet:Example Usage]
let foo, bar, baz, quux = !"foo", !"bar", !"baz", !"{urn:example}quux"
let x = 
    foo.[
        quux.[null],               // intentionally omitted from result
        bar.[baz@?=None,           // omitted attribute
            quux.[()],             // empty element
            quux.[                 // note namespace
                baz@=42,           // value conversion
                "content"]],       // text content
        bar.[baz@=true, 
            quux.[DateTime.Now]]]  // value conversion
printfn "%O" x

printfn "Elements: %A" (!/foo/bar/(baz@=true)/quux/? x)
printfn "Matches: %A" (!/foo/bar/(baz@=true)/quux/= x)
printfn "Doesn't Match: %A" (!/foo/bar/(baz@=false)/quux/= x)
printfn "Values: %A" (!/foo/bar/quux/(baz@=42)/! x)
printfn "Attributes: %A" (!/foo/bar/quux/@baz/! x)
printfn "Split Elements: %A" 
    (Seq.singleton(x) |> (!/foo/bar).Filter |> (!/bar/quux/(baz@=42)).Filter)

// [/snippet]
// [snippet:Optional way to organize schema-specific XNames]
module private ns =
    type Namespace = { ns: XNamespace; decl: XAttribute }
    let inline private n prefix uri = {
        ns = XNamespace.Get(uri)
        decl = XAttribute(XNamespace.Xmlns + prefix, uri) }
    let none       = {ns = XNamespace.None; decl = null}
    let client     = n "client"   "jabber:client"
module private xn =
    let message    = ns.client.ns  + "message"
    let body       = ns.client.ns  + "body"
module private xa = 
    let from       = ns.none.ns    + "from"

printfn "%O" xn.message.[xa.from@="blake@bcdev.com", xn.body.["Hello, World!"]]
// [/snippet]
