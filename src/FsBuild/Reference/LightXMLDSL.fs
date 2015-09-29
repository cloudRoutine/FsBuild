﻿namespace Templater 

open System
open System.Xml

// The DSL
module LightDsl =
(*[omit:(Structural types omitted)]*)
    type XmlMarkup =
        | Element of XmlElement
        | Attribute of XmlAttribute

    and XmlName =
        | Name of string
        | QualifiedName of string * string

    and XmlElement =
        {   Name:XmlName
            Attributes:XmlAttribute list
            Content:XmlElementContent }

    and XmlElementContent =
        | Empty
        | Value of string
        | Content of XmlElement list
            
    and XmlAttribute =
        {   Name:XmlName
            Value:String    }
(*[/omit]*)
    let name s = Name (s)
    let qname ns s = QualifiedName (ns, s)

    let (@=) name value = { Name=name; Value=value }
    let elem name = { Name=name; Attributes=[]; Content=Empty }
    let attribs a (el:XmlElement) = { el with Attributes=a }
    let value s (el:XmlElement) = { el with Content=Value (s) }
    let content items (el:XmlElement) = { el with Content=Content (items) }


open System.Xml.Linq
open LightDsl

[<AutoOpen>]
module XElementExtension =
(*[omit:(Private members omitted)]*)
    let private mapName = function
        | Name n -> XName.Get (n)
        | QualifiedName (ns,n) -> XName.Get (n, ns)

    let private mapAttribs (attribs:XmlAttribute list) =
        attribs |> List.map (fun a -> new XAttribute (mapName a.Name, a.Value))
                
    let rec private map (e:XmlElement) =
        match e.Content with
        | Empty -> new XElement (mapName e.Name)
        | Value s -> 
            let content =
                mapAttribs e.Attributes
                |> List.map (fun a -> a :> obj)
                |> List.append ([s :> obj])

            new XElement (mapName e.Name, content)
        | Content c -> 
            let content =
                mapAttribs e.Attributes
                |> List.map (fun a -> a :> obj)
                |> List.append (c |> List.map (fun e -> map (e) :> obj))

            new XElement (mapName e.Name, content)
(*[/omit]*)
    module XElement =
        let ofLightDsl (xe:XmlElement) = map xe

// Usage
    let xml = 
        elem (qname "http://myschema" "root")
        |> content [
            elem (name "Person")
            |> attribs [name "id" @= "js1"]
            |> content [elem (name "FullName") |> value "John Smith" ]]
        |> XElement.ofLightDsl
