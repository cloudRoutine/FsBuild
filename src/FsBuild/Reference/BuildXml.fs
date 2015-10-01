module FsBuild.BuildXml


open System.Xml.Linq

(*
Under the hood, attrs and elems are just .Net XElements and XAttributes,
in the MSBuild XML namespace.
*)
let nsName = "http://schemas.microsoft.com/developer/msbuild/2003"
let ns = XNamespace.op_Implicit(nsName)

let elem
  (name: string)
  (attrs: list<XAttribute>)
  (children: list<XNode>): XElement =
  let _elem = new XElement(ns + name)

  attrs |> List.iter (fun attr -> _elem.Add(attr))
  children |> List.iter (fun child -> _elem.Add(child))
  _elem

let attr (name: string) (value: string): XAttribute =
  new XAttribute(XName.op_Implicit(name), value)

(*
All supported attributes.
*)
module A =
  let Condition        = attr "Condition"
  let DefaultTargets   = attr "DefaultTargets"
  let DependsOnTargets = attr "DependsOnTargets"
  let Directories      = attr "Directories"
  let Files            = attr "Files"
  let Include          = attr "Include"
  let Inputs           = attr "Inputs"
  let Name             = attr "Name"
  let OutputAssembly   = attr "OutputAssembly"
  let Outputs          = attr "Outputs"
  let Platform         = attr "Platform"
  let Project          = attr "Project"
  let References       = attr "References"
  let Returns          = attr "Returns"
  let Sources          = attr "Sources"
  let TargetType       = attr "TargetType"
  let ToolsVersion     = attr "ToolsVersion"

(*
All supported elements.
*)
let Choose                  = elem "Choose"
let Csc                     = elem "Csc"
let Delete                  = elem "Delete"
let Fsc                     = elem "Fsc"
let Import                  = elem "Import"
let Item                    = elem
let ItemGroup               = elem "ItemGroup"
let MakeDir                 = elem "MakeDir"
let Otherwise               = elem "Otherwise"
let Property                = elem
let PropertyGroup           = elem "PropertyGroup"
let RemoveDir               = elem "RemoveDir"
let Target                  = elem "Target"
let Text (s: string): XText = new XText(s)
let When                    = elem "When"

let ProjectObj attrs children =
  let fsTargets30 =
    @"$(MSBuildExtensionsPath32)\..\Microsoft SDKs\F#\3.0\Framework\v4.0\Microsoft.FSharp.targets"
  let fsTargets31 =
    @"$(MSBuildExtensionsPath32)\..\Microsoft SDKs\F#\3.1\Framework\v4.0\Microsoft.FSharp.targets"

  let _attrs = attr "xmlns" nsName :: attrs
  let _children =
    Choose
      []
      [ When
          [ A.Condition <| sprintf "Exists('%s')" fsTargets31 ]
          [ PropertyGroup
              []
              [ Property "FSTargetsPath" [] [ Text fsTargets31 ] ] ]
        When
          [ A.Condition <| sprintf "Exists('%s')" fsTargets30 ]
          [ PropertyGroup
              []
              [ Property "FSTargetsPath" [] [ Text fsTargets30 ] ] ] ] ::
    Import [ A.Project "$(FSTargetsPath)" ] [] ::
    children
  new XElement(ns + "Project", _attrs, _children)

let Project attrs children =
  (ProjectObj attrs children).Save("Build.xml")

