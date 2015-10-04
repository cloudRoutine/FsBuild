module FsBuild.Structure

open System
open System.Text
open FsBuild.Condition


let inline getChildren  x = (^a : (member Children  : 'c list ) x)
let inline getCondition x = (^a : (member Condition : Condition ) x)

/// Imports the contents of one project file into another project file.
// https://msdn.microsoft.com/en-us/library/92x05xfs.aspx
[<StructuredFormatDisplay "{display}">]
type Import  =
 {  /// Required attribute - The path of the project file to import. The path can include wildcards. 
    /// The matching files are imported in sorted order.
    Project     : string
    Condition   : Condition option 
 }  static member empty = { Project = ""; Condition = None }
    member private self.display =
        sprintf "{Import: Project - %s | Condition - %s}"
            self.Project (  if isNone self.Condition 
                            then "None" else getValue self.Condition |> string )
        
    

let import project condition = { Project = project; Condition = condition }


/// Contains a collection of Import elements that are grouped under an optional condition. 
// https://msdn.microsoft.com/en-us/library/ff606262.aspx
[<StructuredFormatDisplay "ImportGroup{\n{display\n}">]
type ImportGroup =
 {  Condition : Condition option
    Imports : Import list   
 } 
    /// Add an Import to the front of the ImportGroup's collection
    member self.Cons ipr = { self with Imports = ipr::self.Imports}
    /// Add an Import to the end of the ImportGroup's collection
    member self.Conj ipr = { self with Imports = List.conj ipr self.Imports  }
    /// Append a list of imorts to the ImportGroup's collection
    member self.Append ls = { self with Imports = self.Imports @ ls}
    /// New ImportGroup with a different Condition
    member self.WithCondition cond = { self with Condition = Some cond }

    /// An empty ImportGroup
    static member empty = { Condition = None; Imports = [] }
    member private self.display =
        (StringBuilder().AppendLine(
            "Condition : " + (  if Option.isSome self.Condition 
                                then self.Condition.Value.ToString() else ""))
        |> List.fold (fun (sb:StringBuilder) (rcd:Import) -> sb.AppendLine(string rcd)))
            self.Imports |> string
    override self.ToString() = sprintf "ImportGroup\n%s" self.display


/// Contains a user-defined item metadata key, which contains the item
/// metadata value. An item may have any number of metadata key-value pairs.
//  Syntax -  <ItemMetadataName> Item Metadata value </ItemMetadataName> e.g. <Culture>fr</Culture>
//  https://msdn.microsoft.com/en-us/library/ms164284.aspx
type ItemMetadata =
 {  /// This text specifies the item metadata value, which can be either text or XML.
    Value : string
    Condition : Condition option  
 }


 
/// Contains a user-defined item and its metadata. Every item that is used in a 
/// MSBuild project must be specified as a child of an ItemGroup element.
// https://msdn.microsoft.com/en-us/library/ms164283.aspx
type Item =
 {  /// Required attribute. - The file or wildcard to include in the list of items.
    Include : string
    /// Optional attribute -The file or wildcard to exclude from the list of items.
    Exclude : string
    Condition : Condition option
    Remove : string
    /// Optional attribute - The metadata for the source items to add to the target
    /// items. Only the metadata whose names are specified in the semicolon-delimited 
    /// list are transferred from a source item to a target item. 
    KeepMetadata : string list
    /// Optional attribute - The metadata for the source items to not transfer to the 
    /// target items. All metadata is transferred from a source item to a target item 
    /// except metadata whose names are contained in the semicolon-delimited list of names. 
    RemoveMetadata : string list
    /// Specifies whether an item should be added to the target group if it's 
    /// an exact duplicate of an existing item. If the source and target item have
    /// the same Include value but different metadata, the item is added even if KeepDuplicates is set to false. 
    /// This attribute is valid only if it's specified for an item in an ItemGroup that's in a Target.
    KeepDuplicates : bool option
    /// There may be zero or more ItemMetadata elements in an item.
    Children : ItemMetadata list
 }


/// Contains a set of user-defined Item elements. Every item used in a MSBuild 
/// project must be specified as a child of an ItemGroup element.
// https://msdn.microsoft.com/en-us/library/646dk05y.aspx
[<StructuredFormatDisplay "ItemGroup\n{display}">]
type ItemGroup =
 {  Condition : Condition option
    /// Defines the inputs for the build process. There may be zero or more Item elements in an ItemGroup.
    Children : Item list
    } member private self.display =
        (StringBuilder().AppendLine(
            "Condition : " + (  if Option.isSome self.Condition 
                                then string self.Condition.Value else ""))
        |> List.fold (fun (sb:StringBuilder) (rcd:Item) -> sb.AppendLine(string rcd)))
            self.Children |> string
    override self.ToString() =
        sprintf "ItemGroup\n%s" self.display
/// The ItemDefinitionGroup element lets you define a set of Item Definitions, 
/// which are metadata values that are applied to all items in the project, by default. 
//  https://msdn.microsoft.com/en-us/library/bb629392.aspx
type ItemDefinitionGroup =
 {  /// There may be zero or more Item elements in an ItemDefinitionGroup  
    Children : Item list
    Condition : Condition option
     
    }

/// Contains a user defined property name and value. Every property used in 
/// an MSBuild project must be specified as a child of a PropertyGroup element.
//  https://msdn.microsoft.com/en-us/library/ms164288.aspx
type Property =
 {  Condition : Condition
    /// A text value is optional -this text specifies the property value and may contain XML.
    Value : string
    (*  Remarks -
        Property names are limited to ASCII chars only. Property values are 
        referenced in the project by placing the property name between
        "$(" and ")". For example, $(builddir)\classes would resolve to 
        "build\classes", if the builddir property had the value build.
    *)
 }

/// Contains a set of user-defined Property elements. Every Property element
///  used in an MSBuild project must be a child of a PropertyGroup element.
//  https://msdn.microsoft.com/en-us/library/t4w159bs.aspx
type PropertyGroup =
 {  Condition : Condition
    Children : Property list
 }



/// Stores task output values in items and properties.
//  https://msdn.microsoft.com/en-us/library/ms164287.aspx
type Output = 
 {  /// Required attribute - The name of the task's output parameter.
    TaskParameter : string
    Condition : Condition option
    (*
        PropertyName
        Either the PropertyName or ItemName attribute is required.
        The property that receives the task's output parameter value. Your project can then reference 
        the property with the $(PropertyName) syntax. This property name can either be 
        a new property name or a name that is already defined in the project.
        This attribute cannot be used if ItemName is also being used.
    
        ItemName

        Either the PropertyName or ItemName attribute is required.
        The item that receives the task's output parameter value. Your project can 
        then reference the item with the @(ItemName) syntax. The item name can either be a 
        new item name or a name that is already defined in the project.
        This attribute cannot be used if PropertyName is also being used.
    *)
    PropertyName : string
    ItemName     : string
    }


/// Causes one or more targets to execute, if the ContinueOnError attribute is false for a failed task.
//  https://msdn.microsoft.com/en-us/library/ms164285.aspx
type OnError = 
 {  /// The targets to execute if a task fails. Separate multiple targets with semicolons. 
    /// Multiple targets are executed in the order specified.
    ExecuteTargets : string list
    Condition : Condition option
    }


/// Contains information about a specific parameter for a task that is generated 
/// by a UsingTask TaskFactory.  The name of the element is the name of the parameter.
//  https://msdn.microsoft.com/en-us/library/ff606257.aspx
type Parameter =
 {  /// Optional attribute - The .NET type of the parameter, for example, "System.String".
    ParameterType : Type
    /// Optional Boolean attribute - If true, this parameter is an output 
    /// parameter for the task. By default, the value is false.
    Output : bool
    /// Optional Boolean attribute. - If true, this parameter is an required 
    /// parameter for the task. By default, the value is false
    Required : bool
    }


/// Contains an optional list of parameters that will be present on
/// the task that is generated by a UsingTask TaskFactory.
//  https://msdn.microsoft.com/en-us/library/ff606260.aspx
type ParameterGroup =
 {  Children : Parameter list
    }


/// Creates and executes an instance of an MSBuild task. The element name is 
/// determined by the name of the task being created.
// https://msdn.microsoft.com/en-us/library/77f2hx1s.aspx
type Task =
 {  Condition : Condition option

    // UNSURE WHAT THIS MEANS
    /// Required if the task class contains one or more properties labeled with the [Required] attribute.
    /// A user-defined task parameter that contains the parameter value as its value.
    /// There can be any number of parameters in the Task element, with each attribute 
    /// mapping to a .NET property in the task class.
    Parameter : Parameter list
    (*
        Optional attribute. Can contain one of the following values:
        WarnAndContinue or true. When a task fails, subsequent tasks in the Target element and the build continue to execute, and all errors from the task are treated as warnings.
        ErrorAndContinue. When a task fails, subsequent tasks in the Target element and the build continue to execute, and all errors from the task are treated as errors.
        ErrorAndStop or false (default). When a task fails, the remaining tasks in the Target element and the build aren't executed, and the entire Target element and the build is considered to have failed.
        Versions of the .NET Framework before 4.5 supported only the true and false values.
    *)
    ContinueOnError : bool // <- not the right type
    }


(*  Remarks on Tasks -
    A Task element in an MSBuild project file creates an instance of a task, sets properties 
    on it, and executes it. The Output element stores output parameters in properties or items
     to be used elsewhere in the project file.
    If there are any OnError elements in the parent Target element of a task, 
    they will still be evaluated if the task fails and ContinueOnError has a value of false. 

    More Info ::
    MSBUild Tasks   - https://msdn.microsoft.com/en-us/library/ms171466.aspx
    Task Writing    - https://msdn.microsoft.com/en-us/library/t9883dzc.aspx
    How to ignore errors in tasks - https://msdn.microsoft.com/en-us/library/ms171484.aspx
*)

/// Contains the data that is passed to a UsingTask TaskFactory. 
//  https://msdn.microsoft.com/en-us/library/ff606253.aspx
type TaskBody =
 {  /// Optional Boolean attribute - If true, MSBuild evaluates any inner elements, 
    /// and expands items and properties before it passes the information to the 
    /// TaskFactory when the task is instantiated
    Evaluate : bool
    /// The text between the TaskBody tags is sent verbatim to the TaskFactory
    Data : string
 }


/// Maps the task that is referenced in a Task element to the assembly that contains the task implementation.
// https://msdn.microsoft.com/en-us/library/t41tzex2.aspx
type UsingTask =
 {  Condition : Condition option
    /// Required attribute - The name of the task to reference from an assembly. If ambiguities are possible, 
    /// this attribute should always specify full namespaces. If there are ambiguities, MSBuild chooses an arbitrary 
    /// match, which could produce unexpected results.
    TaskName : string

    /// Optional attribute - Specifies the class in the assembly that is responsible for generating instances 
    /// of the specified Task name.  The user may also specify a TaskBody as a child element that the task factory 
    /// receives and uses to generate the task. The contents of the TaskBody are specific to the task factory.
    TaskFactory : string 
    AssemblyFile : string
    AssemblyName : string
    (*
    Either the AssemblyName attribute or the AssemblyFile attribute is required.
    The name of the assembly to load. The AssemblyName attribute accepts strong-named assemblies, 
    although strong-naming is not required. Using this attribute is equivalent to loading an assembly 
    by using the Load method in .NET.

    You cannot use this attribute if the AssemblyFile attribute is used.

    AssemblyFile
    Either the AssemblyName or the AssemblyFile attribute is required.
    The file path of the assembly. This attribute accepts full paths or relative paths. Relative paths are relative to the directory of the project file or targets file where the UsingTask element is declared. Using this attribute is equivalent to loading an assembly by using the LoadFrom method in .NET.
    You cannot use this attribute if the AssemblyName attribute is used.
    
    *)
    /// The data that is passed to the TaskFactory to generate an instance of the task.
    TaskBody : TaskBody option
    /// The set of parameters that appear on the task that is generated by the specified TaskFactory.
    Children : ParameterGroup list

    }


type TargetNode =
    | Task of Task
    | PropertyGroup of PropertyGroup
    | ItemGroup of ItemGroup
    | OnError of OnError

 /// Contains a set of tasks for MSBuild to execute sequentially.
 //  https://msdn.microsoft.com/en-us/library/t50z2hka.aspx
type Target = 
 {  /// Required attribute. - The name of the target. 
    Name : string
    /// Optional attribute - An identifier that can identify or order system and user elements.
    Label : string
    Condition : Condition option
    /// Optional attribute -The targets that must be executed before this target can be executed 
    /// or top-level dependency analysis can occur. Multiple targets are separated by semicolons.
    DependsOnTargets : string list
    /// Optional attribute - A semicolon-separated list of target names. When specified,
    /// indicates that this target should run after the specified target or targets.
    /// This lets the project author extend an existing set of targets without modifying them directly. 
    AfterTargets : string list
    /// Optional attribute - A semicolon-separated list of target names.  When specified,
    /// indicates that this target should run before the specified target or targets. This lets 
    /// the project author extend an existing set of targets without modifying them directly
    BeforeTargets : string list
    /// Optional Boolean attribute -If true, multiple references to the same item in 
    /// the target's Returns are recorded. By default, this attribute is false.
    KeepDuplicateOutputs : bool
    /// Optional attribute -The files that form inputs into this target. Multiple files are 
    /// separated by semicolons. The timestamps of the files will be compared with the 
    /// timestamps of files in Outputs to determine whether the Target is up to date. 
    Inputs : string list // <- unclear on what this type should be
    /// Optional attribute - The files that form outputs into this target. Multiple files are
    /// separated by semicolons. The timestamps of the files will be compared with the 
    /// timestamps of files in Inputs to determine whether the Target is up to date.
    Outputs : string list // <- unclear on what this type should be
    /// Optional attribute - The set of items that will be made available to tasks that invoke 
    /// this target, for example, MSBuild tasks. Multiple targets are separated by semicolons. 
    /// If the targets in the file have no Returns attributes, the Outputs attributes are used instead for this purpose.
    Returns : string list // <- unclear on what this type should be
    Children : TargetNode list
    
    
    }

(*  Reference Related to Targets
        - MSBuild Transforms - https://msdn.microsoft.com/en-us/library/ms171476.aspx
        - How to Build Incrementally - https://msdn.microsoft.com/en-us/library/ms171483.aspx
        - Incremental Builds - https://msdn.microsoft.com/en-us/library/ee264087.aspx
*)

(*  Remarks on Targets
    The first target to execute is specified at run time. Targets can have dependencies on 
    other targets. For example, a target for deployment depends on a target for compilation. 
    The MSBuild engine executes dependencies in the order in which they appear in the 
    DependsOnTargets attribute, from left to right. For more information, see MSBuild Targets.

    A target is only executed once during a build, even if more than one target has a dependency on it.

    If a target is skipped because its Condition attribute evaluates to false, it can still
    be executed if it is invoked later in the build and its Condition attribute evaluates 
    to true at that time.

    Before MSBuild 4, Target returned any items that were specified in the Outputs attribute. 
    To do this, MSBuild had to record these items in case tasks later in the build requested them. 
    Because there was no way to indicate which targets had outputs that callers would require, 
    MSBuild accumulated all items from all Outputs on all invoked Targets. This lead to scaling 
    problems for builds that had a large number of output items.

    If the user specifies a Returns on any Target element in a project, then only those Targets 
    that have a Returns attribute record those items.
    A Target may contain both an Outputs attribute and a Returns attribute. Outputs is used 
    with Inputs to determine whether the target is up-to-date. Returns, if present, overrides 
    the value of Outputs to determine which items are returned to callers. If Returns is not 
    present, then Outputs will be made available to callers except in the case described earlier.

    Before MSBuild 4, any time that a Target included multiple references to the same item in its 
    Outputs, those duplicate items would be recorded. In very large builds that had a large number
    of outputs and many project interdependencies, this would cause a large amount of memory to be 
    wasted because the duplicate items were not of any use. When the KeepDuplicateOutputs attribute 
    is set to true, these duplicates are recorded.
*)




/// Evaluates child elements to select one set of ItemGroup elements and/or PropertyGroup elements to evaluate.
//  https://msdn.microsoft.com/en-us/library/ms164282.aspx
type Choose =
 {  /// Required element - Specifies a possible block of code for the Choose element to select. 
    /// There may be one or more When elements in a Choose element.
    When : When
    /// Optional element - Specifies the block of code PropertyGroup and ItemGroup elements to evaluate 
    /// if the conditions of all When elements evaluate to false. There may be zero or one Otherwise 
    /// elements in a Choose element, and it must be the last element.
    Otherwise : Otherwise option
    /// Any additional whens in the choose
    Children : When list
    }
/// Specifies a possible block of code for the Choose element to select.
//  https://msdn.microsoft.com/en-us/library/ms164289.aspx
and When =
 {  /// Required attribute - Condition to evaluate.
    Condition : Condition
    /// Optional element - Evaluates child elements to select one section of code to execute. 
    /// There may be zero or more Choose elements in an When element.
    ChooseList      : Choose list
    /// Optional element - Contains a set of user-defined Item elements. There may be zero or 
    /// more ItemGroup elements in an When element.
    ItemGroupList   : ItemGroup list
    /// Optional element. Contains a set of user-defined Property elements. There may be zero or 
    /// more PropertyGroup elements in an When element.
    ProperyGroupList: PropertyGroup list
    }
/// Specifies the block of code to execute if and only if the conditions of all When elements evaluate to false.
//  https://msdn.microsoft.com/en-us/library/ms164286.aspx
and Otherwise =
 {  /// Optional element - Evaluates child elements to select one section of code to execute. 
    /// There may be zero or more Choose elements in an Otherwise element.
    ChooseList      : Choose list
    /// Optional element - Contains a set of user-defined Item elements. There may be zero or 
    /// more ItemGroup elements in an Otherwise element.
    ItemGroupList   : ItemGroup list
    /// Optional element. Contains a set of user-defined Property elements. There may be zero or 
    /// more PropertyGroup elements in an Otherwise element.
    ProperyGroupList: PropertyGroup list
 }
    static member Default = {
        ChooseList       = []
        ItemGroupList    = []
        ProperyGroupList = []
    }

(*
    The Choose, When, and Otherwise elements are used together to provide a way to select 
    one section of code to execute out of a number of possible alternatives.

    MSBuild Conditional Constructs - https://msdn.microsoft.com/en-us/library/ms164307.aspx
*)

/// Allows MSBuild project files to contain non-MSBuild information. Anything inside of a 
/// ProjectExtensions element will be ignored by MSBuild
//  https://msdn.microsoft.com/en-us/library/ycwcwzs7.aspx
type ProjectExtensions = { Value : string  }
 
/// Child elements of a project
/// Projects can contain zero or more of each of these elements
type ProjectNode =
    | Choose of Choose
    | Import of Import
    | ItemGroup of ItemGroup
    | PropertyGroup of PropertyGroup
    | Target of Target
    | UsingTask of UsingTask


/// Required root element of an MSBuild project file.
//  https://msdn.microsoft.com/en-us/library/bcxfsh87.aspx
type Project = 
 { 
    /// Optional attribute - The default target or targets to be the entry point of the build if no target has been specified. 
    /// If no default target is specified in either the engine executes the first target 
    /// in the project file after the Import elements have been evaluated.
    DefaultTargets : string list
    /// Optional attribute - The initial target or targets to be run before the targets specified 
    /// in the DefaultTargets attribute or on the command line. Multiple targets are semi-colon (;) delimited.
    InitialTargets : string list
    TreatAsLocalProperty : string list
    /// Optional attribute - The version of the toolset MSBuild uses to determine the values for 
    /// $(MSBuildBinPath) and $(MSBuildToolsPath).
    ToolsVersion : string option

    /// Provides a way to persist non-MSBuild information in an MSBuild project file. 
    /// There may be zero or one ProjectExtensions elements in a project.
    ProjectExtensions : ProjectExtensions option
    Children : ProjectNode list   
    }
    /// Required attribute.
    /// The xmlns attribute must have the value of "http://schemas.microsoft.com/developer/msbuild/2003".
    static member Xmlns = "http://schemas.microsoft.com/developer/msbuild/2003"

// TODO / NOTE - Maybe the targets should be Target lists not string lists

(*  Notes on Treat as local Property

    Property names that won't be considered to be global. This attribute prevents specific command-line properties 
    from overriding property values that are set in a project or targets file and all subsequent imports. 
    Multiple properties are semi-colon (;) delimited.

    Normally, global properties override property values that are set in the project or targets file. 
    If the property is listed in the TreatAsLocalProperty value, the global property value doesn't 
    override property values that are set in that file and any subsequent imports.

    How To Build the Same Source Files with Different Options
    -- https://msdn.microsoft.com/en-us/library/ms171481.aspx

*)

