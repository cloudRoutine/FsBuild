module FsBuild.Cexprs
open System open FsBuild.Structure

type CustOp = CustomOperationAttribute

type ImportBuilder () =
    member __.Yield (_) =  Import.empty
    member __.Zero  () =   Import.empty
    [<CustOp "Condition">] /// Set the Condition for the Import being generated
    member __.Condition (x:Import, cnd) = { x with Condition = Some cnd }


type ImportGroupBuilder () =
    // produce an empty importgroup if nothing is set in the cexpr
    member __.Yield (_) = ImportGroup.empty
    member __.Zero  ()  = ImportGroup.empty
    [<CustOp "Condition">] /// Set the Condition for the ImportGroup being generated  
    member __.Condition (x:ImportGroup, cnd) = { x with Condition = Some cnd }
    [<CustOp "Import">]
    member __.Import (x:ImportGroup, imp)  = x.Cons imp
    [<CustOp "ImportProject">]
    member __.ImportProject (x:ImportGroup, proj:string)  =  x.Cons { Project = proj; Condition = None}
    [<CustOp "AppendImports">]
    member __.AppendImports (x:ImportGroup,col:#seq<_>)  = x.Append (Seq.toList col)


type ItemBuilder () =
    member __.Yield (_) = Item.empty
    member __.Zero  ()  = Item.empty
    [<CustOp "Condition">] /// Set the Condition for the ImportGroup being generated  
    member __.Condition (x:Item, cnd) = { x with Condition = Some cnd }
    [<CustOp "Exclude">] 
    member __.Exclude (x:Item, elm) =  { x with Exclude = elm }
    [<CustOp "Remove">] 
    member __.Remove (x:Item, elm) =  { x with Remove = elm }
    [<CustOp "KeepDuplicates">] 
    member __.KeepDuplicates (x:Item, elm) = { x with KeepDuplicates = elm }
    [<CustOp "KeepMetadata">] 
    member __.KeepMetadata (x:Item, elm) = { x with KeepMetadata  = elm }
    [<CustOp "RemoveMetadata">] 
    member __.RemoveMetadata (x:Item, elm) = { x with RemoveMetadata = elm }    
    [<CustOp "Children">] 
    member __.Children (x:Item, elm) = { x with Children = elm }


type ItemGroupBuilder () =
    member __.Yield (_) = ItemGroup.empty
    member __.Zero  ()  = ItemGroup.empty
    [<CustOp "Condition">] /// Set the Condition for the ImportGroup being generated  
    member __.Condition (x:ItemGroup, cnd) = { x with Condition = Some cnd }
    [<CustOp "Children">] 
    member __.Children (x:ItemGroup, elm) = { x with Children = elm }


type ItemDefinitionGroupBuilder () =
    member __.Yield (_) = ItemDefinitionGroup.empty
    member __.Zero  ()  = ItemDefinitionGroup.empty
    [<CustOp "Condition">] /// Set the Condition for the ImportGroup being generated  
    member __.Condition (x:ItemDefinitionGroup, cnd) = { x with Condition = Some cnd }
    [<CustOp "Children">] 
    member __.Children (x:ItemDefinitionGroup, elm) = { x with Children = elm }


type PropertyBuilder () =
    member __.Yield (_) = Property.empty
    member __.Zero  ()  = Property.empty
    [<CustOp "Condition">] /// Set the Condition for the ImportGroup being generated  
    member __.Condition (x:Property, cnd) = { x with Condition = Some cnd }
    [<CustOp "Value">] 
    member __.Value (x:Property, elm) = { x with Value = elm }


type PropertyGroupBuilder () =
    member __.Yield (_) = PropertyGroup.empty
    member __.Zero  ()  = PropertyGroup.empty
    [<CustOp "Condition">] /// Set the Condition for the ImportGroup being generated  
    member __.Condition (x:PropertyGroup, cnd) = { x with Condition = Some cnd }
    [<CustOp "Children">] 
    member __.Children (x:PropertyGroup, elm) = { x with Children = elm }


type OutputBuilder () =
    member __.Yield (_) = Output.empty
    member __.Zero  ()  = Output.empty
    [<CustOp "TaskParameter">] 
    member __.TaskParameter (x:Output, elm) = { x with TaskParameter = elm }
    [<CustOp "Condition">] 
    member __.Condition (x:Output, elm) = { x with Condition = elm }
    [<CustOp "PropertyName">] 
    member __.PropertyName (x:Output, elm) = { x with PropertyName = elm }
    [<CustOp "ItemName">] 
    member __.ItemName (x:Output, elm) = { x with ItemName = elm }


type OnErrorBuilder () =
    member __.Yield (_) = OnError.empty
    member __.Zero  ()  = OnError.empty
    [<CustOp "ExecuteTargets">] 
    member __.ExecuteTargets (x:OnError, elm) = { x with ExecuteTargets = elm }
    [<CustOp "Condition">] 
    member __.Condition (x:OnError, elm) = { x with Condition = elm }


type ParameterBuilder () =
    member __.Yield (_) = Parameter.empty
    member __.Zero  ()  = Parameter.empty
    [<CustOp "ParameterType">] 
    member __.ParameterType (x:Parameter, elm) = { x with ParameterType = elm }
    [<CustOp "Output">] 
    member __.Output (x:Parameter, elm) = { x with Output = elm }
    [<CustOp "Required">] 
    member __.Required (x:Parameter, elm) = { x with Required = elm }


type ParameterGroupBuilder () =
    member __.Yield (_) = ParameterGroup.empty
    member __.Zero  ()  = ParameterGroup.empty
    [<CustOp "Children">] 
    member __.Children (x:ParameterGroup, elm) = { x with Children = elm }


type TaskBuilder () =
    member __.Yield (_) = Task.empty
    member __.Zero  ()  = Task.empty
    [<CustOp "Condtion">] 
    member __.Condtion (x:Task, elm) = { x with Condition = elm }
    [<CustOp "Parameter">] 
    member __.Parameter (x:Task, elm) = { x with Parameter = elm }
    [<CustOp "ContinueOnError">] 
    member __.ContinueOnError (x:Task, elm) = { x with ContinueOnError = elm }


type TaskBodyBuilder () =
    member __.Yield (_) = TaskBody.empty
    member __.Zero  ()  = TaskBody.empty
    [<CustOp "Evaluate">] 
    member __.Evaluate (x:TaskBody, elm) = { x with Evaluate = elm }
    [<CustOp "Value">] 
    member __.Value (x:TaskBody, elm) = { x with Value = elm }
    

type UsingTaskBuilder () =
    member __.Yield (_) = UsingTask.empty
    member __.Zero  ()  = UsingTask.empty
    [<CustOp "Condition">] 
    member __.Condition (x:UsingTask, elm) = { x with Condition = elm }
    [<CustOp "TaskName">] 
    member __.TaskName (x:UsingTask, elm) = { x with TaskName = elm }
    [<CustOp "TaskFactory">] 
    member __.TaskFactory (x:UsingTask, elm) = { x with TaskFactory = elm }
    [<CustOp "AssemblyFile">] 
    member __.AssemblyFile (x:UsingTask, elm) = { x with AssemblyFile = elm }
    [<CustOp "AssemblyName">] 
    member __.AssemblyName (x:UsingTask, elm) = { x with AssemblyName = elm }
    [<CustOp "TaskBody">] 
    member __.TaskBody (x:UsingTask, elm) = { x with TaskBody = elm }
    [<CustOp "Children">] 
    member __.Children (x:UsingTask, elm) = { x with Children = elm }


type TargetBuilder () =
    member __.Yield (_) = Target.empty
    member __.Zero  ()  = Target.empty
    [<CustOp "Name">] 
    member __.Name (x:Target, elm) = { x with Name = elm }
    [<CustOp "Label">] 
    member __.Label (x:Target, elm) = { x with Label = elm }
    [<CustOp "Condition">] 
    member __.Condition (x:Target, elm) = { x with Condition = elm }
    [<CustOp "DependsOnTargets">] 
    member __.DependsOnTargets (x:Target, elm) = { x with DependsOnTargets = elm }
    [<CustOp "AfterTargets">] 
    member __.AfterTargets (x:Target, elm) = { x with AfterTargets = elm }
    [<CustOp "BeforeTargets">] 
    member __.BeforeTargets (x:Target, elm) = { x with BeforeTargets = elm }
    [<CustOp "KeepDuplicateOutputs">] 
    member __.KeepDuplicateOutputs (x:Target, elm) = { x with KeepDuplicateOutputs = elm }
    [<CustOp "Inputs">] 
    member __.Inputs (x:Target, elm) = { x with Inputs = elm }
    [<CustOp "Outputs">] 
    member __.Outputs (x:Target, elm) = { x with Outputs = elm }
    [<CustOp "Returns">] 
    member __.Returns (x:Target, elm) = { x with Returns = elm }
    [<CustOp "Chilren">] 
    member __.Chilren (x:Target, elm) = { x with Children = elm }


type ChooseBuilder () =
    member __.Yield (_) = Choose.empty
    member __.Zero  ()  = Choose.empty
    [<CustOp "When">] 
    member __.When (x:Choose, elm) = { x with When = elm }
    [<CustOp "Otherwise">] 
    member __.Otherwise (x:Choose, elm) = { x with Otherwise = elm }
    [<CustOp "Children">] 
    member __.Children (x:Choose, elm) = { x with Children = elm }


type WhenBuilder () =
    member __.Yield (_) = When.empty
    member __.Zero  ()  = When.empty
    [<CustOp "Condition">] 
    member __.Condition (x:When, elm) = { x with Condition = elm }
    [<CustOp "ChooseList">] 
    member __.ChooseList (x:When, elm) = { x with ChooseList = elm }
    [<CustOp "ItemGroupList">] 
    member __.ItemGroupList (x:When, elm) = { x with ItemGroupList = elm }
    [<CustOp "PropertyGroupList">] 
    member __.PropertyGroupList (x:When, elm) = { x with ProperyGroupList = elm }


type OtherwiseBuilder () =
    member __.Yield (_) = Otherwise.empty
    member __.Zero  ()  = Otherwise.empty
    [<CustOp "ChooseList">] 
    member __.ChooseList (x:Otherwise, elm) = { x with ChooseList = elm }
    [<CustOp "ItemGroupList">] 
    member __.ItemGroupList (x:Otherwise, elm) = { x with ItemGroupList = elm }
    [<CustOp "PropertyGroupList">] 
    member __.PropertyGroupList (x:Otherwise, elm) = { x with ProperyGroupList = elm }


type ProjectExtensionsBuilder () =
    member __.Yield (_) = ProjectExtensions.empty
    member __.Zero  ()  = ProjectExtensions.empty
    [<CustOp "Value">] 
    member __.Value (x:ProjectExtensions, elm) = { x with Value = elm }


type ProjectBuilder () =
    member __.Yield (_) = Project.empty
    member __.Zero  ()  = Project.empty
    [<CustOp "DefaultTargets">] 
    member __.DefaultTargets (x:Project, elm) = { x with DefaultTargets = elm }
    [<CustOp "InitialTargets">] 
    member __.InitialTargets (x:Project, elm) = { x with InitialTargets = elm }
    [<CustOp "TreatAsLocalProperty">] 
    member __.TreatAsLocalProperty (x:Project, elm) = { x with TreatAsLocalProperty = elm }
    [<CustOp "ToolsVersion">] 
    member __.ToolsVersion (x:Project, elm) = { x with ToolsVersion = elm }
    [<CustOp "ProjectExtensions">] 
    member __.ProjectExtensions (x:Project, elm) = { x with ProjectExtensions = elm }
    [<CustOp "Children">] 
    member __.Children (x:Project, elm) = { x with Children = elm }


//=====================================
//  CExpression Keywords
//=====================================

let import              = ImportBuilder              ()
let importGroup         = ImportGroupBuilder         ()
let item                = ItemBuilder                ()
let itemDefinitionGroup = ItemDefinitionGroupBuilder ()
let property            = PropertyBuilder            ()
let propertyGroup       = PropertyGroupBuilder       ()
let output              = OutputBuilder              ()
let onError             = OnErrorBuilder             ()
let parameter           = ParameterBuilder           ()
let parameterGroup      = ParameterGroupBuilder      ()
let task                = TaskBuilder                ()
let taskBody            = TaskBodyBuilder            ()
let usingTask           = UsingTaskBuilder           ()
let target              = TargetBuilder              ()
let choose              = ChooseBuilder              ()
let when_bld            = WhenBuilder                ()        
let otherwise           = OtherwiseBuilder           ()
let projectExtension    = ProjectExtensionsBuilder   ()
let project             = ProjectBuilder             ()

