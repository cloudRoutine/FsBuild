namespace FsBuild

[<AutoOpen>]
module  Metadata =
    
    
    /// Contains the full path of the item. For example:
    /// C:\MyProject\Source\Program.cs
    let FullPath = "FullPath"
    
    /// Contains the root directory of the item. For example: C:\
    let RootDir = "RootDir"
    
    /// Contains the file name of the item, without the extension. For example: Program
    let Filename = "Filename"
    
    /// Contains the file name extension of the item. For example: .cs
    let Extension = "Extension"

    /// Contains the path specified in the Include attribute, up to the final backslash (\). For example: Source\
    let RelativeDir ="RelativeDir"

    /// Contains the directory of the item, without the root directory. For example: MyProject\Source\
    let Directory = "Directory"

    /// If the Include attribute contains the wildcard **, this metadata specifies the part of the path that replaces the wildcard. For more information on wildcards, see How to: Select the Files to Build.
    /// If the folder C:\MySolution\MyProject\Source\ contains the file Program.cs, and if the project file contains this item:
    /// <ItemGroup>
    /// <MyItem Include="C:\**\Program.cs" />
    /// </ItemGroup>
    /// then the value of %(MyItem.RecursiveDir) would be MySolution\MyProject\Source\.
    let RecursiveDir = "RecursiveDir"

    /// The item specified in the Include attribute.. For example:     Source\Program.cs
    let Identity = "Identity"

    /// Contains the timestamp from the last time the item was modified. For example:     2004-07-01 00:21:31.5073316
    let ModifiedTime = "ModifiedTime"

    /// Contains the timestamp from when the item was created. For example: 2004-06-25 09:26:45.8237425
    let CreatedTime = "CreatedTime"

    /// Contains the timestamp from the last time the item was accessed.     2004-08-14 16:52:36.3168743
    let AccessedTime = "AccessedTime"
    


