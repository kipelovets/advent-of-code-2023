open System.IO
open System
open System.Xml

let args = fsi.CommandLineArgs
if args.Length = 1 then
    printfn $"Usage: {args[0]} <day number>"
    exit 1

let num = int args[1]
let codeProj = "AdventOfCode"
let testProj = "AdventOfCode.Tests"
let codeFile = $"Day{num}.fs"
let testFile = $"Day{num}Test.fs"
let inputFile = $"day{num}.input"


File.WriteAllText ($"{codeProj}/{codeFile}", "")
File.WriteAllText ($"{codeProj}/{inputFile}", "")
File.WriteAllText ($"{testProj}/{testFile}", "")

let addProjectRef (projFileName:string) (fileName:string) (inputFile:string) =
    let xml = File.ReadAllText projFileName
    let doc = new XmlDocument() in
        doc.LoadXml xml;
        let newNode = doc.CreateNode(XmlNodeType.Element, "Compile", "");
        let attr = doc.CreateAttribute("Include");
        attr.Value <- fileName;
        newNode.Attributes.Append(attr) |> ignore;
        let items = doc.SelectSingleNode "//Project/ItemGroup";
        items.AppendChild(newNode) |> ignore;

        (if inputFile.Length > 0 then
            let contentNode = doc.CreateElement("Content")
            let attr = doc.CreateAttribute("Include")
            attr.Value <- inputFile
            contentNode.Attributes.Append(attr) |> ignore
            let copyNode = doc.CreateElement("CopyToOutputDirectory")
            copyNode.InnerText <- "Always"
            contentNode.AppendChild(copyNode) |> ignore
            let inputs = doc.SelectNodes "//Project/ItemGroup"
            inputs[inputs.Count - 1].AppendChild(contentNode) |> ignore );

        let writer = new XmlTextWriter(projFileName, Text.Encoding.UTF8) in
            writer.Formatting <- System.Xml.Formatting.Indented;
            doc.WriteTo(writer);
            writer.Flush();

    
addProjectRef $"{codeProj}/{codeProj}.fsproj" codeFile ""
addProjectRef $"{testProj}/{testProj}.fsproj" testFile inputFile
