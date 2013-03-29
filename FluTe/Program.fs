// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module FluTe.Main
open System
open System.Text
open FParsec
open FluTe.Parser
open FluTe.Core
open System.Diagnostics
open Whatever
let pause() = 
    do Console.Read() |> ignore
type Prsn = {FirstName : string; LastName : string}
[<EntryPoint>]
let rec main argv = 
    let r = parseTemplate "My name is {blah: person.FirstName} {person.LastName}. I'm {age} years old..."
    let r = r.AttachTkStep("blah",ProcessingSteps.LambdaStep(fun x -> x.ToString().ToUpper() :> _))
    let r = r.Inst.Bind("person", {FirstName = "Greg"; LastName = "Ros"}).Bind("age", 15)
    do printfn "%s" r.Eval
    pause()

    0



