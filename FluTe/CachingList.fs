namespace Whatever
open System.Collections
open System.Collections.Generic
[<AbstractClass>]
type MNode<'node,'v when 'node :> MNode<'node,'v>>(value : 'v, tail : MList<'node,'v>) = 
    member this.Tail = tail
    member this.Head = value
    member this.Typed = this :?> 'node
    abstract Create : 'v -> MList<'node,'v> -> 'node
and MList<'node,'v when 'node :> MNode<'node,'v>> =
| Nil
| Node of MNode<'node,'v>

type LengthNode<'v> (value : 'v, tail : MList<LengthNode<'v>,'v>) = 
    inherit MNode<LengthNode<'v>,'v>(value,tail)
    member val Length = match tail with | Nil -> 1 | Node next -> 1 + next.Typed.Length
    override this.Create v t = LengthNode(v,t)
module MemList = 
    let inline mlist (x : MList<_,_>) = x
    let inline fst l = match l |> mlist with | Nil -> failwith "List is empty." | node -> node
    let inline head ml = ml |> mlist |> (function | Nil -> failwith "No head" | Node x -> x.Head)
    let inline tail ml = ml |> mlist |> (function | Nil -> failwith "No tail" | Node x -> x.Tail)
    let inline cons o ml = ml |> mlist |> (fun x -> 

