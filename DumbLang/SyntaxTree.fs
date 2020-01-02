namespace DumbLang

open FParsec

type Label2(value : string) =
    let mutable _Value = value
    member this.Value with get() = _Value and set(value) = _Value <- value
    override this.GetHashCode() =
        hash value
    override this.Equals(b) =
        match b with
        | :? Label2 as l -> value = l.Value
        | _ -> false
    //static member Default() = Label2("")

type Label =
    {Value : string; Position: int * int}
    //with
    //static member Default() = {Value = ""; Position = -1, -1}

type SingleNode =
    { Labels : Label list; Position : int * int }

type Element =
    | Anonymus of Node
    | Named of Label * Node
    //with
    //static member DefaultAnonymus() = Node.DefaultSingleNode() |> Anonymus
    //static member DefaultNamed() = (Label.Default(), Node.DefaultSingleNode()) |> Named
    
and Node =
    | SingleNode of SingleNode
    | GroupNode of Element list
    //with
    //static member DefaultSingleNode() = SingleNode []
    //static member DefaultGroupNode() = GroupNode []