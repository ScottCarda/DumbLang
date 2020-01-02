namespace DumbLang

type Label(value : string) =
    let mutable _Value = value
    member this.Value with get() = _Value and set(value) = _Value <- value
    override this.GetHashCode() =
        hash value
    override this.Equals(b) =
        match b with
        | :? Label as l -> value = l.Value
        | _ -> false
    static member Default() = Label("")

type Element =
    | Anonymus of Node
    | Named of Label * Node
    with
    static member DefaultAnonymus() = Node.DefaultSingleNode() |> Anonymus
    static member DefaultNamed() = (Label.Default(), Node.DefaultSingleNode()) |> Named
    
and Node =
    | SingleNode of Label list
    | GroupNode of Element list
    with
    static member DefaultSingleNode() = SingleNode []
    static member DefaultGroupNode() = GroupNode []