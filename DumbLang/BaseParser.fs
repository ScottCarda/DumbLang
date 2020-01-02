namespace DumbLang

open FParsec
open System

exception Error of string

type BaseParser() =

    abstract member onLabel : string -> Label
    default this.onLabel str = Label(str)

    abstract member onSingleNode : Label list -> Node
    default this.onSingleNode labels = SingleNode labels

    abstract member onGroupNode : Element list -> Node
    default this.onGroupNode elems = GroupNode elems

    abstract member onAnonymus : Node -> Element
    default this.onAnonymus node = Anonymus node

    abstract member onNamed : Label * Node -> Element
    default this.onNamed (name, node) = Named (name, node)
    
    member private this.topParser =
        (* Utility *)

        //let between a b p = a >>. p .>> b
        
        (* Terminals *)
        
        let quote = skipStringCI "\""
        
        let openBrace = spaces >>? skipStringCI "{"
        let closeBrace = spaces >>? skipStringCI "}"
        //let betweenBraces = between openBrace closeBrace
        
        let openBracket = spaces >>? skipStringCI "["
        let closeBracket = spaces >>? skipStringCI "]"
        //let betweenBrackets = between openBracket closeBracket
        
        let colon = spaces >>? skipStringCI ":"

        (* Nonterminals *)
        
        let element, elementImpl = createParserForwardedToRef()
        let node, nodeImpl = createParserForwardedToRef()
        
        let label = spaces >>? quote >>. manyCharsTill anyChar quote |>> this.onLabel

        //let single = many label |> betweenBraces |>> SingleNode
        //let group = many element |> betweenBrackets |>> GroupNode
        
        let single = openBrace  >>=? fun _ ->
                     many label >>=? fun labels ->
                     closeBrace >>=? fun _ ->
                     preturn labels |>> this.onSingleNode

        let group = openBracket  >>=? fun _ ->
                    many element >>=? fun elems ->
                    closeBracket >>=? fun _ ->
                    preturn elems |>> this.onGroupNode

        do nodeImpl := choice[single; group]
        
        let anonymus = node |>> this.onAnonymus
        let named = label >>=? fun name ->
                    colon >>=? fun _ ->
                    node  >>=? fun node ->
                    preturn (name, node) |>> this.onNamed
        
        do elementImpl := choice[anonymus; named]

        element
    
    (* Parser Driver *)

    member public this.parse input =
        match run (this.topParser .>> eof) input with
        | Success(r,_,_) -> r
        | Failure(r,_,_) ->
                Console.WriteLine r
                raise (Error(r))