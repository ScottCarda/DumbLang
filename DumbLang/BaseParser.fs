namespace DumbLang

open FParsec
open System

exception Error of string

type BaseParser() =

    (* Label *)
    
    abstract member onLabel : string -> int -> int -> Label
    default this.onLabel str row col = { Value = str; Position = row, col }
    
    abstract member onSingleNode : Label list -> int -> int -> Node
    default this.onSingleNode labels row col = SingleNode { Labels = labels; Position = row, col }

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
        
        let label =
            spaces >>?
            getPosition .>>?
            quote .>>.? manyCharsTill anyChar quote |>> fun (pos, text) -> 
            this.onLabel text (int pos.Line) (int pos.Column)

        //let single = many label |> betweenBraces |>> SingleNode
        //let group = many element |> betweenBrackets |>> GroupNode
        
        let single = spaces >>?
                     getPosition .>>?
                     skipStringCI "{" .>>.?
                     many label .>>?
                     spaces .>>?
                     skipStringCI "}" |>> fun (pos, labels) ->
                     this.onSingleNode labels (int pos.Line) (int pos.Column)

        let group = openBracket  >>=? fun () ->
                    many element >>=? fun elems ->
                    closeBracket >>=? fun () ->
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