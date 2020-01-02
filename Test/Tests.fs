module Tests

open System
open Xunit
open System.IO
open DumbLang

[<Fact>]
let ``Base Parser Test`` () =

    let text = File.ReadAllText(@"..\..\..\..\DumbLang\Program.script")

    let compilation = BaseParser().parse text

    let test =
        [
            [ Label "First"; Label "Second" ] |> SingleNode |> Anonymus
            ( Label "Name1", [ Label "Third"; Label "Fourth" ] |> SingleNode ) |> Named
            [
                [] |> SingleNode |> Anonymus
                [] |> GroupNode |> Anonymus
            ] |> GroupNode |> Anonymus
            (
                Label "Name2",
                [
                    ( Label "Name3", [ Label "Fifth" ] |> SingleNode ) |> Named
                    [ [ Label "Sixth" ] |> SingleNode |> Anonymus ] |> GroupNode |> Anonymus
                ] |> GroupNode
            ) |> Named
        ] |> GroupNode |> Anonymus

    Assert.Equal(test, compilation)

[<Fact>]
let ``DumbLang Parser Test`` () =

    let text = File.ReadAllText(@"..\..\..\..\DumbLang\Program.script")

    let compilation = DumbLangParser().parse text

    let test =
        [
            [ Label "First"; Label "Second" ] |> SingleNode |> Anonymus
            ( Label "Name1", [ Label "Third"; Label "Fourth" ] |> SingleNode ) |> Named
            [
                [] |> SingleNode |> Anonymus
                [] |> GroupNode |> Anonymus
            ] |> GroupNode |> Anonymus
            (
                Label "sName2",
                [
                    ( Label "sName3", [ Label "sFifth" ] |> SingleNode ) |> Named
                    [ [ Label "ssSixth" ] |> SingleNode |> Anonymus ] |> GroupNode |> Anonymus
                ] |> GroupNode
            ) |> Named
        ] |> GroupNode |> Anonymus

    Assert.Equal(test, compilation)
