module ForbidSpecificImports exposing (rule, Config)

{-|

@docs Config
@docs rule

-}

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule, Error)


{-|-}
type alias Config =
    List ( String, List String )


type alias Context =
    String


{-| Reports when a module in a namespace imports from a list of other namespaces.

    config : List Rule
    config =
        [ ForbidSpecificImports.rule
            [ ( "App.Data", [ "App.View" ] )
            ]
        ]


## Fail

This fails because we've forbidden modules in `App.Data` to import from `App.View`.

    module App.Data.Image exposing (..)

    import App.View.Image


## Success

This passes because we've only forbidden modules in `App.Data` to import from `App.View`, not the
other way around.

    module App.View.Image exposing (..)

    import App.Data.Image


## When (not) to enable this rule

This rule is useful when you have set up a namespace structure where you want your module
dependencies to form a [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph).

-}
rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchema "ForbidSpecificImports" ""
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor (importVisitor config)
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> Context -> ( List (Error {}), Context )
moduleDefinitionVisitor node context =
    ( []
    , Node.value node |> Module.moduleName |> String.join "."
    )


importVisitor : Config -> Node Import -> Context -> ( List (Error {}), Context )
importVisitor config node currentModule =
    let
        importedModule =
            Node.value node |> .moduleName |> Node.value |> String.join "."
    in
    ( List.foldl
        (\( modulePrefix, importPrefixes ) errors ->
            if String.startsWith modulePrefix currentModule then
                List.foldl
                    (\importPrefix ->
                        if String.startsWith importPrefix importedModule then
                            (::)
                                (Rule.error
                                    { message = errorMessage currentModule importedModule
                                    , details = errorDetails modulePrefix importPrefix
                                    }
                                    (Node.range node)
                                )

                        else
                            identity
                    )
                    errors
                    importPrefixes

            else
                errors
        )
        []
        config
    , currentModule
    )


errorMessage : String -> String -> String
errorMessage currentModule importedModule =
    "Module `" ++ currentModule ++ "` imports `" ++ importedModule ++ "`."


errorDetails : String -> String -> List String
errorDetails modulePrefix importPrefix =
    [ "Modules in `" ++ modulePrefix ++ "` must not import from `" ++ importPrefix ++ "`."
    ]
