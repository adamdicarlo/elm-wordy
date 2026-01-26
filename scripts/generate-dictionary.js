#!/usr/bin/env node

import { readFileSync, writeFileSync } from "fs"
import { relative } from "path"

const PROJECT_ROOT = process.env.DEVENV_ROOT;
if (!PROJECT_ROOT) {
  throw Error("Script must be run from within a devenv shell");
}

const dictionaryPath = `${PROJECT_ROOT}/vendor/dolph/dictionary/popular.txt`;
const targetPath = `${PROJECT_ROOT}/generated/Dictionary.elm`;

const rawContent = readFileSync(dictionaryPath, { encoding: "utf8" })
const allWords = rawContent.split("\n")
const legalWords = allWords.filter(
    (rawWord) => rawWord.length >= 3 && rawWord.length <= 9
)

const strings = legalWords.map((word) => `"${word}"`).join("\n    , ")
const elmModule = `module Dictionary exposing (Dictionary, words)

import Dict exposing (Dict)


{-| The set of legal words.

A Set would make more sense for this than a Dict (ironically). But a set with more than around 3550
elements causes the Elm debugger to crash. <https://github.com/elm/browser/issues/132>

-}
type alias Dictionary =
    Dict String ()


words : Dictionary
words =
    wordList
        |> List.map (\\word -> ( word, () ))
        |> Dict.fromList


wordList : List String
wordList =
    [ ${strings}
    ]
`

writeFileSync(targetPath, elmModule)

console.log(`Generated ${relative(process.cwd(), targetPath)}`)
