#!/usr/bin/env node

import { createReadStream, createWriteStream } from "fs"
import split from "split2"
import filter from "through2-filter"
import map from "through2-map"
import through from "through2"

const source = new URL(
    "../vendor/dolph/dictionary/popular.txt",
    import.meta.url
)
const target = new URL("../build/dictionary.json", import.meta.url)

createReadStream(source)
    .pipe(split())
    .pipe(
        filter({ wantStrings: true }, (rawWord) => {
            return rawWord.length >= 3 && rawWord.length <= 9
        })
    )
    .pipe(
        map({ wantStrings: true }, (rawWord, index) => {
            const word = JSON.stringify(rawWord)

            return index === 0 ? `{"dictionary":\n[${word}\n` : `,${word}\n`
        })
    )
    .pipe(
        through({}, null, function (flush) {
            this.push("]}\n")
            flush()
        })
    )
    .pipe(createWriteStream(target))
    .on("finish", () => {
        console.log("dictionary.json generated")
    })
