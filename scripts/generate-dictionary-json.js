#!/usr/bin/env node

const fs = require('fs')
const path = require('path')
const split = require('split2')
const filter = require('through2-filter')
const map = require('through2-map')
const through = require('through2')

fs.createReadStream(path.resolve(__dirname, '../vendor/dolph/dictionary/popular.txt'))
  .pipe(split())
  .pipe(filter({wantStrings: true}, (rawWord) => {
    return rawWord.length >= 3 && rawWord.length <= 9
  }))
  .pipe(map({wantStrings: true}, (rawWord, index) => {
    const word = JSON.stringify(rawWord)

    return index === 0
      ? `{"dictionary":[${word}\n`
      : `,${word}\n`
  }))
  .pipe(through({}, null, function (flush) {
    this.push(']}\n')
    flush()
  }))
  .pipe(fs.createWriteStream(path.resolve(__dirname, '../dictionary.json')))
  .on('finish', function () {
    console.log('dictionary.json generated')
  })
