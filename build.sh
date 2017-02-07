#/bin/sh
elm-make --yes src/Main.elm --output=dist/wordy.elm.js
google-closure-compiler-js --compilationLevel ADVANCED --languageIn ES5 dist/wordy.elm.js > dist/wordy.elm.min.js
mv dist/wordy.elm.min.js dist/wordy.elm.js
