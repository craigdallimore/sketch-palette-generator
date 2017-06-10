#!/usr/bin/env bash
git checkout master
pulp build --optimise --to dist/main.js
git checkout gh-pages
node build-css.js
./node_modules/uglify-js/bin/uglifyjs -c -m -o ./dist/main.min.js -- ./dist/main.js
