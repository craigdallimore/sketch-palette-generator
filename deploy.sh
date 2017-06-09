#!/usr/bin/env bash
git checkout master
pulp build -O dist/main.js
git checkout gh-pages
node build-css.js
git add -f dist
git commit -m "deploy"
git push origin gh-pages
git rm -r dist
git commit -m "clean"
mkdir dist
