#!/usr/bin/env bash
git add -f dist
git commit -m "deploy"
git push origin gh-pages
git rm -r dist
git commit -m "clean"
mkdir dist
