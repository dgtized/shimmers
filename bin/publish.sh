#!/bin/bash -ex

# rm -rf resources/public/.git

clojure -m figwheel.main -bo release

mkdir -p resources/public/js
cp target/public/cljs-out/release-main* resources/public/js

pushd resources/public

sed -i -e 's/cljs-out\/dev-main.js/js\/release-main.js/' index.html

# TODO: add circle ci exclusion for gh-pages?

# git init
# git checkout -b gh-pages
# git add index.html .gitignore js/* css/*
# git commit -m "Deploy to Github Pages"
# git push --force --quiet "git@github.com:dgtized/shimmers.git" gh-pages:gh-pages

xdg-open index.html

popd
