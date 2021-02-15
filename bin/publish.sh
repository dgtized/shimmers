#!/bin/bash -ex

function cleanup() {
    rm -rf resources/public/.git target
}

cleanup

npm install
clojure -M -m figwheel.main -bo release

mkdir -p resources/public/js
cp target/public/cljs-out/release-main* resources/public/js

pushd resources/public

sed -i -e 's/cljs-out\/release-main.js/js\/release-main.js/' index.html
# For routing to page handler after not using fragments
# ln -sf index.html 404.html

# TODO: add circle ci exclusion for gh-pages?

git init
git checkout -b gh-pages
git add index.html .gitignore js/* css/* shaders/*
git commit -m "Deploy to Github Pages"
git push --force --quiet "git@github.com:dgtized/shimmers.git" gh-pages:gh-pages

# xdg-open index.html

popd

# cleanup nested .git, and checkout correct version of index.html
cleanup
git checkout resources/public/index.html
