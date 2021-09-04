#!/bin/bash -ex

function cleanup() {
    rm -rf resources/public/{.git,js} target
}

cleanup

npm install
clojure -M -m figwheel.main -bo release

# include git-sha in js filename to force update of CDN cache
sha=$(git rev-parse HEAD | cut -c 1-8)

mkdir -p resources/public/js
cp target/public/cljs-out/release-main.js "resources/public/js/release-main-${sha}.js"
cp target/public/cljs-out/release-main.js.map "resources/public/js/release-main-${sha}.js.map"

pushd resources/public

sed -i -e "s/cljs-out\/release-main.js/js\/release-main-${sha}.js/" index.html
# For routing to page handler after not using fragments
# ln -sf index.html 404.html

# TODO: add circle ci exclusion for gh-pages?

git init
git checkout -b gh-pages
git add index.html .gitignore js/* css/* shaders/*
git commit -m "Deploy to Github Pages"
git push --force --quiet "git@github.com:dgtized/shimmers.git" gh-pages:gh-pages

ls -hs js/release-main*

# xdg-open index.html

popd

# cleanup nested .git, and checkout correct version of index.html
cleanup
git checkout resources/public/index.html
