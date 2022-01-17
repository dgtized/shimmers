# shimmers

[![CI](https://github.com/dgtized/shimmers/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/dgtized/shimmers/actions/workflows/test.yml)

Shimmering quil sketches; a few particle simulations, video processing
experiments, randomly generated L-systems, 3d transformations, and cellular
automata.

https://dgtized.github.io/shimmers

Note: Some sketches require camera input, so it will prompt for video
permissions, but it's not transmitting the video anywhere, just processing it
locally.

## Install

Install JDK and [Clojure CLI Tools](https://clojure.org/guides/getting_started)

    $ npm install # overrides p5.js to current version over cljsjs coordinates
    $ bin/install-bb

## Usage

    $ bin/repl

This launches a [figwheel REPL](https://figwheel.org/) that will update after saving any changes.

https://localhost:9500 shows main app,
http://localhost:9500/figwheel-extra-main/tests shows test results.

### Emacs & Cider

My preferred development environment is Emacs using Cider to communicate with
the ClojureScript browser REPL. This should start using `cider-jack-in-cljs`
presuming Emacs and Cider are already configured.

The [.dir-locals.el](.dir-locals.el) configuration also enables a couple
convenience functions in [shimmers.el](shimmers.el). It binds <kbd>f8</kbd> to
`shimmers-visit-sketch`, which will change the current sketch in the browser to
reflect the current sketch file buffer in Emacs. It also adds
`shimmers-visit-tests` that opens a browser window to the test runner.

## Release

    $ bin/publish.sh

## License

This source repository is released under the AGPL 3.0 free software license.

See [LICENSE](LICENSE) for details.

Copyright © 2020-2022 Charles L.G. Comstock
