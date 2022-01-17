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

## Release

    $ bin/publish.sh

## License

This source repository is released under the AGPL 3.0 free software license.

See [LICENSE](LICENSE) for details.

Copyright © 2020-2022 Charles L.G. Comstock
