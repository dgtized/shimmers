# shimmers

Shimmering quil sketches; a few particle simulations, video processing
experiments, randomly generated L-systems, 3d transformations, and cellular
automata.

https://dgtized.github.io/shimmers

Note: Some sketches require camera input, so it will prompt for video
permissions, but it's not transmitting the video anywhere, just processing it
locally.

## Install

    $ npm install # overrides p5.js to current version over cljsjs coordinates

## Usage

    $ bin/repl

https://localhost:9500 shows main app,
http://localhost:9500/figwheel-extra-main/tests shows test results.

## Release

    $ clojure -m figwheel.main -b release

## License

Copyright © 2020 Charles L.G. Comstock
