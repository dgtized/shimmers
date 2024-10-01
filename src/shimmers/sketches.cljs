(ns shimmers.sketches
  (:require
   [shimmers.common.sequence :as cs]
   [shimmers.macros.loader :as loader :include-macros true]
   [shimmers.registry :as registry]

   shimmers.sketches.additive-displacement
   shimmers.sketches.alignment-perturbations
   shimmers.sketches.all-the-shapes-in-between
   shimmers.sketches.along-the-curve
   shimmers.sketches.amplification
   shimmers.sketches.angle-of-ascent
   shimmers.sketches.ascendance
   shimmers.sketches.ballistics
   shimmers.sketches.balloon
   shimmers.sketches.barcodes
   shimmers.sketches.beat-table
   shimmers.sketches.blobical
   shimmers.sketches.bold-moves
   shimmers.sketches.box-o-rama
   shimmers.sketches.braid
   shimmers.sketches.breathing-hexes
   shimmers.sketches.brush-strokes
   shimmers.sketches.brush-sweep
   shimmers.sketches.bubbles
   shimmers.sketches.butterfly
   shimmers.sketches.canvas-test
   shimmers.sketches.carrier-wave
   shimmers.sketches.chaikin-demo
   shimmers.sketches.chance-connections
   shimmers.sketches.chaos-cuts
   shimmers.sketches.chemical-attraction
   shimmers.sketches.circle-connections
   shimmers.sketches.circle-hatch
   shimmers.sketches.circle-packing
   shimmers.sketches.circuit-intersections
   shimmers.sketches.circular-repetition
   shimmers.sketches.clothoid-flowers
   shimmers.sketches.clothoids
   shimmers.sketches.clustered-farmlands
   shimmers.sketches.colonial-growth
   shimmers.sketches.color-mapping
   shimmers.sketches.colors
   shimmers.sketches.concentric-chords
   shimmers.sketches.concentric-moire
   shimmers.sketches.concentric-orbits
   shimmers.sketches.constellations
   shimmers.sketches.control-panels
   shimmers.sketches.convex-spiral
   shimmers.sketches.conveyors
   shimmers.sketches.countdown
   shimmers.sketches.cracked-playa
   shimmers.sketches.cube
   shimmers.sketches.cube-rotations
   shimmers.sketches.curvature-of-space
   shimmers.sketches.cut-and-slide
   shimmers.sketches.cutouts
   shimmers.sketches.dance-patterns
   shimmers.sketches.decaying-foundations
   shimmers.sketches.deco-screens
   shimmers.sketches.decomposite
   shimmers.sketches.decorative-tiles
   shimmers.sketches.deeper-squares
   shimmers.sketches.deeply-askew
   shimmers.sketches.deformed-spirals
   shimmers.sketches.delaunator
   shimmers.sketches.delaunay-voronoi
   shimmers.sketches.density-variation
   shimmers.sketches.dependents
   shimmers.sketches.designed-imperfections
   shimmers.sketches.differential-growth
   shimmers.sketches.differential-harmonics
   shimmers.sketches.disassociated-boxes
   shimmers.sketches.dispersion
   shimmers.sketches.displacements-inbetween
   shimmers.sketches.display-tree
   shimmers.sketches.dithering
   shimmers.sketches.divide-by-triangle
   shimmers.sketches.divisible
   shimmers.sketches.dreamcatcher
   shimmers.sketches.elliptics
   shimmers.sketches.emitters
   shimmers.sketches.epicenter-of-impact
   shimmers.sketches.epicycles
   shimmers.sketches.falling-gradients
   shimmers.sketches.fire
   shimmers.sketches.fireworks
   shimmers.sketches.flocking-brushes
   shimmers.sketches.flow-fields
   shimmers.sketches.flow-pairs
   shimmers.sketches.flower-petals
   shimmers.sketches.folding-triangles
   shimmers.sketches.follow-thy-neighbor
   ;; shimmers.sketches.fluid
   shimmers.sketches.future-cities
   shimmers.sketches.gallery-layout
   shimmers.sketches.garden-hose
   shimmers.sketches.geometry-examples
   shimmers.sketches.geometry-interactive
   shimmers.sketches.gl-cube
   shimmers.sketches.glyphs
   shimmers.sketches.gossamer-coils
   shimmers.sketches.graph-traces
   shimmers.sketches.gravity-well
   shimmers.sketches.grid-exclusion
   shimmers.sketches.grid-variations
   shimmers.sketches.hairy-spiral
   shimmers.sketches.harsh-lines
   shimmers.sketches.harmonograph
   shimmers.sketches.hatched-rectangles
   shimmers.sketches.helix
   shimmers.sketches.hexaclock
   shimmers.sketches.hexaflexagon
   shimmers.sketches.hexcursive
   shimmers.sketches.hexflare
   shimmers.sketches.hexpansion
   shimmers.sketches.hyphae
   ;; shimmers.sketches.imperfect-curves
   shimmers.sketches.impressions-of-open-space
   shimmers.sketches.inconsequential-drift
   shimmers.sketches.infographics
   shimmers.sketches.inset-grids
   shimmers.sketches.inset-polygon
   shimmers.sketches.inside-outside
   shimmers.sketches.integer-circles
   shimmers.sketches.intersecting-chords
   shimmers.sketches.intersecting-circle-regions
   shimmers.sketches.interstitial-transitions
   shimmers.sketches.intertwined
   shimmers.sketches.isometric
   shimmers.sketches.iterative-displacement
   shimmers.sketches.k-means
   shimmers.sketches.kaleidoscope
   shimmers.sketches.kd-tree
   shimmers.sketches.kinematic-chain
   shimmers.sketches.kinetic-elliptics
   shimmers.sketches.langton-ant
   shimmers.sketches.lattice-in-steps
   shimmers.sketches.lattice-of-common-chords
   shimmers.sketches.layered-intersections
   shimmers.sketches.ley-lines
   shimmers.sketches.lifecycle-of-shapes
   shimmers.sketches.light-and-dark
   shimmers.sketches.liminal-tension
   shimmers.sketches.logistics-flow
   shimmers.sketches.magnetic-fields
   shimmers.sketches.marching-squares
   shimmers.sketches.mechanism
   shimmers.sketches.memory-allocation
   shimmers.sketches.minimum-spanning-tree
   shimmers.sketches.misplaced-connections
   shimmers.sketches.morse-patterns
   shimmers.sketches.mosaic-deformed
   shimmers.sketches.mosaic-tiling
   shimmers.sketches.motif-shapes
   shimmers.sketches.motion-control
   shimmers.sketches.motion-of-insects
   shimmers.sketches.negative-overlap
   shimmers.sketches.network-effects
   shimmers.sketches.noise-grid
   shimmers.sketches.noisy-shapes
   shimmers.sketches.object-permanence
   shimmers.sketches.offsetting-arcs
   shimmers.sketches.oil-reflections
   shimmers.sketches.ordered
   shimmers.sketches.othello
   shimmers.sketches.overlapping-polygons
   shimmers.sketches.paletteable
   shimmers.sketches.particles
   shimmers.sketches.path-distribution
   shimmers.sketches.path-following
   shimmers.sketches.path-morph
   shimmers.sketches.pawns
   shimmers.sketches.periapsis
   shimmers.sketches.permutations-of-transfiguration
   shimmers.sketches.pendulum-sway
   shimmers.sketches.phase-shifting
   shimmers.sketches.physarum
   shimmers.sketches.pixel-rings
   shimmers.sketches.plaid-arrangements
   shimmers.sketches.point-to-point
   shimmers.sketches.poisson-disc-sampling
   shimmers.sketches.polygon-recomposition
   shimmers.sketches.polygrowth
   shimmers.sketches.polygrowth2
   shimmers.sketches.position-out-of-phase
   shimmers.sketches.precipitation
   shimmers.sketches.probabilistic-automata
   shimmers.sketches.probability-distributions
   shimmers.sketches.prophecies
   shimmers.sketches.pulsing-grid
   shimmers.sketches.punchcard
   shimmers.sketches.quadtrace
   shimmers.sketches.quadtree
   shimmers.sketches.radar
   shimmers.sketches.radial-breaks
   shimmers.sketches.radial-expansion
   shimmers.sketches.radial-mosaic
   shimmers.sketches.radial-symmetries
   shimmers.sketches.radial-wings
   shimmers.sketches.random-point-field
   shimmers.sketches.random-walk
   shimmers.sketches.ray-marching
   shimmers.sketches.reaction-diffusion
   shimmers.sketches.reagent-quil-component
   shimmers.sketches.reflections
   shimmers.sketches.regular-tilings
   shimmers.sketches.remaining
   shimmers.sketches.ring
   shimmers.sketches.ring-impressions
   shimmers.sketches.ripples
   shimmers.sketches.rolling-shapes
   shimmers.sketches.rose
   shimmers.sketches.rtree
   shimmers.sketches.s-chartarum
   shimmers.sketches.sand-strokes
   shimmers.sketches.scintillation
   shimmers.sketches.sea-and-sky
   shimmers.sketches.sediment
   shimmers.sketches.shapes-and-patterns
   shimmers.sketches.shattered
   shimmers.sketches.six-of-one-half-dozen-of-the-other
   shimmers.sketches.slashes
   shimmers.sketches.slither
   shimmers.sketches.slow-zone
   shimmers.sketches.snake
   shimmers.sketches.solar-transit
   shimmers.sketches.space-colonization
   shimmers.sketches.space-filling-curves
   shimmers.sketches.spaces-divided
   shimmers.sketches.sphere
   shimmers.sketches.spiderwebs
   shimmers.sketches.spin-doctor
   shimmers.sketches.spiral-approach
   shimmers.sketches.spiral-distance
   shimmers.sketches.spiral-pack
   shimmers.sketches.splitting-polygons
   shimmers.sketches.square-packing
   shimmers.sketches.squiggle-line
   shimmers.sketches.stem-and-leaf
   shimmers.sketches.stretchy-curves
   shimmers.sketches.stretchy-lines
   shimmers.sketches.string-lights
   shimmers.sketches.subliminal
   shimmers.sketches.substrate
   shimmers.sketches.sunflower
   shimmers.sketches.sunflower-points
   shimmers.sketches.superposition
   shimmers.sketches.superposition-mirrored
   shimmers.sketches.terrain-grid
   shimmers.sketches.texas-fields
   shimmers.sketches.the-journey-between
   shimmers.sketches.three-lines
   shimmers.sketches.tilt
   shimmers.sketches.traffic-intersection
   shimmers.sketches.transitory-tension
   shimmers.sketches.tree-rings
   shimmers.sketches.triangle-flow
   shimmers.sketches.triangle-gradient
   shimmers.sketches.triangle-intersections
   shimmers.sketches.triangle-mosaic
   shimmers.sketches.triangulating-subdivisions
   shimmers.sketches.trigonometry-boxes
   shimmers.sketches.tunnel-flight
   shimmers.sketches.twelve-clocks
   shimmers.sketches.typography
   shimmers.sketches.under-the-surface
   shimmers.sketches.undulating-figures
   shimmers.sketches.uniform-distribution
   shimmers.sketches.unit-circle
   shimmers.sketches.unraveling
   shimmers.sketches.unwinding
   shimmers.sketches.vanishing-points
   shimmers.sketches.velocity-fields
   shimmers.sketches.verlet-brushes
   shimmers.sketches.vertex-curves
   shimmers.sketches.video-delay-shader
   shimmers.sketches.video-shader
   shimmers.sketches.voronoi-after-effect
   shimmers.sketches.wave-function-collapse
   shimmers.sketches.wavetracker
   shimmers.sketches.waystation
   shimmers.sketches.wibble-wobble
   shimmers.sketches.window-glimpses
   shimmers.sketches.wood-grain
   shimmers.sketches.woven
   shimmers.sketches.yin-yang
   shimmers.sketches.zigzag
   shimmers.sketches.zoetropic))

(defn date-parse [s]
  (.parse js/Date s))

(defn- db []
  (->> @registry/sketches
       vals
       (map (fn [s] (update s :created-at date-parse)))))

(defn all []
  (sort-by (comp name :sketch-id) (db)))

(defn by-name [sketch-name]
  (let [sketch-id (keyword sketch-name)]
    (cs/find-first #(= sketch-id (:sketch-id %)) (db))))

;; Only allows one sketch per namespace
(defn by-ns [ns-name]
  (let [sketch-ns (symbol ns-name)]
    (cs/find-first #(= sketch-ns (:ns %)) (db))))

(defn known-names []
  (map (comp name :sketch-id) (all)))

(comment (loader/all-sketches))
