(ns shimmers.sketches
  (:require [shimmers.common.sequence :as cs]
            [shimmers.macros.loader :as loader :include-macros true]
            [shimmers.registry :as registry]

            shimmers.sketches.additive-displacement
            shimmers.sketches.ascendance
            shimmers.sketches.box-o-rama
            shimmers.sketches.braid
            shimmers.sketches.brush-sweep
            shimmers.sketches.breathing-hexes
            shimmers.sketches.bubbles
            shimmers.sketches.butterfly
            shimmers.sketches.canvas-test
            shimmers.sketches.chaikin-demo
            shimmers.sketches.circle-hatch
            shimmers.sketches.circle-packing
            shimmers.sketches.circuit-intersections
            shimmers.sketches.clothoids
            shimmers.sketches.clothoid-flowers
            shimmers.sketches.clustered-farmlands
            shimmers.sketches.colonial-growth
            shimmers.sketches.colors
            shimmers.sketches.concentric-moire
            shimmers.sketches.convex-spiral
            shimmers.sketches.cube
            shimmers.sketches.cube-rotations
            shimmers.sketches.curvature-of-space
            shimmers.sketches.decaying-foundations
            shimmers.sketches.deeper-squares
            shimmers.sketches.deformed-spirals
            shimmers.sketches.delaunay-voronoi
            shimmers.sketches.disassociated-boxes
            shimmers.sketches.dispersion
            shimmers.sketches.displacements-inbetween
            shimmers.sketches.dithering
            ;; shimmers.sketches.dreamcatcher
            shimmers.sketches.emitters
            shimmers.sketches.falling-gradients
            shimmers.sketches.fire
            shimmers.sketches.fireworks
            shimmers.sketches.flocking-brushes
            shimmers.sketches.flow-fields
            shimmers.sketches.folding-triangles
            ;; shimmers.sketches.fluid
            shimmers.sketches.garden-hose
            shimmers.sketches.gl-cube
            shimmers.sketches.glyphs
            shimmers.sketches.gossamer-coils
            shimmers.sketches.gravity-well
            shimmers.sketches.grid-variations
            shimmers.sketches.harsh-lines
            shimmers.sketches.hatched-rectangles
            shimmers.sketches.hexaclock
            shimmers.sketches.hexcursive
            shimmers.sketches.hexflare
            shimmers.sketches.impressions-of-open-space
            shimmers.sketches.inconsequential-drift
            shimmers.sketches.integer-circles
            shimmers.sketches.interstitial-transitions
            shimmers.sketches.intertwined
            shimmers.sketches.isometric
            shimmers.sketches.iterative-displacement
            shimmers.sketches.kd-tree
            shimmers.sketches.kinematic-chain
            shimmers.sketches.k-means
            shimmers.sketches.langton-ant
            shimmers.sketches.lattice-in-steps
            shimmers.sketches.lattice-of-common-chords
            shimmers.sketches.lifecycle-of-shapes
            shimmers.sketches.marching-squares
            shimmers.sketches.mechanism
            shimmers.sketches.memory-allocation
            shimmers.sketches.minimum-spanning-tree
            shimmers.sketches.morse-patterns
            shimmers.sketches.mosaic-tiling
            shimmers.sketches.motif-shapes
            shimmers.sketches.motion-of-insects
            shimmers.sketches.network-effects
            shimmers.sketches.noise-grid
            shimmers.sketches.noisy-shapes
            shimmers.sketches.object-permanence
            shimmers.sketches.offsetting-arcs
            shimmers.sketches.oil-reflections
            shimmers.sketches.overlapping-polygons
            shimmers.sketches.particles
            shimmers.sketches.path-distribution
            shimmers.sketches.path-following
            shimmers.sketches.path-morph
            shimmers.sketches.periapsis
            shimmers.sketches.permutations-of-transfiguration
            shimmers.sketches.physarum
            shimmers.sketches.plaid-arrangements
            shimmers.sketches.point-to-point
            shimmers.sketches.poisson-disc-sampling
            shimmers.sketches.polygon-recomposition
            shimmers.sketches.polygrowth
            shimmers.sketches.polygrowth2
            shimmers.sketches.precipitation
            shimmers.sketches.probabilistic-automata
            shimmers.sketches.pulsing-grid
            shimmers.sketches.quadtree
            shimmers.sketches.radar
            shimmers.sketches.radial-mosaic
            shimmers.sketches.radial-wings
            shimmers.sketches.random-walk
            shimmers.sketches.ray-marching
            shimmers.sketches.reaction-diffusion
            shimmers.sketches.ring
            shimmers.sketches.ripples
            shimmers.sketches.rolling-shapes
            shimmers.sketches.rose
            shimmers.sketches.rtree
            shimmers.sketches.sand-strokes
            shimmers.sketches.scintillation
            shimmers.sketches.sea-and-sky
            shimmers.sketches.sediment
            shimmers.sketches.shattered
            shimmers.sketches.six-of-one-half-dozen-of-the-other
            shimmers.sketches.slashes
            shimmers.sketches.slow-zone
            shimmers.sketches.space-colonization
            shimmers.sketches.spaces-divided
            shimmers.sketches.space-filling-curves
            shimmers.sketches.sphere
            shimmers.sketches.spiral-distance
            shimmers.sketches.square-packing
            shimmers.sketches.squiggle-line
            shimmers.sketches.stem-and-leaf
            shimmers.sketches.substrate
            shimmers.sketches.string-lights
            shimmers.sketches.superposition
            shimmers.sketches.tilt
            shimmers.sketches.traffic-intersection
            shimmers.sketches.triangle-gradient
            shimmers.sketches.triangulating-subdivisions
            shimmers.sketches.tunnel-flight
            shimmers.sketches.typography
            shimmers.sketches.undulating-figures
            shimmers.sketches.uniform-distribution
            shimmers.sketches.unit-circle
            shimmers.sketches.verlet-brushes
            shimmers.sketches.video-shader
            shimmers.sketches.yin-yang
            shimmers.sketches.wood-grain
            shimmers.sketches.zigzag
            shimmers.sketches.zoetropic))

(defn date-parse [s]
  (.parse js/Date s))

(defn- db []
  (->> @registry/sketches
       vals
       (map (fn [s] (update s :created-at date-parse)))))

(defn all []
  (sort-by (comp name :id) (db)))

(defn by-name [sketch-name]
  (let [sketch-id (keyword sketch-name)]
    (cs/find-first #(= sketch-id (:id %)) (db))))

;; Only allows one sketch per namespace
(defn by-ns [ns-name]
  (let [sketch-ns (symbol ns-name)]
    (cs/find-first #(= sketch-ns (:ns %)) (db))))

(defn known-names []
  (map (comp name :id) (all)))

(comment (loader/all-sketches))
