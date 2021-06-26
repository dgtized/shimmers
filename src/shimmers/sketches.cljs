(ns shimmers.sketches
  (:require [shimmers.common.sequence :as cs]
            [shimmers.macros.loader :as loader :include-macros true]
            [shimmers.registry :as registry]

            shimmers.sketches.ascendance
            shimmers.sketches.braid
            shimmers.sketches.brush-sweep
            shimmers.sketches.breathing-hexes
            shimmers.sketches.bubbles
            shimmers.sketches.butterfly
            shimmers.sketches.circle-packing
            shimmers.sketches.clustered-farmlands
            shimmers.sketches.colonial-growth
            shimmers.sketches.colors
            shimmers.sketches.convex-spiral
            shimmers.sketches.cube
            shimmers.sketches.delaunay-voronoi
            shimmers.sketches.decaying-foundations
            shimmers.sketches.disassociated-boxes
            shimmers.sketches.dispersion
            shimmers.sketches.dithering
            shimmers.sketches.emitters
            shimmers.sketches.falling-gradients
            shimmers.sketches.fire
            shimmers.sketches.fireworks
            shimmers.sketches.flocking-brushes
            shimmers.sketches.flow-fields
            shimmers.sketches.folding-triangles
            ;; shimmers.sketches.fluid
            shimmers.sketches.gossamer-coils
            shimmers.sketches.gravity-well
            shimmers.sketches.harsh-lines
            shimmers.sketches.hexaclock
            shimmers.sketches.hexcursive
            shimmers.sketches.inconsequential-drift
            shimmers.sketches.impressions-of-open-space
            shimmers.sketches.interstitial
            shimmers.sketches.k-means
            shimmers.sketches.kd-tree
            shimmers.sketches.kinematic-chain
            shimmers.sketches.langton-ant
            shimmers.sketches.lattice-in-steps
            shimmers.sketches.lattice-of-common-chords
            shimmers.sketches.mechanism
            shimmers.sketches.minimum-spanning-tree
            shimmers.sketches.mosaic-tiling
            shimmers.sketches.noise-grid
            shimmers.sketches.noisy-shapes
            shimmers.sketches.object-permanence
            shimmers.sketches.particles
            shimmers.sketches.path-distribution
            shimmers.sketches.permutations-of-transfiguration
            shimmers.sketches.point-to-point
            shimmers.sketches.polygrowth
            shimmers.sketches.polygrowth2
            shimmers.sketches.precipitation
            shimmers.sketches.probabilistic-automata
            shimmers.sketches.radar
            shimmers.sketches.radial-mosaic
            shimmers.sketches.random-walk
            shimmers.sketches.ray-marching
            shimmers.sketches.ring
            shimmers.sketches.ripples
            shimmers.sketches.rose
            shimmers.sketches.sand-strokes
            shimmers.sketches.scintillation
            shimmers.sketches.sediment
            shimmers.sketches.six-of-one-half-dozen-of-the-other
            shimmers.sketches.space-colonization
            shimmers.sketches.sphere
            shimmers.sketches.substrate
            shimmers.sketches.superposition
            shimmers.sketches.tilt
            shimmers.sketches.triangulating-subdivisions
            shimmers.sketches.tunnel-flight
            shimmers.sketches.typography
            shimmers.sketches.undulating-figures
            shimmers.sketches.uniform-distribution
            shimmers.sketches.verlet-brushes
            shimmers.sketches.video-shader
            shimmers.sketches.yin-yang
            shimmers.sketches.zigzag
            shimmers.sketches.zoetropic ))

(defn- db []
  (vals @registry/sketches))

(defn all []
  (sort-by (comp name :id) (db)))

(defn by-name [sketch-name]
  (let [sketch-id (keyword sketch-name)]
    (cs/find-first #(= sketch-id (:id %)) (db))))

(comment (loader/all-sketches))
