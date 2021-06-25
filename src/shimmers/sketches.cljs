(ns shimmers.sketches
  (:require [shimmers.common.sequence :as cs]
            [shimmers.macros.loader :as loader :include-macros true]
            [shimmers.sketches.ascendance :as ascendance]
            [shimmers.sketches.braid :as braid]
            [shimmers.sketches.brush-sweep :as brush-sweep]
            [shimmers.sketches.breathing-hexes :as breathing-hexes]
            [shimmers.sketches.bubbles :as bubbles]
            [shimmers.sketches.butterfly :as butterfly]
            [shimmers.sketches.circle-packing :as circle-packing]
            [shimmers.sketches.clustered-farmlands :as clustered-farmlands]
            [shimmers.sketches.colonial-growth :as colonial-growth]
            [shimmers.sketches.colors :as colors]
            [shimmers.sketches.convex-spiral :as convex-spiral]
            [shimmers.sketches.cube :as cube]
            [shimmers.sketches.delaunay-voronoi :as delaunay-voronoi]
            [shimmers.sketches.decaying-foundations :as decaying-foundations]
            [shimmers.sketches.disassociated-boxes :as disassociated-boxes]
            [shimmers.sketches.dispersion :as dispersion]
            [shimmers.sketches.dithering :as dithering]
            [shimmers.sketches.emitters :as emitters]
            [shimmers.sketches.falling-gradients :as falling-gradients]
            [shimmers.sketches.fire :as fire]
            [shimmers.sketches.fireworks :as fireworks]
            [shimmers.sketches.flocking-brushes :as flocking-brushes]
            [shimmers.sketches.flow-fields :as flow-fields]
            [shimmers.sketches.folding-triangles :as folding-triangles]
            ;; [shimmers.sketches.fluid :as fluid]
            [shimmers.sketches.gossamer-coils :as gossamer-coils]
            [shimmers.sketches.gravity-well :as gravity-well]
            [shimmers.sketches.harsh-lines :as harsh-lines]
            [shimmers.sketches.hexaclock :as hexaclock]
            [shimmers.sketches.hexcursive :as hexcursive]
            [shimmers.sketches.inconsequential-drift :as inconsequential-drift]
            [shimmers.sketches.impressions-of-open-space :as impressions-of-open-space]
            [shimmers.sketches.k-means :as k-means]
            [shimmers.sketches.kd-tree :as kd-tree]
            [shimmers.sketches.kinematic-chain :as kinematic-chain]
            [shimmers.sketches.langton-ant :as langton-ant]
            [shimmers.sketches.lattice-in-steps :as lattice-in-steps]
            [shimmers.sketches.lattice-of-common-chords :as lattice-of-common-chords]
            [shimmers.sketches.mechanism :as mechanism]
            [shimmers.sketches.minimum-spanning-tree :as minimum-spanning-tree]
            [shimmers.sketches.mosaic-tiling :as mosaic-tiling]
            [shimmers.sketches.noise-grid :as noise-grid]
            [shimmers.sketches.noisy-shapes :as noisy-shapes]
            [shimmers.sketches.object-permanence :as object-permanence]
            [shimmers.sketches.particles :as particles]
            [shimmers.sketches.path-distribution :as path-distribution]
            [shimmers.sketches.permutations-of-transfiguration :as permutations-of-transfiguration]
            [shimmers.sketches.point-to-point :as point-to-point]
            [shimmers.sketches.polygrowth :as polygrowth]
            [shimmers.sketches.polygrowth2 :as polygrowth2]
            [shimmers.sketches.precipitation :as precipitation]
            [shimmers.sketches.probabilistic-automata :as probabilistic-automata]
            [shimmers.sketches.radar :as radar]
            [shimmers.sketches.radial-mosaic :as radial-mosaic]
            [shimmers.sketches.random-walk :as random-walk]
            [shimmers.sketches.ray-marching :as ray-marching]
            [shimmers.sketches.ring :as ring]
            [shimmers.sketches.ripples :as ripples]
            [shimmers.sketches.rose :as rose]
            [shimmers.sketches.sand-strokes :as sand-strokes]
            [shimmers.sketches.scintillation :as scintillation]
            [shimmers.sketches.sediment :as sediment]
            [shimmers.sketches.six-of-one-half-dozen-of-the-other :as six-of-one-half-dozen-of-the-other]
            [shimmers.sketches.space-colonization :as space-colonization]
            [shimmers.sketches.sphere :as sphere]
            [shimmers.sketches.substrate :as substrate]
            [shimmers.sketches.superposition :as superposition]
            [shimmers.sketches.tilt :as tilt]
            [shimmers.sketches.triangulating-subdivisions :as triangulating-subdivisions]
            [shimmers.sketches.tunnel-flight :as tunnel-flight]
            [shimmers.sketches.typography :as typography]
            [shimmers.sketches.undulating-figures :as undulating-figures]
            [shimmers.sketches.uniform-distribution :as uniform-distribution]
            [shimmers.sketches.verlet-brushes :as verlet-brushes]
            [shimmers.sketches.video-shader :as video-shader]
            [shimmers.sketches.yin-yang :as yin-yang]
            [shimmers.sketches.zigzag :as zigzag]
            [shimmers.sketches.zoetropic :as zoetropic]))

(defn- db
  []
  (loader/sketches-with-meta
   [ascendance/ascendance
    braid/braid
    breathing-hexes/breathing-hexes
    brush-sweep/brush-sweep
    bubbles/bubbles
    butterfly/run-sketch
    circle-packing/run-sketch
    clustered-farmlands/run-sketch
    colonial-growth/run-sketch
    colors/run-sketch
    convex-spiral/run-sketch
    cube/run-sketch
    delaunay-voronoi/run-sketch
    decaying-foundations/run-sketch
    disassociated-boxes/run-sketch
    dispersion/run-sketch
    dithering/run-sketch
    emitters/run-sketch
    falling-gradients/run-sketch
    fire/run-sketch
    fireworks/run-sketch
    flocking-brushes/run-sketch
    flow-fields/run-sketch
    folding-triangles/run-sketch
    ;; fluid/run-sketch
    gossamer-coils/run-sketch
    gravity-well/run-sketch
    hexaclock/run-sketch
    hexcursive/run-sketch
    harsh-lines/run-sketch
    inconsequential-drift/run-sketch
    impressions-of-open-space/run-sketch
    k-means/run-sketch
    kd-tree/run-sketch
    kinematic-chain/run-sketch
    langton-ant/run-sketch
    lattice-in-steps/run-sketch
    lattice-of-common-chords/run-sketch
    mechanism/run-sketch
    minimum-spanning-tree/run-sketch
    mosaic-tiling/run-sketch
    noise-grid/run-sketch
    noisy-shapes/run-sketch
    object-permanence/run-sketch
    particles/run-sketch
    path-distribution/run-sketch
    permutations-of-transfiguration/run-sketch
    point-to-point/run-sketch
    polygrowth/polygrowth
    polygrowth2/polygrowth2
    precipitation/run-sketch
    probabilistic-automata/run-sketch
    radar/run-sketch
    radial-mosaic/run-sketch
    random-walk/run-sketch
    ray-marching/run-sketch
    ring/run-sketch
    ripples/run-sketch
    rose/run-sketch
    sand-strokes/run-sketch
    scintillation/run-sketch
    sediment/run-sketch
    six-of-one-half-dozen-of-the-other/run-sketch
    space-colonization/run-sketch
    sphere/run-sketch
    substrate/run-sketch
    superposition/superposition
    tilt/run-sketch
    triangulating-subdivisions/run-sketch
    tunnel-flight/run-sketch
    typography/run-sketch
    undulating-figures/run-sketch
    uniform-distribution/run-sketch
    verlet-brushes/run-sketch
    video-shader/run-sketch
    yin-yang/run-sketch
    zigzag/run-sketch
    zoetropic/run-sketch]))

(defn all []
  (sort-by (comp name :id) (db)))

(defn by-name [sketch-name]
  (let [sketch-id (keyword sketch-name)]
    (cs/find-first #(= sketch-id (:id %)) (db))))

(comment (loader/all-sketches))
