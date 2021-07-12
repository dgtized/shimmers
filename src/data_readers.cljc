;; This is a convenience so that #vec2, #vec3 parse. However it does throw
;; warnings on `Use of undeclared Var cljs.user/vec2` if they are not *also*
;; declared in `cljs.user`.

;; TODO: investigate warnings about:
;; [Figwheel:SEVERE] java.lang.RuntimeException: Can't embed object in code, maybe print-dup not defined: [D@5b94c677
;; when used at runtime.
{vec2 thi.ng.geom.vector/vec2
 vec3 thi.ng.geom.vector/vec3}
