#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform vec2 resolution;
uniform sampler2D trail;
uniform float decay;

vec4 blur(sampler2D tex, vec2 pos, vec2 texel) {
  vec4 color = vec4(0.0);
  color += texture2D(tex, pos + vec2(-1.0,-1.0) * texel) * 1.0/16.0;
  color += texture2D(tex, pos + vec2(0.0, -1.0) * texel) * 2.0/16.0;
  color += texture2D(tex, pos + vec2(1.0, -1.0) * texel) * 1.0/16.0;
  color += texture2D(tex, pos + vec2(-1.0, 0.0) * texel) * 2.0/16.0;
  color += texture2D(tex, pos + vec2(0.0,0.0) * texel) * 4.0/16.0;
  color += texture2D(tex, pos + vec2(1.0, 0.0) * texel) * 2.0/16.0;
  color += texture2D(tex, pos + vec2(-1.0, 1.0) * texel) * 1.0/16.0;
  color += texture2D(tex, pos + vec2(0.0, 1.0) * texel) * 2.0/16.0;
  color += texture2D(tex, pos + vec2(1.0, 1.0) * texel) * 1.0/16.0;

  return color;
}

void main() {
  vec2 pos = vTexCoord.xy;
  pos.y = 1.0 - pos.y;

  vec2 texelSize = vec2(1.0/resolution.x, 1.0/resolution.y);

  float v = blur(trail, pos, texelSize).x * decay;

  gl_FragColor = vec4(v,v,v,1.0);
}
