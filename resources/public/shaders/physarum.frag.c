#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform vec2 u_resolution;
uniform float decay;
uniform sampler2D trail;

void kernel(inout vec4 n[9], sampler2D tex, vec2 pos, float w, float h) {
  n[0] = texture2D(tex, pos + vec2(-w,-h));
  n[1] = texture2D(tex, pos + vec2(0.0, -h));
  n[2] = texture2D(tex, pos + vec2(w, -h));
  n[3] = texture2D(tex, pos + vec2(-w, 0.0));
  n[4] = texture2D(tex, pos);
  n[5] = texture2D(tex, pos + vec2(w, 0.0));
  n[6] = texture2D(tex, pos + vec2(-w, h));
  n[7] = texture2D(tex, pos + vec2(0.0, h));
  n[8] = texture2D(tex, pos + vec2(w, h));
}

void main() {
  vec4 n[9];

  vec2 pos = vTexCoord.xy;

  kernel(n, trail, pos, 1.0/u_resolution.x, 1.0/u_resolution.y);

  vec4 color = vec4(0.0);
  for(int i = 0; i < 9; i++) {
    color = color + n[i];
  }

  gl_FragColor = vec4(color.xyz*decay/9.0,decay);
}
