#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;
uniform int u_mode;

uniform sampler2D videoTexture;

void specular_mouse() {
  vec2 mp = u_mouse/u_resolution;
  vec2 st = gl_FragCoord.xy/u_resolution;
  float pct = 0.0;
  pct = distance(st-(mp-vec2(0.2)), vec2(1.5));

  vec2 pos = vTexCoord;
  pos.y = 1.0 - pos.y;

  vec4 tex = texture2D(videoTexture, pos);
  vec4 color = vec4(tex.rgb, 1.0);
  vec4 blend = vec4(st.x, st.y, (st.x + st.y), abs(sin(u_time/1.2)));
  gl_FragColor = mix(color, blend, clamp(1.2-pct, -0.3, 0.5));
}

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

void edge_detection() {
  vec2 pos = vTexCoord;
  pos.y = 1.0 - pos.y;

  vec4 n[9];
  kernel(n, videoTexture, pos, 1.0/u_resolution.x, 1.0/u_resolution.y);
  vec4 edge = 8.0*n[4] - n[0] - n[1] - n[2] - n[3] - n[5] - n[6] - n[7] - n[8];

  gl_FragColor = vec4(edge.xyz,1.0);
}

void main() {
  if(u_mode == 0) {
    specular_mouse();
  } else if(u_mode == 1) {
    edge_detection();
  }
}
