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

void edge_detection() {
  vec2 pos = vTexCoord;
  pos.y = 1.0 - pos.y;

  vec4 color = vec4(0.0);

  vec4 tex = texture2D(videoTexture, pos);
  gl_FragColor = color + tex;
}

void main() {
  if(u_mode == 0) {
    specular_mouse();
  } else if(u_mode == 1) {
    edge_detection();
  }
}
