#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform vec2 u_resolution;
uniform float u_time;

uniform sampler2D frame;
uniform sampler2D frame10;
uniform sampler2D frame25;

void main() {
  vec2 pos = vTexCoord;
  pos.y = 1.0 - pos.y;

  vec4 color = texture2D(frame, pos);
  vec4 color10 = texture2D(frame10, pos);
  vec4 color25 = texture2D(frame25, pos);

  gl_FragColor = vec4(color.r, color10.g, color25.b, 1.0);
}
