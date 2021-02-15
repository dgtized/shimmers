#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

uniform sampler2D videoTexture;

void main() {
  vec2 pos = vTexCoord;
  pos.y = 1.0 - pos.y;

  vec4 tex = texture2D(videoTexture, pos);
  vec4 blend = vec4(tex.bgr, 1.0);
  gl_FragColor = blend;
}
