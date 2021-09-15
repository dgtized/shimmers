#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform sampler2D image;

void main() {
  vec2 pos = vTexCoord.xy;
  vec4 current = texture2D(image, pos);
  float grey = current.y;

  gl_FragColor = vec4(grey,grey,grey,1.0);
}
