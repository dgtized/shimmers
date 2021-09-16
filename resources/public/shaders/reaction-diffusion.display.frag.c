#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform sampler2D image;

void main() {
  vec2 pos = vTexCoord.xy;
  vec4 col = texture2D(image, pos);
  float grey = abs(col.y-col.x);

  gl_FragColor = vec4(grey,grey,grey,1.0);
}
