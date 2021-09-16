#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform sampler2D image;
uniform int mode;
uniform bool invert;

void main() {
  vec2 pos = vTexCoord.xy;
  vec4 col = texture2D(image, pos);
  float grey = 0.0;
  if(mode == 0) {
    grey = abs(col.y-col.x);
  } else if (mode == 1) {
    grey = col.x;
  } else if (mode == 2) {
    grey = col.y;
  }

  if(invert) {
    grey = 1.0 - grey;
  }

  gl_FragColor = vec4(grey,grey,grey,1.0);
}
