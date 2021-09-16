#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform sampler2D image;
uniform int mode;

void main() {
  vec2 pos = vTexCoord.xy;
  vec4 col = texture2D(image, pos);
  float grey = 0.0;
  if(mode == 0) {
    grey = abs(col.y-col.x);
  } else if (mode == 1) {
    grey = 1.0-abs(col.y-col.x);
  } else if (mode == 2) {
    grey = col.x;
  } else if (mode == 3) {
    grey = 1.0-col.x;
  } else if (mode == 4) {
    grey = col.y;
  } else {
    grey = 1.0-col.y;
  }

  gl_FragColor = vec4(grey,grey,grey,1.0);
}
