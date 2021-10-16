#ifdef GL_ES
precision mediump float;
#endif

#ifndef PI
#define PI 3.141592653589793
#endif

varying vec2 vTexCoord;

uniform sampler2D image;
uniform int mode;
uniform bool invert;

// From https://stackoverflow.com/a/17897228/34450
// All components are in the range [0…1], including hue.
vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

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
  } else if (mode == 3) {
    if(col.y >= col.x) {
      grey = 1.0;
    } else {
      grey = 0.0;
    }
  }

  if(invert) {
    grey = 1.0 - grey;
  }

  if(mode == 4) {
    float hue = 1.0 - ((atan(col.y, col.x) + PI) / 2.0 * PI);
    float dist = 1.0 - distance(col.xy, vec2(0.0,0.0));
    gl_FragColor = vec4(hsv2rgb(vec3(hue, dist, 1.0)), 1.0);
  } else {
    gl_FragColor = vec4(grey,grey,grey,1.0);
  }
}
