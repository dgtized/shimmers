#ifdef GL_ES
precision mediump float;
#endif

uniform vec2 u_resolution;
uniform float u_time;

int max_iterations = 4096;

// derived from https://www.shadertoy.com/view/4lSGRG
int circle_iterations(float x0, float y0, float d, float e) {
  float x = x0;
  float y = y0;

  for(int iter = 0; iter < 4096; iter++) {
    x = x - floor(d*y);
    y = y + floor(e*x);

    if((floor(x) == floor(x0)) && floor(y) == floor(y0)) {
      return iter;
    }
  }
  return max_iterations;
}

void main() {
  vec2 st = floor(gl_FragCoord.xy - u_resolution);
  float d=1.0;
  float e=4.0*pow(sin(3.14159/9.0),2.0);
  float iterations = float(circle_iterations(st.x, st.y, d, e));
  float grey = log(iterations)/log(float(max_iterations));
  gl_FragColor = vec4(grey,grey,grey,1.0);
}
