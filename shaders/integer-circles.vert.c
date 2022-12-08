attribute vec3 aPosition;

void main() {
  vec4 p = vec4(aPosition,1.0);
  p.xy = p.xy * 2.0 - 1.0;
  gl_Position = p;
}
