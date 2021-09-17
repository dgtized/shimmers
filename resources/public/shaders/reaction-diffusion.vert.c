attribute vec3 aPosition;
attribute vec2 aTexCoord;

varying vec2 vTexCoord;

void main() {
  vTexCoord = aTexCoord;
  // ensure same inverted y to keep it from flickering
  vTexCoord.y = 1.0-vTexCoord.y;
  vec4 p = vec4(aPosition,1.0);
  p.xy = p.xy * 2.0 - 1.0;
  gl_Position = p;
}
