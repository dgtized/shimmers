#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform vec2 resolution;
uniform sampler2D concentrations;

uniform float diffusionA;
uniform float diffusionB;
uniform float feed;
uniform float kill;
uniform float deltaT;

// Translated from https://ciphrd.com/2019/08/24/reaction-diffusion-on-shader/
vec2 laplacian(sampler2D tex, vec2 pos, vec2 texelSize) {
  vec2 ab = vec2(0.0,0.0);

  ab += texture2D(tex, pos + vec2(-1,-1)*texelSize).xy * 0.05;
  ab += texture2D(tex, pos + vec2( 0,-1)*texelSize).xy * 0.2;
  ab += texture2D(tex, pos + vec2( 1,-1)*texelSize).xy * 0.05;
  ab += texture2D(tex, pos + vec2(-1, 0)*texelSize).xy * 0.2;
  ab += texture2D(tex, pos + vec2( 0, 0)*texelSize).xy * -1.0;
  ab += texture2D(tex, pos + vec2( 1, 0)*texelSize).xy * 0.2;
  ab += texture2D(tex, pos + vec2(-1, 1)*texelSize).xy * 0.05;
  ab += texture2D(tex, pos + vec2( 0, 1)*texelSize).xy * 0.2;
  ab += texture2D(tex, pos + vec2( 1, 1)*texelSize).xy * 0.05;

  return ab;
}

// Why is this drifting from right to left?
void main() {
  vec2 pos = vTexCoord.xy;
  vec2 texelSize = 1.0 / resolution;

  vec4 current = texture2D(concentrations, pos);
  float a = current.x;
  float b = current.y;
  vec2 lp = laplacian(concentrations, pos, texelSize);

  float a2 = clamp(a + (diffusionA*lp.x - a*b*b + feed*(1.0-a)) * deltaT, 0.0, 1.0);
  float b2 = clamp(b + (diffusionB*lp.y + a*b*b - (kill + feed)*b) * deltaT, 0.0, 1.0);

  gl_FragColor = vec4(a2,b2,0.0,1.0);
}
