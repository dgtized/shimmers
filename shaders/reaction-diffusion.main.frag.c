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
vec2 laplacian(sampler2D tex, vec2 pos, vec2 texel) {
  vec2 ab = vec2(0.0,0.0);

  ab += texture2D(tex, pos + vec2(-1.0,-1.0)*texel).xy * 0.05;
  ab += texture2D(tex, pos + vec2( 0.0,-1.0)*texel).xy * 0.2;
  ab += texture2D(tex, pos + vec2( 1.0,-1.0)*texel).xy * 0.05;
  ab += texture2D(tex, pos + vec2(-1.0, 0.0)*texel).xy * 0.2;
  ab += texture2D(tex, pos + vec2( 0.0, 0.0)*texel).xy * -1.0;
  ab += texture2D(tex, pos + vec2( 1.0, 0.0)*texel).xy * 0.2;
  ab += texture2D(tex, pos + vec2(-1.0, 1.0)*texel).xy * 0.05;
  ab += texture2D(tex, pos + vec2( 0.0, 1.0)*texel).xy * 0.2;
  ab += texture2D(tex, pos + vec2( 1.0, 1.0)*texel).xy * 0.05;

  return ab;
}

vec2 laplacianCartesian(sampler2D tex, vec2 pos, vec2 texel) {
  vec2 ab = vec2(0.0,0.0);

  ab += texture2D(tex, pos + vec2( 0.0,-1.0)*texel).xy;
  ab += texture2D(tex, pos + vec2(-1.0, 0.0)*texel).xy;
  ab += texture2D(tex, pos + vec2( 1.0, 0.0)*texel).xy;
  ab += texture2D(tex, pos + vec2( 0.0, 1.0)*texel).xy;
  ab += texture2D(tex, pos + vec2( 0.0, 0.0)*texel).xy * -4.0;

  return ab;
}

// Why is this drifting from right to left?
void main() {
  vec2 pos = vTexCoord.xy;
  vec2 texel = 1.0 / resolution;

  vec4 current = texture2D(concentrations, pos);
  float a = current.x;
  float b = current.y;
  vec2 lp = laplacian(concentrations, pos, texel);

  float da = diffusionA*lp.x - a*b*b + feed*(1.0-a);
  float db = diffusionB*lp.y + a*b*b - (kill + feed)*b;

  vec2 ab = current.xy + vec2(da, db) * deltaT;

  gl_FragColor = vec4(ab.xy,0.0,1.0);
}
