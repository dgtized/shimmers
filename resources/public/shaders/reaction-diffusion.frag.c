#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform float diffusionA;
uniform float diffusionB;
uniform float feed;
uniform float kill;
uniform float deltaT;
uniform sampler2D concentrations;
uniform vec2 texelSize;

// Translated from https://ciphrd.com/2019/08/24/reaction-diffusion-on-shader/
vec3 laplacian(sampler2D tex, vec2 pos) {
  vec3 ab = vec3(0,0,0);

  ab += texture2D(tex, pos + vec2(-1.0,-1.0)*texelSize).rgb * 0.05;
  ab += texture2D(tex, pos + vec2(-0.0,-1.0)*texelSize).rgb * 0.2;
  ab += texture2D(tex, pos + vec2( 1.0,-1.0)*texelSize).rgb * 0.05;
  ab += texture2D(tex, pos + vec2(-1.0, 0.0)*texelSize).rgb * 0.2;
  ab += texture2D(tex, pos + vec2( 0.0, 0.0)*texelSize).rgb * -1.0;
  ab += texture2D(tex, pos + vec2( 1.0, 0.0)*texelSize).rgb * 0.2;
  ab += texture2D(tex, pos + vec2(-1.0, 1.0)*texelSize).rgb * 0.05;
  ab += texture2D(tex, pos + vec2( 0.0, 1.0)*texelSize).rgb * 0.2;
  ab += texture2D(tex, pos + vec2( 1.0, 1.0)*texelSize).rgb * 0.05;

  return ab;
}

void main() {
  vec2 pos = vTexCoord.xy;

  vec4 current = texture2D(concentrations, pos);
  float a = current.r;
  float b = current.g;
  vec3 lp = laplacian(concentrations, pos);

  float a2 = a + (diffusionA*lp.x - a*b*b + feed*(1.0-a)) * deltaT;
  float b2 = b + (diffusionB*lp.y - a*b*b - (kill + feed)*b) * deltaT;

  gl_FragColor = vec4(a2,b2,0.0,0.0);
}
