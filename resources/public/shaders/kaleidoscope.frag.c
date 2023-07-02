#ifdef GL_ES
precision mediump float;
#endif

#define pi 3.14159

varying vec2 vTexCoord;

uniform vec2 u_resolution;
uniform float u_time;
uniform int u_mode;
uniform float blades;

uniform sampler2D frame;
uniform sampler2D frame10;
uniform sampler2D frame25;

// cribbed from https://www.shadertoy.com/view/MtKXDR
vec2 kaleidoscope(vec2 uv)
{
  float th = atan(uv.y, uv.x);
  float r = pow(length(uv), 1.);

  float q = 2. * pi / blades;
  th = abs(mod(th + cos(0.1*u_time), q) - 0.5 * q);
  return pow(r, 1.2)*vec2(cos(th), sin(th)) * 0.3;
}

vec2 transform(vec2 at)
{
  vec2 v;
  float fov = 0.5*sin(u_time*0.2);
  v.x = at.x * cos(fov) - at.y * sin(fov) - 0.2 * sin(fov);
  v.y = at.x * sin(fov) + at.y * cos(fov) + 0.2 * cos(fov);
  return v;
}

vec4 scene(sampler2D tex, vec2 at)
{
  return texture2D(tex, transform(mod(at+0.0, 1.0)) * 2.0);
}

void main() {
  vec2 pos = vTexCoord.xy;
  pos.x = mix(-1.0,1.0,pos.x);
  pos.y = mix(-1.0,1.0,pos.y);
  pos.y *= u_resolution.y / u_resolution.x;

  vec4 color;
  if(u_mode == 0) {
    color = scene(frame, kaleidoscope(pos));
  } else if(u_mode == 1) {
    color.r = scene(frame, kaleidoscope(pos)).r;
    color.g = scene(frame10, kaleidoscope(pos)).g;
    color.b = scene(frame25, kaleidoscope(pos)).b;
  }

  gl_FragColor = vec4(color.rgb, 1.0);
}
