#ifdef GL_ES
precision mediump float;
#endif

varying vec2 vTexCoord;

uniform vec2 u_resolution;
uniform float u_time;
uniform int u_mode;

uniform sampler2D frame;
uniform sampler2D frame10;
uniform sampler2D frame25;

void kernel(inout vec4 n[9],
            sampler2D tex0,
            sampler2D tex1,
            sampler2D tex2,
            vec2 pos, float w, float h) {
  n[0] = texture2D(tex2, pos + vec2(-w,-h));
  n[1] = texture2D(tex1, pos + vec2(0.0, -h));
  n[2] = texture2D(tex2, pos + vec2(w, -h));
  n[3] = texture2D(tex1, pos + vec2(-w, 0.0));
  n[4] = texture2D(tex0, pos);
  n[5] = texture2D(tex1, pos + vec2(w, 0.0));
  n[6] = texture2D(tex2, pos + vec2(-w, h));
  n[7] = texture2D(tex1, pos + vec2(0.0, h));
  n[8] = texture2D(tex2, pos + vec2(w, h));
}

void main() {
  vec2 pos = vTexCoord;
  pos.y = 1.0 - pos.y;

  vec4 color0 = texture2D(frame, pos);
  vec4 color10 = texture2D(frame10, pos);
  vec4 color25 = texture2D(frame25, pos);

  vec4 color;
  if(u_mode == 0) {
    color = vec4(color0.r, color10.g, color25.b, 1.0);
  } else if (u_mode == 1) {
    color = vec4(1.0-1.0/length(color0), 1.0-1.0/length(color10), 1.0-1.0/length(color25), 1.0);
  } else if (u_mode == 2) {
    color = vec4(color0.r,
                 mod(color10.g * 2.0 + color25.g, 1.0),
                 mod(color10.b * 0.5 + color25.b, 1.0),
                 1.0);
  } else if (u_mode == 3) {
    vec4 n[9];
    kernel(n, frame, frame10, frame25, pos, 2.0/u_resolution.x, 2.0/u_resolution.y);
    color = 8.0*n[4] - n[0] - n[1] - n[2] - n[3] - n[5] - n[6] - n[7] - n[8];
  }

  gl_FragColor = color;
}
