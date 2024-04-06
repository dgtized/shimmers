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
    color = vec4(length(color0)/4.0, length(color10)/4.0, length(color25)/4.0, 1.0);
  } else if (u_mode == 2) {
    color = mix(mix(color0, color10, 0.75),
                mix(color0, color25, 0.5), 0.5);
  } else if (u_mode == 3) {
    color = vec4(color0.r,
                 mod(color10.g * 2.0 + color25.g, 1.0),
                 mod(color10.b * 0.5 + color25.b, 1.0),
                 1.0);
  } else if (u_mode == 4) {
    vec4 n[9];
    kernel(n, frame, frame10, frame25, pos, 2.0/u_resolution.x, 2.0/u_resolution.y);
    color = 8.0*n[4] - n[0] - n[1] - n[2] - n[3] - n[5] - n[6] - n[7] - n[8];
  } else if (u_mode == 5) {
    vec4 n[9];
    kernel(n, frame, frame10, frame25, pos, 2.0/u_resolution.x, 2.0/u_resolution.y);
    vec4 edge = 8.0*n[4] - n[0] - n[1] - n[2] - n[3] - n[5] - n[6] - n[7] - n[8];
    color = vec4(length(edge), length(edge), length(edge),1.0);
  } else if (u_mode == 6) {
    vec4 n[9];
    kernel(n, frame, frame10, frame25, pos, 2.0/u_resolution.x, 2.0/u_resolution.y);
    vec4 edge = 8.0*n[4] - n[0] - n[1] - n[2] - n[3] - n[5] - n[6] - n[7] - n[8];
    color = vec4(color0.r/length(edge), color10.g/length(edge), color25.b/length(edge),1.0);
  } else if (u_mode == 7) {
    vec4 n[9];
    kernel(n, frame, frame10, frame25, pos, 2.0/u_resolution.x, 2.0/u_resolution.y);
    vec4 edge = 8.0*n[4] - n[0] - n[1] - n[2] - n[3] - n[5] - n[6] - n[7] - n[8];
    //color = vec4(color0.r/length(edge), color10.g/length(edge), color25.b/length(edge),1.0);
    float t = length(edge)/4.0;
    if(t < 0.5) {
      color = mix(color0, color10, t*2.0);
    } else {
      color = mix(color0, color25, t*2.0-1.0);
    }
  } else if (u_mode == 8) { // motion-extraction
    // https://www.youtube.com/watch?v=NSS6yAMZF78
    vec4 inv0 = vec4(1.0-color0.r,1.0-color0.g,1.0-color0.b,0.5);
    vec4 inv10 = vec4(1.0-color10.r,1.0-color10.g,1.0-color10.b,0.5);
    vec4 inv25 = vec4(1.0-color25.r,1.0-color25.g,1.0-color25.b,0.5);
    vec4 motion =
      vec4(inv0.r,0.0,0.0,0.1)+
      vec4(0.0,inv10.g,0.0,0.3)+
      vec4(0.0,0.0,inv25.b,0.6);
    color = color0+motion;
  } else if (u_mode == 9) { // motion-extraction-mask
    // https://www.youtube.com/watch?v=NSS6yAMZF78
    vec4 inv0 = vec4(1.0-color0.r,1.0-color0.g,1.0-color0.b,0.5);
    vec4 inv10 = vec4(1.0-color10.r,1.0-color10.g,1.0-color10.b,0.5);
    vec4 inv25 = vec4(1.0-color25.r,1.0-color25.g,1.0-color25.b,0.5);
    vec4 motion =
      vec4(inv0.r,0.0,0.0,0.33)+
      vec4(0.0,inv10.g,0.0,0.33)+
      vec4(0.0,0.0,inv25.b,0.33);
    color = vec4(color0.rgb*(color0.rgb+motion.rgb),1.0);
  }

  gl_FragColor = color;
}
