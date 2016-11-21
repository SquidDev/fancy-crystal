#version 330

in vec3 f_color;
in vec4 f_shadowCoord;
in vec3 f_position;

uniform sampler2DShadow shadow;
vec2 poissonDisk[16] = vec2[](
  vec2( -0.94201624, -0.39906216 ),
  vec2( 0.94558609, -0.76890725 ),
  vec2( -0.094184101, -0.92938870 ),
  vec2( 0.34495938, 0.29387760 ),
  vec2( -0.91588581, 0.45771432 ),
  vec2( -0.81544232, -0.87912464 ),
  vec2( -0.38277543, 0.27676845 ),
  vec2( 0.97484398, 0.75648379 ),
  vec2( 0.44323325, -0.97511554 ),
  vec2( 0.53742981, -0.47373420 ),
  vec2( -0.26496911, -0.41893023 ),
  vec2( 0.79197514, 0.19090188 ),
  vec2( -0.24188840, 0.99706507 ),
  vec2( -0.81409955, 0.91437590 ),
  vec2( 0.19984126, 0.78641367 ),
  vec2( 0.14383161, -0.14100790 )
);

// Returns a random number based on a vec3 and an int.
float random(vec3 seed, int i){
  vec4 seed4 = vec4(seed,i);
  float dot_product = dot(seed4, vec4(12.9898,78.233,45.164,94.673));
  return fract(sin(dot_product) * 43758.5453);
}
void main(void) {
  float visibility = 1.0;
  for(int i = 0; i < 4; i++) {
    int index = int(16.0 * random(floor(f_position * 1000.0), i)) % 16;
    visibility -= 0.1 * (1.0 - texture(shadow, vec3(f_shadowCoord.xy + poissonDisk[index]/7000.0, (f_shadowCoord.z - 0.05)/f_shadowCoord.w)));
  }

  gl_FragColor = vec4(visibility * f_color, 1);
}
