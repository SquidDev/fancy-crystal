#version 330

in vec3 position;
in vec3 normal;
in vec3 color;

uniform mat4 mvp;
uniform mat4 mv;
uniform mat4 v;
uniform mat3 mv_inv;
uniform mat4 mvp_bias;
uniform vec3 lightPosition;

out vec3 f_color;
out vec4 f_shadowCoord;
out vec3 f_position;

float ambient = 0.5;

void main(void) {
  gl_Position = mvp * vec4(position, 1);

  vec3 normalDirection = normalize(mv_inv * normal);
  vec3 lightDirection = -normalize((v * vec4(lightPosition, 1)).xyz - (mv * vec4(position, 1)).xyz);
  vec3 diffuseReflection = color * ambient + (1 - ambient) * color * max(0, dot(normalDirection, lightDirection));

  f_color = diffuseReflection;
  f_position = position;
  f_shadowCoord = mvp_bias * vec4(position, 1);
}
