#version 330

in vec3 position;
in vec3 normal;
in vec3 color;

uniform mat4 mvp;
uniform mat3 mv_inv;
uniform mat4 mvp_bias;

out vec3 f_color;
out vec4 f_shadowCoord;
out vec3 f_position;

vec3 materialDiffuse = vec3(1.0, 1.0, 1.0);
vec3 materialAmbiant = vec3(0.3, 0.3, 0.3);

vec3 lightPosition = vec3(0.0, 0.0, -3.0);
vec3 lightDiffuse = vec3(0.7, 0.7, 0.7);

void main(void) {
  gl_Position = mvp * vec4(position, 1);

  vec3 normalDirection = normalize(mv_inv * normal);
  vec3 lightDirection = normalize(lightPosition);
  vec3 diffuseReflection = (color * 0.1) + lightDiffuse * color * max(0, dot(normalDirection, lightDirection));

  f_color = diffuseReflection;
  f_position = position;
  f_shadowCoord = mvp_bias * vec4(position, 1);
}
