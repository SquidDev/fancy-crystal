#version 120

attribute vec3 position;
attribute vec3 normal;
attribute vec3 color;

uniform mat4 mvp;
uniform mat3 mv_inv;

varying vec3 f_color;

vec3 materialDiffuse = vec3(1.0, 0.8, 0.8);

vec3 lightPosition = vec3(0.0, 0.0, -3.0);
vec3 lightDiffuse = vec3(1.0, 1.0, 1.0);

void main(void) {
  gl_Position = mvp * vec4(position, 1);

  vec3 normalDirection = normalize(mv_inv * normal);
  vec3 lightDirection = normalize(lightPosition);
  vec3 diffuseReflection = lightDiffuse * color * max(0, dot(normalDirection, lightDirection));

  f_color = diffuseReflection;
}
