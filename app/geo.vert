#version 330 core

uniform mat4 uMVP2;

in vec2 aUV;
in vec3 aPosition;

out vec2 vUV;

void main() {
  vUV = aUV;

  gl_Position = uMVP2 * vec4(aPosition, 1.0);

}
