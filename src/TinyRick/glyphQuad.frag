#version 330 core

in vec2 vTexCoord;
in vec3 vNormal;

out vec4 color;

uniform sampler2D uTexture;
uniform vec3 uColor;

void main() {

  float a = texture(uTexture, vTexCoord).r;
  color = vec4(uColor, a);
  
  // Dump the texture contents to color:
  // color = texture(uTexture, vTexCoord);

  // Visualize the UVs:
  // vec2 debugUV = vTexCoord;
  // color = vec4(debugUV.x, debugUV.y, debugUV.x, debugUV.y);
  
  // Just use a color to make sure verts exist:
  // color = vec4(0,1,1,1);
}
