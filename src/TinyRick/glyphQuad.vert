#version 330 core

uniform mat4 uMVP;
uniform mat4 uModel;

uniform float uXOffset;
uniform float uYOffset;

in vec3 aVertex;
in vec3 aNormal;
in vec2 aTexCoord;

out vec3 vNormal;
out vec2 vTexCoord;

void main() { 

    vec4 finalVertex = vec4(aVertex.x + uXOffset, aVertex.y + uYOffset, aVertex.z, 1.0);
    gl_Position = uMVP * finalVertex;

    vNormal   = aNormal;
    vTexCoord = aTexCoord;
}
