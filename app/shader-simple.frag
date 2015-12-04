#version 330 core

in vec2 vUV;
out vec4 fragColor;

uniform float uTime;

void main() {

  fragColor = vec4(sin(sin(vUV.x*7+uTime)*53+uTime), 
                   sin(sin(vUV.y*50+uTime)*6+uTime),
                   sin(vUV.y+uTime),1.0);
  // fragColor = vec4(0.5);
}
