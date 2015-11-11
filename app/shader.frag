#version 330 core

in vec2 vUV;
out vec4 fragColor;

void main() {

  fragColor = vec4(sin(sin(vUV.x*10)*53), 
                   sin(sin(vUV.y*10)*35),
                   sin(vUV.x*250),1.0);
  //fragColor = vec4(0.5);
}
