#version 330 core

layout(location = 0) in vec3 vert;
layout(location = 1) in vec2 vertUV;

uniform mat4 P;
uniform mat4 V;
uniform mat4 M;

// Output data ; will be interpolated for each fragment.
out vec2 UV;

void main()
{
    gl_Position = P * /* V * M */ vec4(vert,1.0);
    UV = vertUV;
}