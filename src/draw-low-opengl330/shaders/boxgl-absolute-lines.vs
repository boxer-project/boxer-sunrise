#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aColor;

out vec4 theColor;

layout (std140) uniform Matrices
{
    mat4 model;
    mat4 ortho;
    mat4 transform;
    vec2 u_resolution;
    vec4 u_rgba;
};

void main()
{
    theColor = aColor;
    gl_Position = ortho * model * vec4(aPos, 1.0);
}
