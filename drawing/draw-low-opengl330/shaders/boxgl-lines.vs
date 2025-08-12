#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aColor;

out vec4 theColor;

uniform mat4 model;

layout (std140) uniform Matrices
{
    mat4 projection;
    mat4 transform;
    vec2 u_resolution;
};

void main()
{
    theColor = aColor;
    gl_Position = projection * transform * model * vec4(aPos, 1.0);
}
