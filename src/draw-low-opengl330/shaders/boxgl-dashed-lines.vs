#version 330 core
//layout (location = 0) in vec3 aPos;
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec4 aColor;

out vec4 theColor;

flat out vec3 startPos;
out vec3 vertPos;

layout (std140) uniform Matrices
{
    mat4 model;
    mat4 projection;
    mat4 transform;
        vec2 u_resolution;
    vec4 u_rgba;
};

void main()
{
    theColor = aColor;
    vec4 pos = projection * transform * model *vec4(aPos, 0.0, 1.0);
    gl_Position = pos;
    vertPos = pos.xyz / pos.w;
    startPos = vertPos;
}
