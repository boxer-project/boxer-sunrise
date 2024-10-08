#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 TexCoord;

uniform mat4 model;

layout (std140) uniform Matrices
{
    mat4 projection;
    mat4 transform;
    vec2 u_resolution;
};

void main()
{
    gl_Position = projection * transform * model * vec4(aPos, 1.0);
    TexCoord = vec2(aTexCoord.x, aTexCoord.y);
}
