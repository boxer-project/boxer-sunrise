#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 TexCoord;

layout (std140) uniform Matrices
{
    mat4 ortho;
    mat4 transform;
};

void main()
{
    gl_Position = ortho * transform * vec4(aPos, 1.0);
    TexCoord = vec2(aTexCoord.x, aTexCoord.y);
}
