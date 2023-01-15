#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aColor;

out vec4 theColor;

layout (std140) uniform Matrices
{
    mat4 ortho;
    mat4 transform;
};

void main()
{
    theColor = aColor;
    gl_Position = ortho * transform * vec4(aPos, 1.0);
}
