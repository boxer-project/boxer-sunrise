#version 330 core
layout (location = 0) in vec4 vertex; // <vec2 pos, vec2 tex>
out vec2 TexCoords;

layout (std140) uniform Matrices
{
    mat4 ortho;
    mat4 transform;
};

void main()
{
    gl_Position = ortho * transform * vec4(vertex.xy, 0.0, 1.0);
    TexCoords = vertex.zw;
}
