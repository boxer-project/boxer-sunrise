#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;

out vec3 theColor;

uniform mat4 ortho;
uniform mat4 transform;

void main()
{
    theColor = aColor;
    gl_Position = ortho * transform * vec4(aPos, 1.0);
}
