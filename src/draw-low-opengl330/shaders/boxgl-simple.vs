#version 330 core
layout (location = 0) in vec2 aPos;

// A vertex shader for simpler lines that gets the color from the global uniform u_rgba
// rather than 4 vertices passed in from a vao location.  Useful for simple things like
// drawing a box border where the entire thing will be the same basic color.
// layout (location = 0) in vec3 aPos;

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
    theColor = vec4(0.0, 0.0, 0.0, 1.0); //u_rgba;
    gl_Position = projection * transform * model * vec4(aPos, 0.0, 1.0);
}
