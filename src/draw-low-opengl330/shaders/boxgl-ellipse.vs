#version 330 core
layout (location = 0) in vec3 a_position;
layout (location = 1) in vec4 a_color;
layout (location = 2) in vec3 a_circle_pos; // cx, cy, pen-width
layout (location = 3) in vec2 a_ellipse_dims;

out vec4 color;
out vec3 circle_pos;
out vec2 ellipse_dims;

layout (std140) uniform Matrices
{
    mat4 ortho;
    mat4 transform;
    vec2 u_resolution;
};

void main() {
    color = a_color;
    circle_pos = a_circle_pos;
    ellipse_dims = a_ellipse_dims;
    gl_Position = ortho * transform * vec4(a_position.xyz, 1.0);
}
