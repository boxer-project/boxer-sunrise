#version 330 core
layout (location = 0) in vec3 a_position;
layout (location = 1) in vec4 a_color;
layout (location = 2) in vec4 a_circle_pos; // cx, cy, radius, pen-width
layout (location = 3) in vec2 a_arc_sweeps; // start-angle, end-angle

out vec4 color;
out vec4 circle_pos;
out vec2 arc_sweeps;

layout (std140) uniform Matrices
{
    mat4 ortho;
    mat4 transform;
    vec2 u_resolution;
};

void main() {
    color = a_color;
    circle_pos = a_circle_pos;
    arc_sweeps = a_arc_sweeps;
    gl_Position = ortho * transform * vec4(a_position.xyz, 1.0);
}
