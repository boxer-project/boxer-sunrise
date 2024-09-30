#version 330 core
layout (location = 0) in vec2 a_position;

uniform mat4 model;

void main() {
    gl_Position = vec4(a_position, 0.0, 1.0);
}
