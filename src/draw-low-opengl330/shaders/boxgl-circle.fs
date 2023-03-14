#version 330 core
layout(origin_upper_left) in vec4 gl_FragCoord;

in vec4 color;
in vec3 circle_pos;

layout (std140) uniform Matrices
{
    mat4 ortho;
    mat4 transform;
    vec2 u_resolution;
};

out vec4 FragColor;

float circle(in vec2 center, in vec2 pos, in float radius) {
    float dist = distance(pos, center);
    return 1.0 - step(radius, dist);
}

void main() {
    vec4 final_pos = transform * vec4(circle_pos.xy, 1.0, 1.0);
    FragColor = vec4(color.xyz, circle(final_pos.xy , gl_FragCoord.xy, circle_pos.z));
}
