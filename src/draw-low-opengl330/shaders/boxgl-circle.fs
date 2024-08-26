#version 330 core
layout(origin_upper_left) in vec4 gl_FragCoord;

in vec4 color;
in vec4 circle_pos;

layout (std140) uniform Matrices
{
    mat4 model;
    mat4 projection;
    mat4 transform;
    vec2 u_resolution;
};

out vec4 FragColor;

float circle(in vec2 center, in vec2 pos, in float radius) {
    float dist = distance(pos, center);
    return 1.0 - step(radius, dist);
}

float hollow_circle(in vec2 center, in vec2 pos, in float radius, in float width) {
    float togo = 0.0;
    // First see if it's in the outermost circle
    float dist = distance(pos, center);
    togo = 1.0 - step(radius, dist);

    // Then if it's outside the innermost circle
    if (togo == 1.0) {
        togo = step(radius - width, dist);
    }

    return togo;
}

void main() {
    vec4 final_pos = transform * model * vec4(circle_pos.xy, 1.0, 1.0);
    float alpha = 0.0;

    if (circle_pos.w > 0.0) {
        alpha = hollow_circle(final_pos.xy , gl_FragCoord.xy, circle_pos.z, circle_pos.w);
    }
    else {
        alpha = circle(final_pos.xy , gl_FragCoord.xy, circle_pos.z);
    }

    FragColor = vec4(color.xyz, alpha * color.w);
}
