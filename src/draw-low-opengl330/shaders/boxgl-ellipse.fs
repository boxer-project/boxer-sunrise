#version 330 core
layout(origin_upper_left) in vec4 gl_FragCoord;

in vec4 color;
in vec3 circle_pos;
in vec2 ellipse_dims;

uniform mat4 model;

layout (std140) uniform Matrices
{
    mat4 projection;
    mat4 transform;
    vec2 u_resolution;
};

out vec4 FragColor;

float ellipse(in vec2 center, in vec2 pos, in float width, in float height, in float size) {
    float togo = 0.0;

    float x2 = pow(pos.x - center.x, 2.);
    float y2 = pow(pos.y - center.y, 2.);
    float a2 = pow(width / 2., 2.);
    float b2 = pow(height / 2., 2.);
    float final = (x2 / a2) + (y2 / b2);

    if (final <= 1.0) {
        togo = 1.0;
    }

    if (size > 0.) {
        a2 = pow((width - size) / 2., 2.);
        b2 = pow((height - size) / 2., 2.);
        final = (x2 / a2) + (y2 / b2);
        if (final <= 1.0) {
            togo = 0.0;
        }
    }

    return togo;
}

void main() {
    vec4 final_pos = transform * model * vec4(circle_pos.xy, 1.0, 1.0);
    float alpha = 0.0;

    alpha = ellipse(final_pos.xy,  gl_FragCoord.xy, ellipse_dims.x, ellipse_dims.y, circle_pos.z);

    FragColor = vec4(color.xyz, color.w * alpha);
}

