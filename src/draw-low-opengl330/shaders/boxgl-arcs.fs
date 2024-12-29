#version 330 core
layout(origin_upper_left) in vec4 gl_FragCoord;

in vec4 color;
in vec4 circle_pos;
in vec2 arc_sweeps;

uniform mat4 model;

layout (std140) uniform Matrices
{
    mat4 projection;
    mat4 transform;
    vec2 u_resolution;
};

out vec4 FragColor;

float PI = 3.14;

float figure_ground(in float fill, in bool flip) {
    if (flip) {
        return abs(fill - 1.0);
    }
    else {
        return fill;
    }
}

// https://stackoverflow.com/questions/6270785/how-to-determine-whether-a-point-x-y-is-contained-within-an-arc-section-of-a-c
float arc(in vec2 center, in vec2 pos, in float radius, in float start, in float end, in float width) {
    float togo = 0.0;
    bool flip = false;

    float dist = distance(pos, center);
    togo = 1.0 - step(radius, dist);

    // if width is greater than zero, it's a hollow arc of width
    if (togo == 1.0 && width > 0.0) {
        togo = step(radius - width, dist);
    }

    float xt = pos.x - center.x;
    float yt = pos.y - center.y;
    float at = atan(yt, xt);

    if (yt < 0.0) {
        at = ((radians(180.0)) - abs(at)) + PI;
    }

    if (togo == 1.0) {
        if (start < end) {
            if (start < at && at < end) {
                togo = figure_ground(1.0, flip); // ok
            }
            else {
                togo = figure_ground(0.0, flip);
            }
        }
        else {
            if (at > start) {
                togo = figure_ground(1.0, flip); // ok
            }
            else if (at < end) {
                togo = figure_ground(1.0, flip); // ok
            }
            else {
                togo = figure_ground(0.0, flip);
            }
        }
    }

    return togo;
}

void main() {
    vec4 final_pos = transform * model * vec4(circle_pos.xy, 1.0, 1.0);
    float alpha = 0.0;
    float start = arc_sweeps.x;
    float end = arc_sweeps.y;

    alpha = arc(final_pos.xy , gl_FragCoord.xy, circle_pos.z, start, end, circle_pos.w);

    FragColor = vec4(color.xyz, color.w * alpha);
}
