#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aColor;

out vec4 theColor;

layout (std140) uniform Matrices
{
    mat4 ortho;
    mat4 transform;
    vec2 u_resolution;
    vec4 u_rgba;
    float u_time;
};

uniform float uTime;

void main()
{
    // float ang = 0.87;
    float ang = abs(sin(uTime / 16)) * 6.28;

    // x axis
    // mat4 rotate = mat4(1.0, 0.0, 0.0, 0.0,
    //                    0.0, cos(ang), sin(ang), 0.0,
    //                    0.0, -sin(ang), cos(ang), 0.0,
    //                    0.0, 0.0, 0.0, 1.0);

    // y axis
    mat4 rotate = mat4(cos(ang), 0.0, -sin(ang), 0.0,
                       0.0,      1.0,  0.0, 0.0,
                       sin(ang), 0,   cos(ang), 0.0,
                       0.0, 0.0, 0.0, 1.0);

    // z axis
    // mat4 rotate = mat4(cos(ang), sin(ang), 0.0, 0.0,
    //                    -sin(ang),      cos(ang),  0.0, 0.0,
    //                    0.0, 0.0,   1.0, 0.0,
    //                    0.0, 0.0, 0.0, 1.0);


    theColor = aColor;
    // gl_Position = rotate *  ortho * transform * vec4(aPos, 1.0);
    gl_Position = ortho * transform * vec4(aPos, 1.0);

}
