#version 330 core

uniform vec3 circle_xyrad;

layout (std140) uniform Matrices
{
    mat4 model;
    mat4 projection;
    mat4 transform;
    vec2 u_resolution;
};

out vec4 FragColor;

float circle(in vec2 _st, in float _radius){
    vec2 dist = _st-vec2(0.5);
    // return 1.-smoothstep(_radius-(_radius*0.01),
    //                      _radius+(_radius*0.01),
    //                      dot(dist,dist)*4.0);
    return 1.-step(_radius, dot(dist,dist)*4.0);
}

float circle2(in vec2 center, in vec2 _st, in float _radius){
    vec2 dist = _st-center;
    // return 1.-smoothstep(_radius-(_radius*0.01),
    //                      _radius+(_radius*0.01),
    //                      dot(dist,dist)*4.0);
    return 1.-step(_radius, dot(dist,dist)*4.0);
}

void main() {
    vec2 center = vec2(200.0, 100.0);

    vec3 new_circle = vec3(circle_xyrad.x, u_resolution.y - circle_xyrad.y, circle_xyrad.z);

    vec4 finalCoord = transform * gl_FragCoord;

    vec2 st = finalCoord.xy/u_resolution;
    FragColor = vec4(st.x, st.y, 0.0, circle2(new_circle.xy, finalCoord.xy, 50.0));
}
