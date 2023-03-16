#version 330 core
in vec2 TexCoords;
in vec4 theColor;

out vec4 color;

uniform sampler2D text;

void main()
{
    vec4 sampled = vec4(1.0, 1.0, 1.0, texture(text, TexCoords).r);
    color = vec4(theColor.xyz, sampled.w);
}
