#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
layout (location = 2) in vec4 aColor; // We aren't using the alpha layer yet, but bringing it in
                                      // for future blending possibilities.

out vec2 TexCoords;
out vec4 theColor;

layout (std140) uniform Matrices
{
    mat4 ortho;
    mat4 transform;
    vec2 u_resolution;
};

void main()
{
    gl_Position = ortho * transform * vec4(aPos.xyz, 1.0);
    TexCoords = aTexCoord;
    theColor = aColor;
}
