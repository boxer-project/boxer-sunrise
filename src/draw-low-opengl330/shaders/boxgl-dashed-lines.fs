#version 330 core

flat in vec3 startPos;
in vec3 vertPos;

in vec4 theColor;
out vec4 FragColor;

uniform vec2 u_resolution;

void main()
{
  // https://stackoverflow.com/questions/52928678/dashed-line-in-opengl3
  float u_dashSize = 3.0;
  float u_gapSize = 3.0;

  vec2  dir  = (vertPos.xy-startPos.xy) * u_resolution/2.0;
  float dist = length(dir);

  if (fract(dist / (u_dashSize + u_gapSize)) > u_dashSize/(u_dashSize + u_gapSize))
      discard;

  FragColor = theColor;
}
