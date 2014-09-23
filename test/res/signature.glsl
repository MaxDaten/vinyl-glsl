#ifndef __SIGNATURE__
#define __SIGNATURE__

in vec3 vPosition;
in vec2 vTexture;

uniform mat4 ProjMatrix     = mat4(0.0);
uniform mat4 ModelMatrix    = mat4(0.0);

out vec2 VertexUV;
out vec4 VertexPos;

#endif // __SIGNATURE__