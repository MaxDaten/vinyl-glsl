#version 410 core

#include "signature.glsl"

void main()
{
    VertexPos   = ProjMatrix * ModelMatrix * vec4(vPosition, 1.0);
    
    VertexUV    = vTexture;

    gl_Position = VertexPos;
}
