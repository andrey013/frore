#version 330 core

// Interpolated values from the vertex shaders
in vec2 UV;

// Ouput data
out vec4 color;

// Values that stay constant for the whole mesh.
uniform sampler2D tex;

void main(){
    
    color = texture2D(tex, UV);
    /*
    if (texture2D(tex, UV).a > 0.5) {
        color = vec4(0.0, 0.0, 0.0, 1.0);; //vec4(0.5, UV.x, 0.1, 1.0);
    } else {
        color = vec4(0.0, 0.0, 0.0, 0.0);
    }
    */
}