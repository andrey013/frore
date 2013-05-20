#version 330 core

// Interpolated values from the vertex shaders
in vec2 UV;

// Ouput data
out vec4 color;

// Values that stay constant for the whole mesh.
uniform sampler2D tex;

void main(){
    /*
    color = texture2D(tex, UV);
    */
    vec4 t = texture2D(tex, UV);
    if ((t.r > 0.5) && (t.g > 0.5) ){// && (t.b > 0.5) && (t.a > 0.5)) {
        color = vec4(0.0, 0.0, 0.0, 1.0);; //vec4(0.5, UV.x, 0.1, 1.0);
    } else {
        color = vec4(t.r, t.g, t.b, t.a);
    }
    
}