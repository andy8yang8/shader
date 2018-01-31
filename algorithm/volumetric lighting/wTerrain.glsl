#ifdef GL_ES
precision mediump float;
#endif

#extension GL_OES_standard_derivatives : enable

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;
// Created by Vinicius Graciano Santos - vgs/2016
// https://www.shadertoy.com/view/4ltGDS
// Making of: http://viniciusgraciano.com/blog/making-of-sunfall/
// The first pass renders the background, the terrain, and the dust volumetric effect.
#define RAYMARCH_STEPS 80
#define FAR_PLANE 150.0
// Perlin Noise using quintic interpolation
//from Dave (https://www.shadertoy.com/view/4djSRW)
/*
float hash(vec2 p) {
	p = fract(p *vec2(5.3983, 5.4427));
	p += dot(p.yx, p.xy + vec2(21.5351, 14.3137));
	return fract(p.x * p.y * 95.4337);
}
float noise(vec2 p) {
	vec2 i = floor(p);
	vec2 f = fract(p);
	vec2 u = f * f*(3.0 - 2.0*f);
	return -1.0 + 2.0*mix(mix(hash(i + vec2(0.0, 0.0)),
		hash(i + vec2(1.0, 0.0)), u.x),
		mix(hash(i + vec2(0.0, 1.0)),
			hash(i + vec2(1.0, 1.0)), u.x), u.y);
}
*/
float noise(vec2 uv) {
    vec2 iuv = floor(uv);
    vec2 fuv = fract(uv);

    vec4 i = vec4(iuv, iuv + 1.0);
    vec4 f = vec4(fuv, fuv - 1.0);

    i = (i + 0.5) / resolution.xy;

    vec2 grad_a = 2.0 * texture(iChannel0, i.xy, -100.0).rg - 1.0;
    vec2 grad_b = 2.0 * texture(iChannel0, i.zy, -100.0).rg - 1.0;
    vec2 grad_c = 2.0 * texture(iChannel0, i.xw, -100.0).rg - 1.0;
    vec2 grad_d = 2.0 * texture(iChannel0, i.zw, -100.0).rg - 1.0;

    float a = dot(f.xy, grad_a);
    float b = dot(f.zy, grad_b);
    float c = dot(f.xw, grad_c);
    float d = dot(f.zw, grad_d);

    fuv = fuv*fuv*fuv*(fuv*(fuv*6.0 - 15.0) + 10.0);
    return mix(mix(a, b, fuv.x), mix(c, d, fuv.x), fuv.y);
}
// Low quality, turbulent FBM
float fbm(vec2 uv) {
    float h = 0.0, a = 1.0;
    for (int i = 0; i < 4; ++i) {
        h += 1.0-abs(a * noise(uv));
        a *= 0.45; uv *= 2.02;
    }
    return h;
}
// High quality, turbulent FBM
float fbmH(vec2 uv) {
    float h = 0.0, a = 1.0;
    for (int i = 0; i < 9; ++i) {
        h += 1.0-abs(a * noise(uv));
        a *= 0.45; uv *= 2.02;
    }
    return h;
}
// Auxiliary functions for raymarching
float terrain(vec2 p) {
    float h = fbm(0.025 * p);
    return 10.0*h;
}
float terrainH(vec2 p) {
    float h = fbmH(0.025 * p);
    return 10.0*h;
}
float map(vec3 p) {
    return 0.8*(p.y - terrain(p.xz));
}
float mapH(vec3 p) {
    return p.y - terrainH(p.xz);
}
// Filter the normal at a distance to avoid aliasing
vec3 normal(vec3 p, float t) {
    vec2 q = vec2(0.0, t * t /resolution.y + 0.01);
    return normalize(vec3(mapH(p + q.yxx) - mapH(p - q.yxx),
                          mapH(p + q.xyx) - mapH(p - q.xyx),
                          mapH(p + q.xxy) - mapH(p - q.xxy)));
}
// Just a basic raymarcher, not much to see here...
float raymarch(vec3 ro, vec3 rd) {
    float t = 0.0, pix = 2.0 / resolution.y;
    for (int i = 0; i < RAYMARCH_STEPS; ++i) {
        float d = map(ro + t * rd);
        if (d < pix * t || t > FAR_PLANE)
            break;
        t += d;
    }
    return t;
}
// Global variables
vec3 g_sundir = normalize(vec3(-0.75, 0.0, -1.0));
vec3 g_suncol = 1.65*vec3(1.0, 0.792, 0.455);
vec3 g_sand_diff = vec3(0.76, 0.7, 0.5);
vec3 g_sky_blue = vec3(0.218, 0.325, 0.455);
// Background Color
vec3 bg(vec3 rd) {
    vec3 col = mix(vec3(0.547, 0.455, 0.218),
                   g_sky_blue,
                   smoothstep(-0.5, 1.0, rd.y));
    return 1.65*col;
}
vec3 shade(vec3 ro, vec3 rd, float t) {
    const float pi = 3.141592;

    vec3 p = ro + t * rd;
    vec3 n = normal(p, t);

    // Diffuse
    vec3 diff_brdf = g_sand_diff / pi;
    // Specular
    float m = 20.0;
    vec3 h = normalize(g_sundir - rd);
    vec3 spec_brdf = vec3((m + 8.0)*pow(max(dot(n, h), 0.0), m)/(8.0*pi));
    float schlick = 0.045 + 0.955*pow(1.0 - dot(h, -rd), 5.0);
    // Rendering Equation
    vec3 brdf = mix(diff_brdf, spec_brdf, schlick);
    vec3 col = brdf * g_suncol * max(dot(n, g_sundir), 0.0);
    // Fill light hack
    col += 0.75*g_sky_blue*diff_brdf*max(dot(n, -rd), 0.0);
    // Ambient Hack
    m = smoothstep(0.0, 1.0, n.y);
    col = mix(col, g_sand_diff * vec3(0.382, 0.39, 0.336)*m, 0.2);
    float fog = exp(-0.015*t);

    return mix(bg(rd), 7.0*col, fog);
}
// LookAt transform for positioning the camera
mat3 lookAt(vec3 position, vec3 target) {
    vec3 f = normalize(position - target);
    vec3 r = cross(vec3(0.0, 1.0, 0.0), f);
    return mat3(r, cross(f, r), f);
}
// I decided to add a volumetric effect and was lazy to code 3D Perlin Noise,
// so I got the 3D Value Noise by iq ( https://www.shadertoy.com/view/4sfGzS )
float hash(float n ) { return fract(sin(n)*43758.5453123); }
float noise(vec3 x) {
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);

    float n = p.x + p.y*157.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+157.0), hash(n+158.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+270.0), hash(n+271.0),f.x),f.y),f.z);
}
// Another FBM function, but uses IQ's 3D Value Noise
float fbm(vec3 p) {
	float k = 0.0;
	p *= 1.25;
	k += 1.000*noise(p); p*=2.0;
	k += 0.500*noise(p); p*=2.0;
	k += 0.250*noise(p);
	return k/1.75;
}
// Volume-march backwards after the main raymarcher has finished
vec3 volmarch(vec3 ro, vec3 rd, float t, vec3 col) {
    float pix = 2.0 / resolution.y;

    for (int i = 0; i < 10; ++i) {
        vec3 p = ro + t * rd - vec3(-.5, .5, .1)*time;
    	float EPS = pix * t;

        float f1 = 0.25*fbm(p);
        float f2 = 0.25*fbm(p + EPS*g_sundir);

        // Directional derivatives FTW!
        //( http://www.iquilezles.org/www/articles/derivative/derivative.htm )
        vec3 shade = g_suncol * g_sand_diff * max(f2 - f1, 0.0)/EPS;

        col = mix(col, shade, f1*smoothstep(0.1, -0.2, rd.y));

        // hashing to reduce banding
        t *= 0.9*(0.9 + 0.1*hash(t));
    }
    return col;
}
void main(void)
{
	vec2 uv = (-resolution.xy + 2.0*gl_FragCoord.xy) / resolution.y;

    // Trace rays
    vec2 pos = vec2(0.0, -time-100.0);
    vec3 ro = vec3(pos.x, terrain(pos) + 2.0, pos.y);
    mat3 m = lookAt(ro, ro - vec3(0.0, 0.25, 1.0));
    vec3 rd = normalize(m*vec3(uv, -1.0));
    // Main raymarcher and shading routines
    gl_FragColor = vec4(bg(rd), 0.0);
    if (rd.y > 0.2)
        return;
    float t = raymarch(ro, rd), pass = float(t < FAR_PLANE);
    gl_FragColor.rgb = mix(gl_FragColor.rgb, shade(ro, rd, t), pass);
    // Occlusion mask for improving the radial blur in the next render
    gl_FragColor.a = pow(max(dot(rd, g_sundir), 0.0), 100.0)*(1.0-pass);
    // Volumetric effect (dust)
    if (rd.y > 0.1)
        return;
    gl_FragColor.rgb = volmarch(ro, rd, t, gl_FragColor.rgb);
}
