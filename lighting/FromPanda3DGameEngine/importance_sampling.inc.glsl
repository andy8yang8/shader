//https://github.com/tobspr/RenderPipeline/blob/master/rpcore/shader/includes/importance_sampling.inc.glsl
#pragma once

// From:
// http://www.trentreed.net/blog/physically-based-shading-and-image-based-lighting/
vec2 hammersley(uint i, uint N)
{
    return vec2(float(i) / float(N), float(bitfieldReverse(i)) * 2.3283064365386963e-10);
}

// From:
// http://www.gamedev.net/topic/655431-ibl-problem-with-consistency-using-ggx-anisotropy/
vec3 importance_sample_ggx(vec2 Xi, float alpha)
{
    // alpha is already squared roughness
    float r_square = alpha * alpha;
    float phi = TWO_PI * Xi.x;
    float cos_theta_sq = (1 - Xi.y) / max(1e-3, 1 + (r_square * r_square - 1) * Xi.y);
    float cos_theta = sqrt(cos_theta_sq);
    float sin_theta = sqrt(max(0.0, 1.0 - cos_theta_sq));
    return vec3(sin_theta * cos(phi), sin_theta * sin(phi), cos_theta);
}

vec3 importance_sample_lambert(vec2 Xi)
{
    float phi = TWO_PI * Xi.x;
    float cos_theta = sqrt(Xi.y);
    float sin_theta = sqrt(1 - Xi.y);
    return vec3(sin_theta * cos(phi), sin_theta * sin(phi), cos_theta);
}
