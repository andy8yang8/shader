/*
*   NOISE CLAMPED
*/
fixed random(in fixed2 _st) {
	return frac(sin(dot(_st.xy, fixed2(12.9898, 78.233)))*43758.5453123);
}

//clamped noise
fixed noise_util3(in fixed2 _st) {
	fixed2 i = floor(_st);
	fixed2 f = frac(_st);
	// Four corners in 2D of a tile
	fixed a = random(i);
	fixed b = random(i + fixed2(1.0, 0.0));
	fixed c = random(i + fixed2(0.0, 1.0));
	fixed d = random(i + fixed2(1.0, 1.0));
	fixed2 u = f * f * (3.0 - 2.0 * f);
	return clamp(lerp(a, b, u.x) +
		(c - a)* u.y * (1.0 - u.x) +
		(d - b) * u.x * u.y, 0.6, 100);
}
#define NUM_OCTAVES 5

fixed fbm_util3(in fixed2 _st) {
	fixed v = 0.0;
	fixed a = .6;//amplitude
	fixed2 shift = fixed2(100.0, 100.0);
	// Rotate to reduce axial bias
	fixed2x2 rot = fixed2x2(cos(0.5), sin(0.5),
		-sin(0.5), cos(0.5));
	for (int i = 0; i < NUM_OCTAVES; ++i) {
		v += a * noise_util3(_st);
		_st = mul(rot, _st) * 2.0 + shift;
		a *= 0.5;
	}
	return v;
}

fixed3 fbm_inaction(fixed aspectR, fixed2 uv, fixed time) {
	fixed2 st = uv * aspectR;

	fixed3 color = fixed3(1.0, 1.0, 1.0);

	fixed2 q = fixed2(0., 0.);
	q.x = fbm_util3(st + 0.00*time);
	q.y = fbm_util3(st + fixed2(1.0, 1.0));


	fixed2 r = fixed2(0., 0.);
	r.x = fbm_util3(st + 1.0*q + fixed2(1.7, 9.2) + 0.15*time);
	r.y = fbm_util3(st + 1.0*q + fixed2(8.3, 2.8) + 0.126*time);

	fixed f = fbm_util3(st + r);

	color = lerp(fixed3(0.101961, 0.619608, 0.666667),
		fixed3(0.666667, 0.666667, 0.498039),
		clamp((f*f)*4.0, 0.0, 1.0));
	color = lerp(color,
		fixed3(0, 0, 0.164706),
		clamp(length(q), 0.0, 1.0));
	color = lerp(color,
		fixed3(0.666667, 1, 1),
		clamp(length(r.x), 0.0, 1.0));

	return fixed3((f*f*f + .6*f*f + .5*f)*color);
}
