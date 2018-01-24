fixed random(in fixed2 _st) {
	return frac(sin(dot(_st.xy, fixed2(12.9898, 78.233)))*43758.5453123);
}
fixed noise(in fixed2 _st) {
	fixed2 i = floor(_st);
	fixed2 f = frac(_st);
	// Four corners in 2D of a tile
	fixed a = random(i);
	fixed b = random(i + fixed2(1.0, 0.0));
	fixed c = random(i + fixed2(0.0, 1.0));
	fixed d = random(i + fixed2(1.0, 1.0));
	fixed2 u = f * f * (3.0 - 2.0 * f);
	return lerp(a, b, u.x) +
		(c - a)* u.y * (1.0 - u.x) +
		(d - b) * u.x * u.y;
}
#define NUM_OCTAVES 5
fixed fbm(in fixed2 _st) {
	fixed v = 0.0;
	fixed a = .6;//amplitude
	fixed2 shift = fixed2(100.0, 100.0);
	// Rotate to reduce axial bias
	fixed2x2 rot = fixed2x2(cos(0.5), sin(0.5),
		-sin(0.5), cos(0.5));
	for (int i = 0; i < NUM_OCTAVES; ++i) {
		v += a * noise(_st);
		_st = mul(rot, _st) * 2.0 + shift;
		a *= 0.5;
	}
	return v;
}
