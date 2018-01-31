fixed noise_v5(in fixed3 x, sampler2D _mainTex) {
	fixed3 p = floor(x);
	fixed3 f = frac(x);
	f = f * f*(3.0 - 2.0*f);
	//------------use texture instead to reduce GPU workload------------
	fixed2 uv = (p.xy + fixed2(37.0, 17.0)*p.z) + f.xy;
	fixed2 rg = tex2Dlod(_mainTex, float4((uv + 0.5) / 256.0, 0., 0)).yx;
	//------------------------------------------------------------------
	return -1.0 + 2.0*lerp(rg.x, rg.y, f.z);
}

#define NUM_OCTAVES 5

fixed fbm_v5(in fixed2 _st, sampler2D _mainTex) {
	fixed v = 0.0;
	fixed a = .6;//amplitude
	fixed2 shift = fixed2(100.0, 100.0);
	// Rotate to reduce axial bias
	fixed2x2 rot = fixed2x2(cos(0.5), sin(0.5),
		-sin(0.5), cos(0.5));
	for (int i = 0; i < NUM_OCTAVES; ++i) {
		v += a * noise_v5(fixed3(_st, 0.0), _mainTex);//<<<<<<<<<<<
		_st = mul(rot, _st) * 2.0 + shift;
		a *= 0.5;
	}
	return v;
}

fixed3 fbm_inaction_v5(fixed aspectR, fixed2 uv, fixed time, sampler2D _mainTex) {
	fixed2 st = uv * aspectR;

	fixed3 color = fixed3(1.0, 1.0, 1.0);

	fixed2 q = fixed2(0., 0.);
	q.x = fbm_v5(st + 0.00*time, _mainTex);
	q.y = fbm_v5(st + fixed2(1.0, 1.0), _mainTex);


	fixed2 r = fixed2(0., 0.);
	r.x = fbm_v5(st + 1.0*q + fixed2(1.7, 9.2) + 0.15*time, _mainTex);
	r.y = fbm_v5(st + 1.0*q + fixed2(8.3, 2.8) + 0.126*time, _mainTex);

	fixed f = fbm_v5(st + r, _mainTex);

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
//==================================================================================
// only for cloud
//==================================================================================
fixed map5(in fixed3 p, sampler2D t)
{
	fixed3 q = p - fixed3(0.0, 0.1, 1.0)*_Time.y;
	fixed f;
	f = 0.50000*noise_v5(q, t);
	q = q * 2.02;
	f += 0.25000*noise_v5(q, t);
	q = q * 2.03;
	f += 0.12500*noise_v5(q, t);
	q = q * 2.01;
	f += 0.06250*noise_v5(q, t);
	q = q * 2.02;
	f += 0.03125*noise_v5(q, t);
	return clamp(1.5 - p.y - 2.0 + 1.75*f, 0.0, 1.0);
}
fixed map4(in fixed3 p, sampler2D t)
{
	fixed3 q = p - fixed3(0.0, 0.1, 1.0)*_Time.y;
	fixed f;
	f = 0.50000*noise_v5(q, t); q = q * 2.02;
	f += 0.25000*noise_v5(q, t); q = q * 2.03;
	f += 0.12500*noise_v5(q, t); q = q * 2.01;
	f += 0.06250*noise_v5(q, t);
	return clamp(1.5 - p.y - 2.0 + 1.75*f, 0.0, 1.0);
}
fixed map3(in fixed3 p, sampler2D t)
{
	fixed3 q = p - fixed3(0.0, 0.1, 1.0)*_Time.y;
	fixed f;
	f = 0.50000*noise_v5(q, t); q = q * 2.02;
	f += 0.25000*noise_v5(q, t); q = q * 2.03;
	f += 0.12500*noise_v5(q, t);
	return clamp(1.5 - p.y - 2.0 + 1.75*f, 0.0, 1.0);
}
fixed map2(in fixed3 p, sampler2D t)
{
	fixed3 q = p - fixed3(0.0, 0.1, 1.0)*_Time.y;
	fixed f;
	f = 0.50000*noise_v5(q, t); q = q * 2.02;
	f += 0.25000*noise_v5(q, t);;
	return clamp(1.5 - p.y - 2.0 + 1.75*f, 0.0, 1.0);
}
