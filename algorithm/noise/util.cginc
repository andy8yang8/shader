//------------------------------------------------------------------
//----------------------Utility functions---------------------------
//------------------------------------------------------------------
fixed3 rotx(fixed3 p, fixed a) {
	fixed s = sin(a), c = cos(a);
	return fixed3(p.x, c*p.y - s * p.z, s*p.y + c * p.z);
}
fixed3 roty(fixed3 p, fixed a) {
	fixed s = sin(a), c = cos(a);
	return fixed3(c*p.x + s * p.z, p.y, -s * p.x + c * p.z);
}
//from Dave (https://www.shadertoy.com/view/4djSRW)
fixed hash(fixed2 p) {
	p = frac(p * fixed2(5.3983, 5.4427));
	p += dot(p.yx, p.xy + fixed2(21.5351, 14.3137));
	return frac(p.x * p.y * 95.4337);
}

fixed noise(in fixed2 p) {
	fixed2 i = floor(p);
	fixed2 f = frac(p);
	fixed2 u = f * f*(3.0 - 2.0*f);
	return -1.0 + 2.0*lerp(lerp(hash(i + fixed2(0.0, 0.0)),
		hash(i + fixed2(1.0, 0.0)), u.x),
		lerp(hash(i + fixed2(0.0, 1.0)),
			hash(i + fixed2(1.0, 1.0)), u.x), u.y);
}
//------------------------------------------------------------------
//-------------------------------Extras(requires time and tecture)--
//------------------------------------------------------------------
fixed bnoise(in fixed2 p, in sampler2D tex, fixed time)
{
	fixed d = sin(p.x*1.5 + sin(p.y*.2))*0.1;//<<<<<<<<<<<<
	return d += tex2D(tex, p.xy + time * 0.001).x*0.04;//<<<<<<<<<<<<<<<
}

fixed3 bump(in fixed2 p, in fixed3 n, in fixed t, in sampler2D tex, in fixed time)
{
	fixed2 e = fixed2(40., 0) / (t*t);
	fixed n0 = bnoise(p, tex, time);
	fixed3 d = fixed3(bnoise(p + e.xy, tex, time) - n0, 2., bnoise(p + e.yx, tex, time) - n0) / e.x;
	n = normalize(n - d);
	return n;
}

//------------------------------------------------------------------------
//------------------------------------------------------------------------
fixed noise_v2(fixed3 uv, fixed res)
{
	const fixed3 s = fixed3(1e0, 1e2, 1e3);
	uv *= res;
	fixed3 uv0 = floor(fmod(uv, res))*s;
	fixed3 uv1 = floor(fmod(uv + fixed3(1., 1., 1.), res))*s;
	fixed3 f =
		//	hash(
		frac(uv)
		//		)
		;
	f = f * f*(3.0 - 2.0*f);
	fixed4 v = fixed4(uv0.x + uv0.y + uv0.z,
		uv1.x + uv0.y + uv0.z,
		uv0.x + uv1.y + uv0.z,
		uv1.x + uv1.y + uv0.z);
	fixed4 r = frac(sin(v*1e-1)*1e3);
	fixed r0 = lerp(lerp(r.x, r.y, f.x), lerp(r.z, r.w, f.x), f.y);
	r = frac(sin((v + uv1.z - uv0.z)*1e-1)*1e3);
	fixed r1 = lerp(lerp(r.x, r.y, f.x), lerp(r.z, r.w, f.x), f.y);
	return lerp(r0, r1, f.z)*2. - 1.;
}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// Volumetric clouds. It performs level of detail (LOD) for faster rendering
fixed noise_iq(in fixed3 x, in sampler2D Tex)
{
	fixed3 p = floor(x);
	fixed3 f = frac(x);
	f = f * f*(3.0 - 2.0*f);
	//#if 1
	//	fixed2 uv = (p.xy + fixed2(37.0, 17.0)*p.z) + f.xy;
	//	fixed2 rg = tex2Dlod(_MainTex, float4((uv + 0.5) / 256.0, 0., 0)).yx;
	//#else
	fixed3 q = p;
	fixed2 uv = q.xy + fixed2(37, 17)*q.z;
	fixed2 rg =
		lerp(
			lerp(tex2D(Tex, uv),
				tex2D(Tex, uv + fixed2(1, 0)), f.x),
			lerp(tex2D(Tex, uv + fixed2(0, 1)),
				tex2D(Tex, uv + fixed2(1, 1)), f.x), f.y
		).yx;
	//#endif
	//vec2 rg = mix(
	//	mix(texelFetch(iChannel0, uv),
	//		texelFetch(iChannel0, uv + ivec2(1, 0)), f.x),
	//	mix(texelFetch(iChannel0, uv + ivec2(0, 1)),
	//		texelFetch(iChannel0, uv + ivec2(1, 1)), f.x), f.y
	//).yx;

	return -1.0 + 2.0*lerp(rg.x, rg.y, f.z);
}
