// based on scrolling plane
Shader "SetPosition"
{
	SubShader
	{
		Tags{ "RenderType" = "Transparent" "Queue" = "Transparent" }
		Pass
	{
		ZWrite Off
		Blend SrcAlpha OneMinusSrcAlpha

		CGPROGRAM
		#pragma vertex vert
		#pragma fragment frag

		struct VertexInput {
			fixed4 vertex : POSITION;
			fixed2 uv : TEXCOORD0;
		};
		struct VertexOutput {
			fixed4 pos : SV_POSITION;
			fixed2 uv : TEXCOORD0;
		};
		#define FAR 20
		fixed2x2 rot2(fixed a) {
			fixed2 v = sin(fixed2(1.570796, 0) - a);
			return fixed2x2(v, -v.y, v.x);
		}
		//SDF
		fixed map(fixed3 p)
		{
			const fixed R = 5.0;
			//从镜头出发(ro),已经步进到p点用来取样，向量的长度减掉球半径
			return length(p) - R;
			//return p.y + dot(sin(p / 2. + cos(p.yzx / 2. + 3.14159 / 2.)), fixed3(.5,.5,.5));
		}
		fixed raymarch(fixed3 ro, fixed3 rd)
		{
			fixed t = 0.0;
			for (int i = 0; i < 96; i++)
			{
				fixed d = map(ro + rd * t);
				if (abs(d) < 0.0025*(t*.125 + 1.) || t > FAR) break;
				t += d * .7;  // Using more accuracy, in the first pass.
			}
			return min(t, FAR);
		}
		fixed3 getNormal(in fixed3 p)
		{
			const fixed2 e = fixed2(0.005, 0);
			return normalize(fixed3(map(p + e.xyy) - map(p - e.xyy), map(p + e.yxy) - map(p - e.yxy),	map(p + e.yyx) - map(p - e.yyx)));
		}
		VertexOutput vert(VertexInput v)
		{
			VertexOutput o;
			o.pos = UnityObjectToClipPos(v.vertex);
			o.uv = v.uv;
			return o;
		}
		fixed4 frag(VertexOutput i) : SV_Target
		{
			fixed2 uv = (0.5 - i.uv);
			// Camera
			float time = 0.0;
			//_Time.y;
			fixed3 lk = fixed3(0, 3.5, time);  // "Look At" position.
			fixed3 ro = lk + fixed3(0, 2.25, -.25); // Camera position, doubling as the ray origin.
			//Light positioning. One is just in front of the camera, and the other is in front of that.
			fixed3 lp = ro + fixed3(0, .75, 2);// Put it a bit in front of the camera.
			fixed3 lp2 = ro + fixed3(0, .75, 9);// Put it a bit in front of the camera.
			// Using the above to produce the unit ray-direction fixedtor.
	   	fixed  FOV = 20.;
	   	fixed3 fwd = normalize(lk - ro);
	   	fixed3 rgt = normalize(fixed3(fwd.z, 0., -fwd.x));
	   	fixed3 up = cross(fwd, rgt);
			//Ray direction.
			fixed3 rd = normalize(fwd + FOV * uv.x*rgt + FOV * uv.y*up);
			//Raymarch
			fixed t = raymarch(ro, rd);
			// Rolling Plane color
			fixed3 sceneCol = fixed3(0.9,0.5,0.5);
			// The ray has effectively hit the surface, so light it up.
			if (t < FAR)
			{
				fixed3 sp = ro + rd * t;
				fixed3 sn = getNormal(sp);
				// Texture scale factor.
				const fixed tSize0 = 1. / 2.;
				// Light direction fixedtors.
				fixed3 ld = lp - sp;
				fixed3 ld2 = lp2 - sp;
				// Distance from respective lights to the surface point.
				fixed lDist = max(length(ld), 0.001);
				fixed lDist2 = max(length(ld2), 0.001);
				// Normalize the light direction fixedtors.
				ld /= lDist;
				ld2 /= lDist2;
				// Light attenuation, based on the distances above.
				fixed atten = 1. / (1. + lDist * lDist*0.025);
				fixed atten2 = 1. / (1. + lDist2 * lDist2*0.0025);
				// Diffuse lighting.
				fixed diff = max(dot(sn, ld), 0.0);
				fixed diff2 = max(dot(sn, ld2), 0.0);
				// Specular lighting.
				fixed spec = pow(max(dot(reflect(-ld, sn), -rd), 0.0), 8.);
				fixed spec2 = pow(max(dot(reflect(-ld2, sn), -rd), 0.0), 8.);
				// Fresnel term. Good for giving a surface a bit of a reflective glow.
				fixed fre = pow(clamp(dot(sn, rd) + 1., .0, 1.), 1.);
				// Darkening the crevices. Otherse known as cheap, scientifically-incorrect shadowing.
				fixed shading = 0.5 + 0.5;
				shading *= smoothstep(-.1, .15, 1.0);
				fixed3 rCol = 0.0;
				sceneCol += (rCol*(diff)+fixed3(.8, .95, 1)*spec*1.5)*atten;
				sceneCol += (rCol*(diff2)+fixed3(.8, .95, 1)*spec2*1.5)*atten2;
				sceneCol *= shading;
			}
			// Simple dark fog. It's almost black, but I left a speck of blue in there to account for
			sceneCol = lerp(sceneCol, fixed3(.90, .90, .0), smoothstep(0.,FAR, t * 2.1));
			return fixed4(sceneCol.x, sceneCol.y, sceneCol.z, 0.7);
		}
		ENDCG
		}
	}
}
