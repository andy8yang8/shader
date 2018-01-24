Shader "hair"
{
	Properties{
	_MainTex("MainTex", 2D) = "white" {}
	}
		SubShader
	{
	Tags { "RenderType" = "Transparent" "Queue" = "Transparent" }

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

	//Variables
  sampler2D _MainTex;

	fixed curl = 20.0;
	// Created by inigo quilez - iq/2013
	// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
	fixed T = 1.;
	fixed hash(fixed n)
	{
		return frac(sin(n)*43758.5453);
	}
	fixed noise(in fixed2 x)
	{
		fixed2 p = floor(x);
		fixed2 f = frac(x);
		f = f * f*(3.0 - 2.0*f);
		fixed n = p.x + p.y*57.0;
		return lerp(lerp(hash(n + 0.0), hash(n + 1.0),f.x),
				   lerp(hash(n + 57.0), hash(n + 58.0),f.x),f.y);
	}
	fixed2 map(fixed2 p, in fixed offset)
	{

		fixed a = 0.;
		fixed b = 1.57;

		fixed alpha = (-p.y + 1.0) * 0.5;
		a = 3.14159*(0.5 + sin(p.y*curl)*0.167 + sin(_Time.y)*0.55*min(alpha,0.5));
		a = lerp(b,a,alpha);

		return fixed2(cos(a), sin(a));
	}
	VertexOutput vert(VertexInput v)
	{
		VertexOutput o;
		o.pos = UnityObjectToClipPos(v.vertex);
		o.uv = v.uv;
		//VertexFactory
		return o;
	}
	fixed4 frag(VertexOutput i) : SV_Target
	{
		fixed2 p = i.uv / 1. / 1;
		p.x += sin(_Time.y - 1.2)*0.05;
		fixed2 uv = -1.0 + 2.0*p;
		uv.x *= 1 / 1;
		fixed offset = T + i.uv.x / 1;
		fixed acc = 0.0;
		fixed3  col = fixed3(0.0,0.0,0.0);
		fixed ycut = sin(_Time.y);
		fixed2 uv2 = uv;
		fixed2 dir;
		[unroll(100)]
		for (int i = 0; i < 32; i++)
		{
			dir = map(uv, offset);
			fixed h = fixed(i) / 32.0;
			fixed w = 4.0*h*(1.0 - h);
			fixed3 ttt = w * tex2D(_MainTex, uv * fixed2(1, 1.0)).xyz;
			ttt *= lerp(fixed3(0.6, 0.7, 0.7), fixed3(1.0, 0.95, 0.9), 0.5 - 0.5*dot(reflect(fixed3(dir, 0.0), fixed3(1.0, 0.0, 0.0)).xy, fixed2(0.707, 0.707)));
			col += w * ttt;
			acc += w;
			col *= smoothstep(1.0, 0.0, (-clamp(uv.y, -1.0, -0.8) - 0.8) * 5.0);
			uv2 = uv;
			uv2.y -= ycut * (uv.x) *0.1;
			uv2.y -= abs(uv.x)*0.1;
			if (uv2.x < -0.5 || uv2.x > 0.5 || uv2.y < -0.44)
			{
				fixed f = hash(floor(uv.x*64.));
				f += hash(floor(uv.x*64.) + 1.);
				f += hash(floor(uv.x*64.) - 1.);
				f += hash(floor(uv.x*64.) + 2.)*0.5;
				f += hash(floor(uv.x*64.) - 2.)*0.5;
				if (uv2.y < -0.7)
				{
					acc += smoothstep(0., f*0.25, -(uv2.y - -0.7) / 0.3)*10.0;
				}
			}
			uv += 0.008*dir;
		}
		uv -= 0.008*dir;
		uv.y -= ycut * (uv.x) *0.5;
		if (abs(uv.x) > 0.5)
		{
			acc += smoothstep(0., 1.0, 2.0*clamp(abs(uv.x) - 0.5, 0., 0.5) / 0.5) * 1000.0;
		}
		if (uv2.y < -0.50)
		{
			float f = hash(floor(uv.x*64.));
			f += hash(floor(uv.x*64.) + 1.);
			f += hash(floor(uv.x*64.) - 1.);
			f += hash(floor(uv.x*64.) + 2.)*0.5;
			f += hash(floor(uv.x*64.) - 2.)*0.5;
			acc += smoothstep(0., pow(clamp(f*0.25, 0.0, 1.0), 0.5), -(uv2.y - -0.50) / 1.0)*1000.0;
		}
		col /= acc;
		float gg = dot(col, fixed3(0.333,0.333,0.333));
		fixed3 nor = normalize(fixed3(ddx(gg), 0.5, ddy(gg)));
		col += fixed3(0.4,0.4,0.4)*dot(nor, fixed3(0.7, 0.01, 0.7));
		fixed2 di = map(uv, offset);
		col *= 0.65 + 0.35*dot(di, fixed2(0.707,0.707));
		col *= 0.20 + 0.80*pow(4.0*p.x*(1.0 - p.x), 0.1);
		col *= 1.7;
		return fixed4(col, 1.0);
		}
		ENDCG
		}
	}
}
