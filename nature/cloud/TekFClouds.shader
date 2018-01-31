Shader "ShaderMan/CLOUD_TEKF"
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
	#include "UnityCG.cginc"

	struct VertexInput {
	 fixed4 vertex : POSITION;
	 fixed2 uv : TEXCOORD0;
	};
	struct VertexOutput {
	 fixed4 pos : SV_POSITION;
	 fixed2 uv : TEXCOORD0;
	};

	//Variables
  //float4 _iMouse;
  sampler2D _MainTex;

  // Created by Ben Weston - 2013
  // License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
  fixed2 glFragCoord;

  fixed noise(in fixed3 x)
  {
   fixed3 p = floor(x);
   fixed3 f = frac(x);
   f = f * f*(3.0 - 2.0*f);
	  // there's an artefact because the y channel almost, but not exactly, matches the r channel shifted (37,17)
	   // this artefact doesn't seem to show up in chrome, so I suspect firefox uses different tex2D compression.
	 fixed2 uv = (p.xy + fixed2(37.0,17.0)*p.z) + f.xy;
	 fixed2 rg = tex2Dlod(_MainTex,float4((uv + 0.5) / 256.0, 0.0 ,0)).yx;
	 return lerp(rg.x, rg.y, f.z);
 }

 fixed4 map(fixed3 p)
 {
	 fixed den = -1.0 - (abs(p.y - 0.5) + 0.5) / 2.0;

	// clouds
	 fixed f;
	 fixed3 q = p * .5 - fixed3(0.0,0.0,1.5)*_Time.y + fixed3(sin(0.7*_Time.y),0,0);
	 f = 0.50000*noise(q); q = q * 2.02 - fixed3(0.0,0.0,0.0)*_Time.y;
	 f += 0.25000*noise(q); q = q * 2.03 - fixed3(0.0,0.0,0.0)*_Time.y;
	 f += 0.12500*noise(q); q = q * 2.01 - fixed3(0.0,0.0,0.0)*_Time.y;
	 f += 0.06250*noise(q); q = q * 2.02 - fixed3(0.0,0.0,0.0)*_Time.y;
	 f += 0.03125*noise(q);
   den = clamp(den + 4.0*f, 0.0, 1.0);
   fixed3 col = lerp(fixed3(1.0, 1.0, 1.0), fixed3(0.6,0.5,0.4), den*.5);// + 0.05*sin(p);
	 return fixed4(col, den*.7);
}
const fixed3 sunDir = fixed3(-1,.2,-1);
fixed testshadow(fixed3 p, fixed dither)
{
	fixed shadow = 1.0;
	fixed s = 0.0; // this causes a problem in chrome: .05*dither;
	for (int j = 0; j < 5; j++)
	{
		fixed3 shadpos = p + s * sunDir;
		shadow = shadow - map(shadpos).a*shadow;
		s += .05;
	}
	return shadow;
}

fixed3 raymarch(in fixed3 ro, in fixed3 rd, fixed2 uv)
{
	fixed4 sum = fixed4(0 , 0 , 0 , 0);

	fixed t = 0.0;

	// dithering	//<<<<<<<<<<<<<<<<
	fixed dither =
		//0.0;
	tex2D(_MainTex,uv.xy / _ScreenParams.x).x;
	t += 0.1*dither;

[unroll(100)]
for (int i = 0; i < 65; i++)
	{
		if (sum.a > 0.99) continue;
		fixed3 pos = ro + (t + .2*t*t)*rd;
		fixed4 col = map(pos);
		fixed shadow = testshadow(pos, dither);//<<
		col.xyz *= lerp(fixed3(0.4,0.47,0.6), fixed3(1.0,1.0,1.0), shadow);
		col.rgb *= col.a;
		sum = sum + col * (1.0 - sum.a);
		t += 0.1;
	}

	fixed4 bg = lerp(fixed4(.3,.4,.5,0), fixed4(.5,.7,1,0), smoothstep(-.4,.0,rd.y)); // sky/ocean

	/*// floor
	if ( rd.y < -.2 )
	{
		fixed3 pos = ro + rd*(ro.y+1.0)/(-rd.y);

		fixed shadow = testshadow(pos+sunDir/sunDir.y, dither);
		bg.xyz = lerp( fixed3(0,0,0), fixed3(.5,.5,.5), shadow*.8+.2 );
	}*/

	sum += bg * (1.0 - sum.a);

	return clamp(sum.xyz, 0.0, 1.0);
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

	glFragCoord = i.uv;

	fixed2 q = i.uv / 1;
	fixed2 p = -1.0 + 2.0*q;
	p.x *= 1 / 1;

	fixed2 mo =
  // _iMouse.xy / 1;
	//if (_iMouse.w <= 0.00001)
   fixed2(0.5,0.5);

	// camera
	fixed3 ro = 6.0*normalize(fixed3(cos(3.0*mo.x + .0), 1.0 - 1.0*(mo.y + .0), sin(3.0*mo.x + .0)));
	fixed3 ta = fixed3(0.0, 1.0, 0.0);
	fixed cr = 0.15*cos(0.7*_Time.y);

	// shake
	ro += 0.02*(-1.0 + 2.0*tex2D(_MainTex, _Time.y*fixed2(0.010,0.014)).xyz);
	ta += 0.02*(-1.0 + 2.0*tex2D(_MainTex, _Time.y*fixed2(0.013,0.008)).xyz);

	// build ray
	fixed3 ww = normalize(ta - ro);
	fixed3 uu = normalize(cross(fixed3(sin(cr),cos(cr),0.0), ww));
	fixed3 vv = normalize(cross(ww,uu));
	fixed3 rd = normalize(p.x*uu + p.y*vv + 2.0*ww);

	// raymarch
	fixed3 col = raymarch(ro, rd, glFragCoord);

	// contrast and vignetting
	col = col * 0.5 + 0.5*col*col*(3.0 - 2.0*col);
	col *= 0.25 + 0.75*pow(16.0*q.x*q.y*(1.0 - q.x)*(1.0 - q.y), 0.1);

	return fixed4(col, 1.0);

	}
	ENDCG
	}
	}
}
