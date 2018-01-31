Shader "simple raymarch no phong"
{
SubShader
{
	Tags{ "RenderType" = "Opaque" }

	Pass
	{
	CGPROGRAM

	#pragma vertex vert
	#pragma fragment frag
	#include "../noise/util3_clamped.cginc"
	#define MARCHING_STEPS 40
	#define MAX_DIST 100.0
	#define EPSILON 0.0001

	struct appdata
	{
		fixed4 vertex : POSITION;
		fixed2 uv : TEXCOORD0;
	};
	struct v2f
	{
		fixed4 pos : SV_POSITION;
		fixed2 uv : TEXCOORD0;
		fixed3 ray : TEXCOORD1;
	};
	/*
	float sdEllipsoid(in fixed3 p, in fixed3 r)
	{
		return (l  ength(p / r) - 1.0) * min(min(r.x, r.y), r.z);
	}
	*/
	//--------------------------------------------
	fixed map(fixed3 p) {
		//先用球体因为运算最简单
		const fixed3 elliR = fixed3(2., 1., 1.);
		const fixed R = 1.0;
		//从镜头出发(ro),已经步进到p点用来取样，向量的长度减掉球半径
		return length(p) - R;
		//return sdEllipsoid(p, elliR);
	}
	//---------------------------------------------
	fixed raymarch(fixed3 ro, fixed3 rd, fixed max)
	{
		fixed depth = 0.0; //从 ro 出发
		for (int i = 0; i < MARCHING_STEPS; i++)
		{
			//世界空间里沿着ray取样的坐标
			fixed3 p = ro + rd * depth;
			//Distance Field 的取样结果
			fixed dist = map(p);
			if (dist < EPSILON)
				//穿到“球”里了
				return depth;

			depth += dist;//这里有优化空间

			if (depth >= max)
				//也有
				return max;
		}

		return max;
	}
	//------------------------------------------------
	v2f vert(appdata v)
	{
		v2f o;
		o.pos = UnityObjectToClipPos(v.vertex);
		o.uv = v.uv;
		return o;
	}
	fixed4 frag(v2f i) : SV_Target
	{
		fixed2 uv = i.uv;
		//from 0~1 to -1~1
		uv = uv * 2.0 - 1.0;
		//Ray Direction (Z 取 1 是没有考虑FOV的简化)
		fixed3 rd = normalize(fixed3(uv,1));
		//Camera Origion
		fixed3 ro = fixed3(0.0,0.0,-2.0);//（x,y,z） 坐标在 （0,0,-2）
		fixed dist = raymarch(ro,rd, MAX_DIST);
		if (dist > MAX_DIST - EPSILON)
		{
			// Didn't hit anything
			return fixed4(0.0, 0.0, 0.0, 0.0);
		}
		fixed3 color = 1/(dist*dist);
		//fixed3 color = 0.5;
		return fixed4(color, 1.0);
	}
	ENDCG
	}
  }
}
