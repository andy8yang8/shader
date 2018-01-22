Shader "Panda/Star"
{
	SubShader
	{
		Pass
		{
			ZWrite Off

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
                        /*
			fixed2 rotate2(in fixed2 p, fixed t)
			{
				fixed c = cos(t);
				fixed s = sin(t);
				p = fixed2(p.x*c - p.y*s, p.x*s + p.y*c);
				return p;
			}
                        */
			VertexOutput vert(VertexInput v)
			{
				VertexOutput o;
				o.pos = UnityObjectToClipPos(v.vertex);
				o.uv = v.uv;
				return o;
			}
			fixed4 frag(VertexOutput i) : SV_Target
			{
				fixed2 uv = (i.uv / 1) - .5;
				fixed t = _Time.y * .1 + ((.25 + .05 * sin(_Time.y * .1)) / (length(uv.xy) + .07)) * 2.2;
				fixed v1, v2, v3, s;
				s = v1 = v2 = v3 = 0.0;
				for (int i = 0; i < 90; i++)
				{
					fixed3 p = s * fixed3(uv, 0.0);
					//p = fixed3(rotate2(p.xy,t),p.z);
					p += fixed3(.22, .3, s - 1.5 - sin(_Time.y * .13) * .1);
					for (int i = 0; i < 8; i++)	p = abs(p) / dot(p,p) - 0.659;
					v1 += dot(p,p) * .0015 * (1.8 + sin(length(uv.xy * 13.0) + .5 - _Time.y * .2));
					v2 += dot(p,p) * .0013 * (1.5 + sin(length(uv.xy * 14.5) + 1.2 - _Time.y * .3));
					v3 += length(p.xy*10.) * .0003;
					s += .035;
				}
				float len = length(uv);
				v1 *= smoothstep(.7, .0, len);
				v2 *= smoothstep(.5, .0, len);
				v3 *= smoothstep(.9, .0, len);

				fixed3 col = fixed3
				(
					v3 * (1.5 + sin(_Time.y * .2) * .4),
					(v1 + v3) * .3,
					v2) + smoothstep(0.2, .0, len
				) * .85 + smoothstep(.0, .6, v3) * .3;
			return fixed4(min(pow(abs(col), fixed3(2.2,2.2,2.2)), 1.0), 1.0);
			}
			ENDCG
		}
	}
}
