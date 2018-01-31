//https://www.alanzucconi.com/2017/08/30/fast-subsurface-scattering-2/
Shader "Custom/Skin" {
	Properties
  {
		_Color("SubColor", Color) = (0.9,0.0,0.0,1)
		_MainTex("Albedo (RGB)", 2D) = "white" {}
		[NoScaleOffset] _BumpMap("Bump Map", 2D) = "bump" {}
		_Metallic("Metallic", Range(0.0, 1.0)) = 0.0
		_Glossiness("Smoothness", Range(0.0, 1.0)) = 0.5
		_Distortion("Distortion", Range(0.0, 1.0)) = 0.0
		_Power("Power", Range(0.0, 1.0)) = 0.5
		//[NoScaleOffset] _SpecTex("Translucency (G) AO (A)", 2D) = "gray" {}
	}
	SubShader
  {
		Tags { "RenderType" = "Opaque" }
						LOD 200
						CGPROGRAM
		//#pragma surface surf SimpleSpecular
		#pragma surface surf StandardTranslucent fullforwardshadows
		#pragma target 3.0
		fixed4 _Color;
		half _Metallic;
		half _Glossiness;
		half _Distortion;
		half _Power;
		half _Scale;

		#include "UnityPBSLighting.cginc"
		inline fixed4 LightingStandardTranslucent(SurfaceOutputStandard s, fixed3 viewDir, UnityGI gi)
		{
			// Original colour
			fixed4 pbr = LightingStandard(s, viewDir, gi);
			// ...
			// Alter "pbr" here to include the new light
			// Calculate intensity of backlight (light translucent)
			// --- Translucency ---
			float3 L = gi.light.dir;
			float3 V = viewDir;
			float3 N = s.Normal;
			float3 H = normalize(L + N * _Distortion);
			float I = pow(saturate(dot(V, -H)), _Power) * _Scale;
			//final add
			pbr.rgb = pbr.rgb + gi.light.color * I;
			return pbr;
		}
		//gi is not used; just for Unity sanity
		void LightingStandardTranslucent_GI(SurfaceOutputStandard s, UnityGIInput data, inout UnityGI gi)
		{
			LightingStandard_GI(s, data, gi);
		}
		struct Input {
			float2 uv_MainTex;
		};
		sampler2D _MainTex;
		sampler2D _BumpMap;
		  //sampler2D _SpecTex;
		void surf(Input IN, inout SurfaceOutputStandard o) {
			// Albedo comes from a texture tinted by color
			fixed4 c = tex2D(_MainTex, IN.uv_MainTex) * _Color;
			o.Albedo = c.rgb;
			// Metallic and smoothness come from slider variables
			o.Metallic = _Metallic;
			o.Smoothness = _Glossiness;
			o.Alpha = c.a;
		}
		ENDCG
		}
		FallBack "Diffuse"
}
