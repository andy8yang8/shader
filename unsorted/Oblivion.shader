// Based on [SIG15] Oblivion
// by David Hoskins.
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
// The Oblivion drone. I love all the scenes that include these things.
// It takes bits from all over:-
// https://www.youtube.com/watch?v=rEby9OkePpg&feature=youtu.be
// These drones were the true stars of the film!

// You might need to rewind to sync the audio properly.

// Some info, if you want to know:-
// The camera is delayed when following the drone to make it feel hand held.
// The rendering layers consist of:-
// 1. Background, including sky, ground and shadow. Plus a check for a possible heat haze
//    to bend the ray, before beginning trace.
// 2. Anti-aliased drone ray-marching, which is traced from just in front to just behind it for speed.
// 3. Clouds, and fogging.
// 4. Foreground, for ID scanner

Shader "Oblivion"
{
	Properties{
	_MainTex("MainTex", 2D) = "white" {}
	_SecondTex("SecondTex", 2D) = "white" {}
	_ThirdTex("ThirdTex", 2D) = "white" {}
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
	fixed4 tangent : TANGENT;
	fixed3 normal : NORMAL;
	//VertexInput
	};


	struct VertexOutput {
	fixed4 pos : SV_POSITION;
	fixed2 uv : TEXCOORD0;
	//VertexOutput
	};

	//Variables
	sampler2D _MainTex;
	sampler2D _FourthTex;
	sampler2D _ThirdTex;
	sampler2D _SecondTex;

#define PI 3.14156
#define TAU 6.2831853071
#define MOD2 fixed2(443.8975,397.2973)
#define MOD3 fixed3(443.8975,397.2973, 491.1871)

const fixed2 add = fixed2(1.0, 0.0);
fixed3 sunDir = normalize(fixed3(-2.3, 3.4, -5.89));
const fixed3 sunCol = fixed3(1.0, 1.0, .9);
fixed2 gunMovement;
fixed3 drone;
fixed3 droneRots;
fixed scannerOn;
fixed4 dStack;
fixed4 eStack;
int emitionType = 0;

//----------------------------------------------------------------------------------------
// Action cue sheet, for easy manipulation...
#define cueINCLOUDS 0.0
#define cueFLYIN 14.0
#define cueFRONTOF cueFLYIN + 10.0
#define cueTHREAT cueFRONTOF + 5.
#define cueFLYOFF cueTHREAT + 19.0

//----------------------------------------------------------------------------------------
// A hash that's the same on all platforms...
//fixed3 hash32(fixed2 p)
//{
	//fixed temp = fract(mul(fixed3(p.xyx, p.xyx, p.xyx) , MOD3));
	//fixed3 p3 = fixed3(temp,temp,temp);
//	fixed3 p3 = frac(mul(fixed3(p, p, p), MOD3));
//	p3 += dot(p3.zxy, p3.yxz + 19.19);
//	return frac(fixed3(p3.x * p3.y, p3.x*p3.z, p3.y*p3.z));
//}

fixed3 hash32(fixed2 p) {
	p = frac(p * fixed2(5.3983, 5.4427));
	p += dot(p.yx, p.xy + fixed2(21.5351, 14.3137));

	fixed t = frac(p.x * p.y * 95.4337);
	return fixed3(t,t,t);
}

//----------------------------------------------------------------------------------------
fixed3 hash31(fixed p)
{
	fixed3 p3 = frac(mul(fixed3(p, p, p), MOD3));
   p3 += dot(p3.xyz, p3.yzx + 19.19);
   return frac(fixed3(p3.x * p3.y, p3.x*p3.z, p3.y*p3.z));
}

//----------------------------------------------------------------------------------------
fixed3 noise3(fixed n)
{
	fixed f = frac(n);
	n = floor(n);
	f = f * f*(3.0 - 2.0*f);
	return lerp(hash31(n), hash31(n + 1.0), f);
}

//----------------------------------------------------------------------------------------
fixed3 noise(in fixed2 x)
{
	fixed2 p = floor(x);
	fixed2 f = frac(x);
	f = f * f*(1.5 - f)*2.0;

	fixed3 res = lerp(lerp(hash32(p), hash32(p + add.xy),f.x),
			   lerp(hash32(p + add.yx), hash32(p + add.xx),f.x),f.y);
	return res;
}

//----------------------------------------------------------------------------------------
// CubeMap OpenGL clamping fix. Why do I have to do this?
/*
fixed3 cubeMap(in samplerCube sam, in fixed3 v, fixed size)
{
   fixed M = max(max(abs(v.x), abs(v.y)), abs(v.z));
   fixed scale = (fixed(size) - 1.) / fixed(size);
   if (abs(v.x) != M) v.x *= scale;
   if (abs(v.y) != M) v.y *= scale;
   if (abs(v.z) != M) v.z *= scale;
   return tex2D(sam, v).xyz;
}
*/
// Thanks to iq for the distance functions...
//----------------------------------------------------------------------------------------
fixed circle(fixed2 p, fixed s)
{
	return length(p) - s;
}
//----------------------------------------------------------------------------------------
fixed  sphere(fixed3 p, fixed s)
{
	return length(p) - s;
}

fixed prism(fixed3 p, fixed2 h)
{
	fixed3 q = abs(p);
	return max(q.x - h.y,max(q.z*0.6 + p.y*.5,-p.y) - h.x*0.5);
}

//----------------------------------------------------------------------------------------
fixed prismFlip(fixed3 p, fixed2 h)
{
	fixed3 q = abs(p);
	return max(q.x - h.y,max(q.z*.8 - p.y*.5,p.y) - h.x*0.5);
}

//----------------------------------------------------------------------------------------
fixed roundedSquare(fixed2 p, fixed2 b)
{
  fixed2 d = abs(p) - b;
  return min(max(d.x,d.y),0.0) + length(max(d,0.0));

}
//----------------------------------------------------------------------------------------
fixed roundedBox(fixed3 p, fixed3 b, fixed r)
{
	return length(max(abs(p) - b,0.0)) - r;
}
//----------------------------------------------------------------------------------------
fixed sMin(fixed a, fixed b, fixed k)
{

	fixed h = clamp(0.5 + 0.5*(b - a) / k, 0.0, 1.0);
	return lerp(b, a, h) - k * h*(1. - h);
}

//----------------------------------------------------------------------------------------
fixed2 rot2D(inout fixed2 p, fixed a)
{
	return cos(a)*p - sin(a) * fixed2(p.y, -p.x);
}

//----------------------------------------------------------------------------------------
fixed3 rot3DXY(in fixed3 p, in fixed2 a)
{
	fixed2 si = sin(a);
	fixed2 co = cos(a);
	p.xz = mul(p.xz, fixed2x2(co.y, -si.y, si.y, co.y));
	p.zy = mul(p.zy, fixed2x2(co.x, -si.x, si.x, co.x));
	return p;
}

//----------------------------------------------------------------------------------------
fixed boxMap(sampler2D sam, in fixed3 p, in fixed3 n)
{
	p = p * fixed3(.1, .03, .1);
	n = abs(n);
	fixed x = tex2D(sam, p.yz).y;
	fixed y = tex2D(sam, p.zx).y;
	fixed z = tex2D(sam, p.xy).y;
	return (x*n.x + y * n.y + z * n.z) / (n.x + n.y + n.z);
}

fixed tri(in fixed x) { return abs(frac(x) - .5); }
fixed3 tri3(in fixed3 p) { return fixed3(tri(p.z + tri(p.y*1.)), tri(p.z + tri(p.x*1.)), tri(p.y + tri(p.x*1.))); }

fixed triNoise3d(in fixed3 p, in fixed spd, fixed ti)
{
	fixed z = 1.1;
	fixed rz = 0.;
	fixed3 bp = p * 1.5;
	for (fixed i = 0.; i <= 3.; i++)
	{
		fixed3 dg = tri3(bp);
		p += (dg + spd);
		bp *= 1.9;
		z *= 1.5;
		p *= 1.3;

		rz += (tri(p.z + tri(p.x + tri(p.y)))) / z;
		bp += 0.14;
	}
	return rz;
}

fixed fogmap(in fixed3 p, in fixed d, fixed ti)
{
	p.xz *= .4;
	p.z += ti * 1.5;
	return max(triNoise3d(p*.3 / (d + 20.),0.2, ti)*1.8 - .7, 0.)*(smoothstep(0.,25.,p.y));
}
// Thanks to nimitz for the quick fog/clouds idea...
// https://www.shadertoy.com/view/4ts3z2
fixed3 clouds(in fixed3 col, in fixed3 ro, in fixed3 rd, in fixed mt, fixed ti)
{
	fixed d = 3.5;
	[unroll(100)]
for (int i = 0; i < 7; i++)
	{
		if (d > mt)break;
		fixed3  pos = ro + rd * d;
		fixed rz = fogmap(pos, d, ti);
		fixed3 col2 = (fixed3(.4,0.4,.4));
		col = lerp(col,col2,clamp(rz*smoothstep(d,d*1.86,mt),0.,1.));
		d *= 1.86;

	}
	return col;
}

//----------------------------------------------------------------------------------------
fixed4 numbers(fixed4 mat, fixed2 p)
{
	p.y *= 1.70;
	p.y += .32;
	fixed d;
	d = (roundedSquare(p + fixed2(1.4, -.25), fixed2(.02, .76)));
	d = min(d, (roundedSquare(p + fixed2(1.48, -1.04), fixed2(.1, .06))));

	fixed2 v = p;
	v.x -= v.y*.6;
	v.x = abs(v.x + .149) - .75;
	d = min(d, roundedSquare(v + fixed2(0.0, -.7), fixed2(.07, .4)));
	v = p;
	v.x -= v.y*.6;
	v.x = abs(v.x - .225) - .75;
	p.x = abs(p.x - .391) - .75;
	d = min(d, circle(p, .5));
	d = max(d, -circle(p, .452));
	d = max(d, -roundedSquare(v + fixed2(0., -.87), fixed2(.33, .9)));

	mat = lerp(mat, fixed4(.8,.8,.8,.8), smoothstep(0.2, .15, d));
	return mat;
}

//----------------------------------------------------------------------------------------
// Find the drone...
fixed mapDE(fixed3 p)
{
	p -= drone.xyz;
	p = rot3DXY(p, droneRots.xy);

	fixed d = sphere(p, 10.0);
	fixed3 v = p;
	v.xy = abs(v.xy);
	v.xy = rot2D(v.xy, -PI / 6.2);
	// Cross pieces...
	d = sMin(d, roundedBox(v - fixed3(0,0,-8), fixed3(4.9, .3, .5), 1.), 1.2);
	d = max(d, -roundedBox(v - fixed3(0,0,-8.5), fixed3(4.8, .3, 1.), 1.));

	// Centre cutout...
	//d = sMin(d, roundedBox(p-fixed3(0,0,-8.5), fixed3(1.3, 1.4, 1.5), .7), .4);
	d = max(d,-roundedBox(p - fixed3(0,0,-9.1), fixed3(2., 1.5, 4.0), .7));
	// Inside...
	d = min(d, sphere(p, 8.8));
	d = max(d, roundedBox(p, fixed3(6.5, 12, 12.0), .8));
	// Make back...
	d = sMin(d, prismFlip(p + fixed3(.0, -4.1, -8.1), fixed2(7., 4.7)), 1.);
	d = max(d, -prism(p + fixed3(.0, 6.4, -11.4), fixed2(8.0, 10.0)));
	d = min(d, sphere(p + fixed3(.0, 5.6, -6.2), 3.0));
	// Eye locations../
	d = min(d, sphere(v + fixed3(-3.5, .0, 7.4), 1.1));
	v = p;
	v.x = abs(v.x);
	d = sMin(d, roundedBox(v + fixed3(-4.2,-6.,-10.0), fixed3(1.1, .1, 4.5), 1.), 2.4);
	v = abs(p) - fixed3(gunMovement.x, .0, 0.);
	v.x -= p.z*.1*gunMovement.y;
	fixed d2 = sphere(v, 10.0);
	d2 = max(d2, -roundedBox(v, fixed3(6.55, 12, 12.0), .8));
	d = min(d2 ,d);
	d = min(d,roundedBox(v - fixed3(5.5, 3.5, 3.5), fixed3(2.3, .1, .1), .4));
	d = min(d,roundedBox(v - fixed3(5.5, .0, 5.), fixed3(2.4, .1, .1), .4));
	v = fixed3(abs(p.xy) - fixed2(gunMovement.x, .0), p.z);
	v.x -= p.z*.1*gunMovement.y;
	d = min(d, roundedBox(v - fixed3(8., 2.8, -6.5), fixed3(.3, 1., 3.), .2));
	d = min(d, roundedBox(v - fixed3(8., 2.3, -10.), fixed3(.2, .4, 1.2), .2));
	d = min(d, roundedBox(v - fixed3(8., 3.4, -10.), fixed3(.01, .01, 1.2), .4));
	d = max(d, -roundedBox(v - fixed3(8., 3.4, -10.4), fixed3(.01, .01, 1.2), .3));
	d = max(d, -roundedBox(v - fixed3(8., 2.3, -10.4), fixed3(.01, .01, 1.2), .3));
	d = min(d,  roundedBox(v - fixed3(8.55, 0, -4.5), fixed3(.4, .2, 1.), .4));
	d = max(d, -roundedBox(v - fixed3(8.65, 0, -4.5), fixed3(.0, .0, 2.), .34));
	return d;
}
//---------------------------------------------------------------------------
fixed bumpstep(fixed edge0, fixed edge1, fixed x)
{
	return 1.0 - abs(clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0) - .5)*2.0;
}
//----------------------------------------------------------------------------------------
// Find the drone's material...yes, it's IFtastic! :D
fixed4 mapCE(fixed3 p, fixed3 nor)
{
	fixed4 mat;
	p -= drone.xyz;
	p = rot3DXY(p, droneRots.xy);

	const fixed4 gunMetal = fixed4(.05, .05, .05,.3);
	fixed4 body = fixed4(.8, .8, .8,.4);

	fixed dirt1 = smoothstep(-.1, .5,boxMap(_SecondTex,p, nor))*.25 + .75;
	mat = body * dirt1;

	fixed d = sphere(p + fixed3(0,0,.5), 8.9);
	fixed d2;
	d = max(d, roundedBox(p, fixed3(6., 12, 11.0), .72));
	if (d < .0 || p.z > 14.5)
	{
		d = sphere(p - fixed3(-3.3 , 1.8, -8.1), .9);
		d2 = sphere(p - fixed3(3.1 , 1.7, -8.1), .5);
		// EyeCam...
		if (d < 0.0)
		{
			mat = fixed4(1., 0.03, 0.0, .7);
			emitionType = 1;
		}
else
// Scanner...
if (d2 < 0.0)
{
	d2 = d2 < -.015 ? max(-circle(fmod(p.xy - fixed2(3.185 , 1.78), .16) - .08, .085)*35.0, 0.0) : 1.0;
	mat = fixed4(.2 + scannerOn * .6, 0.2 + scannerOn * .75, 0.2 + scannerOn, .7*d2)*d2;

	emitionType = 2;
}
else
	mat = numbers(gunMetal, p.xy);
		// Do hex border line around numbers...
		p = abs(p);
		mat = p.x > p.y*.76 ? lerp(mat, fixed4(0.0,0.0,0.0,0.0), bumpstep(2.3, 2.4, p.x + p.y*.5)) : lerp(mat, fixed4(0.0,0.0,0.0,0.0), bumpstep(1.82, 1.92, p.y));
		return mat;
}

	// Gun placements and carriers...
   fixed3 v = p;

   //v.yz = rot2D(p.yz, gunMovement.x);
   v = abs(v) - fixed3(gunMovement.x, .0, 0.);
   v.x -= p.z*.1*gunMovement.y;
   d2 = sphere(v, 10.0);
   d2 = max(d2, -roundedBox(v, fixed3(6.55, 12, 4.0), 1.1));

   d = min(d2, d);
   d2 = min(d,	roundedBox(v - fixed3(5.5, 3.5, 3.5), fixed3(2.3, .1, .1), .4));
   //d2 = min(d2,roundedBox(v-fixed3(5.5, .0, 3.7), fixed3(2.3, .1, .1), .4));
   d2 = min(d2, sphere(v - fixed3(5., .0, 3.7), 3.8));
   if (d2 < d) mat = fixed4(.0, .0, .0, 6.);
   //return mat;

   v = fixed3(abs(p.x) - gunMovement.x, p.yz);
   v.x -= p.z*.1*gunMovement.y;
   fixed dirt = (smoothstep(-.1, .5,boxMap(_SecondTex,v, nor))*.2 + .8);
   body = body * dirt;

   v = fixed3(abs(p.xy) - fixed2(gunMovement.x, .0), p.z);
   v.x -= p.z*.1*gunMovement.y;

   if (v.x > 7.4)  mat = lerp(body, gunMetal, smoothstep(2.5, 2.3, v.y))*dirt;
   d2 = roundedBox(v - fixed3(8., 2.3, -10.5), fixed3(.4, 1.6, 1.5), .2);
   //if ( d2 < 0.1)  mat = gunMetal*dirt;
   mat = lerp(mat, gunMetal*dirt, clamp(-d2 * 10.0, 0.0, 1.0));

   d = sphere(p + fixed3(.0, 5.6, -6.2), 3.2);
   if (d < 0.0)
   {
	   mat = fixed4(0,0,0,0);
	   emitionType = 3;
   }

   return mat;
}
//----------------------------------------------------------------------------------------
fixed shadow(in fixed3 ro, in fixed3 rd)
{
	fixed res = 1.0;
	fixed t = .2;
	for (int i = 0; i < 12; i++)
	{
		fixed h = mapDE(ro + rd * t);
		if (h < -2.) break;
		res = min(10.*h / t, res);
		t += h + .2;
	}
	return max(res, .3);
}
//----------------------------------------------------------------------------------------
fixed SphereRadius(in fixed t)
{
	t = t * .003 + .01;
	return min(t,256.0 / 1);
}
//----------------------------------------------------------------------------------------
void rayMarch(fixed3 pos, fixed3 dir)
{
	// Efficiently start the ray just in front of the drone...
	fixed l = max(length(drone - pos) - 14.2, .0);
	fixed d = l;
	l += 23.;// ...and end it just after
	int hits = 0;
	// Collect 4 of the closest scrapes on the tracing sphere...
	for (int i = 0; i < 55; i++)
	{
		// Leave if it's gone past the drone or when it's found 7 stacks points...
		if (d > l || hits == 6) break;
		fixed3 p = pos + dir * (d);
		fixed r = SphereRadius(d);
		fixed de = mapDE(p);
		// Only store the closest ones (roughly), which means we don't
		// have to render the 8 stack points, just the most relavent ones.
		// This also prevents the banding seen when using small stacks.
		if (de < r &&  de < eStack.x)
		{
			// Rotate the stack and insert new value!...
			dStack = dStack.wxyz; dStack.x = d;
			eStack = eStack.wxyz; eStack.x = de;
			hits++;
		}
		d += de * .9;
	}
	return;
}

//----------------------------------------------------------------------------------------
fixed3 normal(in fixed3 pos, in fixed r)
{
	fixed2 eps = fixed2(r*1., 0.0);
	fixed3 nor = fixed3(
		mapDE(pos + eps.xyy) - mapDE(pos - eps.xyy),
		mapDE(pos + eps.yxy) - mapDE(pos - eps.yxy),
		mapDE(pos + eps.yyx) - mapDE(pos - eps.yyx));
	return normalize(nor);
}

//----------------------------------------------------------------------------------------
fixed terrain(in fixed2 q)
{
	q *= .5;
	q += fixed2(4.,4.);
	fixed h = smoothstep(0., 0.7, tex2Dlod(_SecondTex,(0.023*q).x).x)*6.0;//<<
	h += smoothstep(0., 0.7,       tex2Dlod(_ThirdTex,(0.03*q).x).y)*3.0;
	//h +=  smoothstep( 0., 1., tex2D( _SecondTex, .01*q, 00.0 ).y )*1.0;
	return h;
}

//----------------------------------------------------------------------------------------
fixed3 skyUpper(in fixed3 rd)
{
	fixed3  sky;
	fixed f = pow(max(rd.y, 0.0), .5);
	sky = lerp(fixed3(.45, .5, .6), fixed3(.7, .7, .7), f);
	fixed sunAmount = pow(max(dot(rd, sunDir), 0.0), 8.0);
	sky = sky + sunCol * sunAmount*.5;
	rd.xz = rd.zx; rd.y -= .05;
	sky -= fixed3(.65, .67, .75);
	//- cubeMap(_FourthTex, rd, 64.0).xyz)*.5;//<<<<<<<<<

return clamp(sky, 0.0, 1.0);
}

//----------------------------------------------------------------------------------------
fixed3 fogIt(in fixed3 col, in fixed3 sky, in fixed d)
{
	return lerp(col, sky, clamp(1.0 - exp(-d * 0.001), 0.0, 1.0));
}

//----------------------------------------------------------------------------------------
fixed3 ground(fixed3 sky, in fixed3 rd, in fixed3 pos)
{

	if (rd.y > .0) return sky;

	fixed d = (-20.0 - pos.y) / rd.y;
	fixed2 p = pos.xz + rd.xz * d;

	fixed3 tex1 = tex2D(_SecondTex, p*.1).xyz;
	fixed3 tex2 = tex2D(_ThirdTex, p*.0004).yyx*fixed3(1.0, .8, .8);

	fixed3 gro = fixed3(1.,1.,1.);

	d -= 20.0;
	fixed a = .0004*d*d;

	fixed3 nor = fixed3(0.0,		    terrain(p), 0.0);
	fixed3 v2 = nor - fixed3(a,		terrain(p + fixed2(a, 0.0)), 0.0);
	fixed3 v3 = nor - fixed3(0.0,		terrain(p + fixed2(0.0, a)), -a);
	nor = cross(v2, v3);
	nor = normalize(nor);
	gro = lerp(tex1, tex2, nor.y*.8);
	fixed sha = shadow(fixed3(p.x, 0.0, p.y),  sunDir);
	fixed z = max(dot(nor, sunDir), 0.1);
	if (dStack[0] < 0.0) dStack[0] = d;
	fixed3 col = gro * z*sha;

	return col = fogIt(col, sky, d);
}



//----------------------------------------------------------------------------------------
// This is also used for the camera's delayed follow routine.
// Which make the scene more dramitic because it's a human camera operator!
fixed3 dronePath(fixed ti)
{
	fixed3 p = fixed3(-2030, 500, 2400.0);
	p = lerp(p, fixed3(-2030, 500, 2000.0),	 	smoothstep(cueINCLOUDS, cueFLYIN, ti));
	p = lerp(p, fixed3(-30.0, 18.0, 300.0),		smoothstep(cueFLYIN, cueFLYIN + 4.0, ti));
	p = lerp(p, fixed3(-35.0, 25.0, 10.0), 		smoothstep(cueFLYIN + 2.0,cueFLYIN + 8.0, ti));
	p = lerp(p, fixed3(30.0, 0.0, 15.0), 			smoothstep(cueFRONTOF + .5,cueFRONTOF + 2.5, ti)); //../ Move to front of cam.

	p = lerp(p, fixed3(0.0, 8.0, .0), 				smoothstep(cueTHREAT, cueTHREAT + .5, ti)); 	// ...Threaten
	p = lerp(p, fixed3(0.0, 8.0, -4.0), 			smoothstep(cueTHREAT + 2.0, cueTHREAT + 2.3, ti)); 	// ...Threaten
	p = lerp(p, fixed3(0.0, 8., -12.0), 			smoothstep(cueTHREAT + 3.0, cueTHREAT + 3.3, ti)); 	// ...Threaten

	p = lerp(p, fixed3(0.0, 110.0, 0.0), 			smoothstep(cueFLYOFF,cueFLYOFF + 1.5, ti)); // ...Fly off
	p = lerp(p, fixed3(4000.0, 110.0, -4000.0), 	smoothstep(cueFLYOFF + 2.6,cueFLYOFF + 10.0, ti));
	return p;
}

//----------------------------------------------------------------------------------------
fixed3 droneRotations(fixed ti)
{
	fixed3 a = fixed3(0,0,0);


	a.x = lerp(a.x, .2, smoothstep(cueFLYIN - 3.0,cueFLYIN - 1.5, ti));
	a.x = lerp(a.x, .0, smoothstep(cueFLYIN - 1.5,cueFLYIN, ti));

	a.y = lerp(a.y, -.8,smoothstep(cueFLYIN - 1.5,cueFLYIN, ti));

	a.x = lerp(a.x, .2,smoothstep(cueFLYIN + 2.0,cueFLYIN + 4.0, ti));
	a.x = lerp(a.x, 0.,smoothstep(cueFLYIN + 4.0,cueFLYIN + 6., ti));

	a.y = lerp(a.y, 0.0, smoothstep(cueFLYIN + 3.0,cueFLYIN + 4.4, ti));
	a.x = lerp(a.x, .1,smoothstep(cueFLYIN + 7.0,cueFLYIN + 7.8, ti));
	a.x = lerp(a.x, 0.,smoothstep(cueFLYIN + 7.8,cueFLYIN + 8.3, ti));

	a.y = lerp(a.y, -1.5,smoothstep(cueFRONTOF,cueFRONTOF + .5, ti));// ..Turn to go right, infront
	a.y = lerp(a.y, .6, 	smoothstep(cueFRONTOF + 3.,cueFRONTOF + 4.5, ti));

	a.y = lerp(a.y, .0,  smoothstep(cueTHREAT,cueTHREAT + .5, ti));

	a.x = lerp(a.x, -.28,smoothstep(cueTHREAT, cueTHREAT + .3, ti)); // ...Threaten

	a.x = lerp(a.x, 0.0, smoothstep(cueFLYOFF - 2.0, cueFLYOFF, ti)); // Normalise position, relax!
	a.x = lerp(a.x, -0.5,smoothstep(cueFLYOFF, cueFLYOFF + .2, ti)); 	// ...Fly off
	a.x = lerp(a.x, 0.0, smoothstep(cueFLYOFF + .2, cueFLYOFF + .7, ti));

	a.y = lerp(a.y, -.78,smoothstep(cueFLYOFF + 2., cueFLYOFF + 2.3, ti));

	scannerOn = smoothstep(cueTHREAT + 4.0,cueTHREAT + 4.2, ti)* smoothstep(cueTHREAT + 11.5,cueTHREAT + 11.2, ti);
	a.z = sin(ti*2.) * scannerOn;

	return a;
}

//----------------------------------------------------------------------------------------
fixed2 droneGunAni(fixed ti)
{
	fixed2 a;
	fixed mov = smoothstep(cueTHREAT + .5, cueTHREAT + 1.5, ti);
	mov = mov * smoothstep(cueFLYOFF - 1., cueFLYOFF - 3.0, ti);
	mov = mov * 3.1 - 1.4;
	a.x = (sin(mov) + 1.0)*1.5;
	a.y = smoothstep(.3,.7,sin(mov))*3.0;
	return a;
}

//----------------------------------------------------------------------------------------
fixed3 cameraAni(fixed ti)
{
	fixed3 p;
	p = lerp(drone - fixed3(0.0,0.0, 10.0), drone - fixed3(0.0,0.0, 20.0), smoothstep(cueINCLOUDS,cueINCLOUDS + 2.0, ti));
	p = lerp(p, drone - fixed3(17.0,-14.0, 35.0), smoothstep(cueINCLOUDS + 2.0,cueFLYIN - 3.0, ti));

	p = lerp(p, fixed3(0.0, 0.0, -28.0), step(cueFLYIN, ti));
	p = fixed3(p.xy, lerp(p.z, -40.0, smoothstep(cueTHREAT,cueTHREAT + 4.0, ti)));
	return p;
}

//----------------------------------------------------------------------------------------
fixed overlay(fixed3 p, fixed3 dir)
{
	fixed r = 0.0;
	fixed3 pos = drone.xyz + fixed3(3.25, -.48, -8.0);
	fixed3 v = p - pos;
	fixed3 n = fixed3(0.0, 1., 0.0);
	n.zy = rot2D(n.zy, droneRots.z);
	n = normalize(n);
	fixed d = -dot(n, v) / dot(n, dir);
	p = p + dir * d - pos;

	if (p.z < .0 && p.z > -20.)
	{
		fixed d = abs(p.z) - abs(p.x) + .4;
		r = step(.3, d)*.3;
		r += smoothstep(-.3, -.2,p.x) * smoothstep(0., -.2, p.x)*r;
		r += smoothstep(.3, .2,p.x) * smoothstep(0.0, .2, p.x)*r;
		r += smoothstep(0.1, .2, d) * smoothstep(0.4, .2, d);
	}
	r += smoothstep(0.3, 0.0,abs(droneRots.z - .4))*1.5;

	return r;
}

//----------------------------------------------------------------------------------------
void heatHaze(fixed3 p, inout fixed3 dir, fixed t)
{
	if (t < cueFLYIN) return;
	fixed r = 0.0;
	fixed3 pos = fixed3(0.0, -4.8, 7.);
	if (drone.y < 20.0)
		pos.y += smoothstep(-.90, .5,droneRots.y)*smoothstep(.9, 0.5,droneRots.y)*-8.0;
	pos.zx = rot2D(pos.zx, droneRots.y);
	pos += drone.xyz;
	fixed3 v = p - pos;
	fixed3 n = fixed3(0.0, 0., 1.0);

	n = normalize(n);
	fixed d = -dot(n, v) / dot(n, dir);
	p = p + dir * d - pos;

	if (p.y < .0 && p.y > -30.)
	{
		fixed l = abs(p.y) - abs(p.x*(1.1)) + 8.0;
		r = smoothstep(.0, 14., l);
		//p.xy *= fixed2(.5,.9);
		t *= 23.0;
		dir += r * (noise(p.xy*.8 + fixed2(0.0,t)) - .5)*.001 / (.07 + (smoothstep(10.0, 2500.0, d)*20.0));
	}
}

//----------------------------------------------------------------------------------------
fixed3 cameraLookAt(in fixed2 uv, in fixed3 pos, in fixed3 target, in fixed roll)
{
	fixed3 cw = normalize(target - pos);
	fixed3 cp = fixed3(sin(roll), cos(roll),0.0);
	fixed3 cu = normalize(cross(cw,cp));
	fixed3 cv = normalize(cross(cu,cw));
	return normalize(-uv.x*cu + uv.y*cv + 2.*cw);
}

//----------------------------------------------------------------------------------------




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

	fixed2 xy = i.uv / 1;
	fixed2 uv = (xy - .5)*fixed2(1 / 1, 1)*2.0;
	// Multiply this time to speed up playback, but remember to do the sound as well!
   fixed ti = fmod(_Time.y, 57.);
   //fixed ti = fmod(_Time.y, 5.)+cueFRONTOF;	// ...Test cues..
   //fixed ti = fmod(_Time.y, 15.0)+cueTHREAT+1.0;
   //fixed ti = fmod(_Time.y, 5.)+cueFLYIN;
   //fixed ti = fmod(_Time.y, 5.)+cueFLYOFF;

   //---------------------------------------------------------
   // Animations...
   drone = dronePath(ti);
   droneRots = droneRotations(ti);
   fixed3 camPos = cameraAni(ti);
   gunMovement = droneGunAni(ti);
   fixed t = smoothstep(cueTHREAT, cueTHREAT + .5, ti) *smoothstep(cueTHREAT + 15.5, cueTHREAT + 14.7, ti);

   fixed e = -droneRots.y + t * tex2D(_MainTex, fixed2(.3, ti*.02)).x*.25 - .22;
   e += tex2D(_MainTex, fixed2(.4, ti*.005)).x*.5 - .35;
   fixed3 eyeCam = normalize(fixed3(0.3, -.4*t,  -1.0));
   eyeCam.xz = rot2D(eyeCam.xz, e);

   //---------------------------------------------------------
   fixed3 tar = dronePath(ti - .25);
   // Cameraman gets shaky when the drone is close...oh no...
   fixed l = 30.0 / length(tar - camPos);
   tar += (noise3(ti*4.0) - .5)*l;
   fixed3 dir = cameraLookAt(uv, camPos, tar, 0.0);


   heatHaze(camPos, dir, ti);
   //--------------------------------------------------------
   // Reset and fill the render stack through ray marching...
   dStack = fixed4(-1,-1,-1,-1);
   eStack = fixed4(1000.0,1000.0,1000.0,1000.0);
   rayMarch(camPos, dir);

   //---------------------------------------------------------
   // Use the last stacked value to do the shadow, seems to be OK, phew!...
   fixed lg = dStack[0];
   fixed3 p = camPos + dir * lg;
   fixed sha = shadow(p, sunDir);
   fixed3 sky = skyUpper(dir);
   //---------------------------------------------------------
   // Render the stack...
   fixed alphaAcc = .0;
   fixed3 col = fixed3(0,0,0);
   fixed spe;
   for (int i = 0; i < 4; i++)
   {
	   fixed d = dStack[i];
	   if (d > 0.0)
	   {
		   fixed de = eStack[i];
		   fixed s = SphereRadius(d);
		   fixed alpha = max((1.0 - alphaAcc) * min(((s - de) / s), 1.0),0.0);

		   fixed3 p = camPos + dir * d;
		   fixed3  nor = normal(p, s);
		   fixed4  mat = mapCE(p, nor);
		   fixed amb = abs(nor.y)*.6; amb = amb * amb;
		   fixed temp = max(dot(sunDir, nor), 0.0);
		   fixed3 c = mul(mat.xyz , fixed3(temp,temp,temp)) + amb * mat.xyz;
		   spe = pow(max(dot(sunDir, reflect(dir, nor)), 0.0), 18.0);

		   if (emitionType != 0)
		   {
			   if (emitionType == 1)
			   {
				   s = cos(pow(max(dot(eyeCam, nor), 0.0), 4.4)*9.0)*.14;
				   s += pow(abs(dot(eyeCam, nor)), 80.)*18.0;
				   c *= max(s, 0.0);
			   }

			   //if (emitionType == 3)
			   //{
			//	   fixed3 dp = p - drone;
			//	   s = smoothstep(.0, -.1, nor.y) * smoothstep(-1.0, -.3, nor.y);
			//	   fixed tmp = (smoothstep(-5.8, -5., dp.y) * smoothstep(-4.8, -5., dp.y))*.1;
			//	   c = fixed3(tmp,tmp,tmp);
			//	   float g = abs(sin((atan2(nor.x, -nor.z))*TAU + ti * 33.0)) + .2;//<<<
			//	   c += s * mul((texture(_SecondTex, p.xy*fixed2(.04, .01) + fixed2(0.0, ti)).xyy) , fixed3(1.5, 2.3, 3.5)*g);
				   //c += s * (texture(iChannel2, p.xy*vec2(.04, .01) + vec2(0.0, ti)).xyy) * vec3(1.5, 2.3, 3.5)*g;
			//	   alpha *= smoothstep(-9., -4.5, dp.y) - g * smoothstep(-4.5, -10., dp.y)*.2;

			  // }

			   sha = 1.0;
		   }

		   c += sunCol * spe * mat.w;


		   col += c = fogIt(c *sha, sky, d)* alpha;
		   alphaAcc += alpha;
	   }
   }
   //---------------------------------------------------------
   // Back drop...
   fixed3 gro = ground(sky, dir, camPos);
   col = lerp(col, gro, clamp(1.0 - alphaAcc, 0.0, 1.0));
   if (dStack[0] < 0.0) dStack[0] = 4000.0;
   col = clouds(col, camPos, dir, dStack[0], ti);
   // Overlay...
   float scan = overlay(camPos, dir)*scannerOn;
   col = min(col + fixed3(scan*.6, scan*.75, scan), 1.0);
   //---------------------------------------------------------
   // Post effects...
   col = col * 0.5 + 0.5*col*col*(3.0 - 2.0*col);			// Slight contrast adjust
   col = sqrt(col);											// Adjust Gamma
															// I can't decide if I like the added noise or not...
															//col = clamp(col + hash32(xy + ti)*.11, 0.0, 1.0); 					// Random film noise

   col *= 1.6 + 0.4*pow(50.0*xy.x*xy.y*(1.0 - xy.x)*(1.0 - xy.y), 0.2);	// Vignette
   col *= smoothstep(0.0, .5, ti)*smoothstep(58.0, 53., ti);
   return fixed4(col, 1.0);
   }
   ENDCG
   }
	}
}
