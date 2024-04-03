/*
 * Copyright (c) 2021, 117 <https://twitter.com/117scape>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include utils/misc.glsl
#include utils/texture_tiling.glsl

//#define OLD_WATER
//#define HOODER_WATER

#ifdef OLD_WATER
#include utils/legacy_water.glsl
#elif defined HOODER_WATER
#include utils/hooder_water.glsl
#else

//#define DEVELOPMENT_WATER_TYPE 7 // DEVELOPMENT OVERRIDE - ALSO SET IN SAMPLEWATER
// 1 = water
// 2 = flat water
// 3 = swamp water
// 4 = swamp water flat
// 5 = poison waste
// 6 = black tar flat
// 7 = blood water
// 8 = ice
// 9 = ice flat
// 10 = muddy water
// 11 = scar sludge
// 12 = abyss bile
// 13 = plain flat water --- #2 is color-matched to model-water in caves etc, while this one isn't
// 14 = dark blue water
// 15 = flat blood

float calculateFresnel(const vec3 I, const vec3 N, const float ior) {
    float cosi = dot(I, N);
    float etai = ior, etat = 1;
    if (cosi < 0) {
        etai = 1;
        etat = ior;
        return 1; // hide artifacts
    }

    float R0 = (etai - etat) / (etai + etat);
    R0 *= R0;
    return R0 + (1 - R0) * pow(1 - cosi, 5);
}

void sampleUnderwater(inout vec3 outputColor, WaterType waterType, float depth, float lightDotNormals);

vec4 sampleWater(int waterTypeIndex, vec3 viewDir)
{
    #ifdef DEVELOPMENT_WATER_TYPE
    waterTypeIndex = DEVELOPMENT_WATER_TYPE;
    #endif

    WaterType waterType = getWaterType(waterTypeIndex);

    // VARIABLES
    vec3 fragToCam = viewDir;
    vec4 d = vec4(0);
    vec3 I = -viewDir; // incident




















    // NORMALS STUFF
    float speed = .024;
    if(waterTypeIndex == 8 || waterTypeIndex == 9) // ice
    {
        speed = 0.00000001; // 0 speed bugs out the normals code, glacier speed is fine
    }
    else
    speed = 0.024;
    float waveSizeConfig = waterWaveSizeConfig / 100.f;
    float waveSpeedConfig = waterWaveSpeedConfig / 100.f;
    speed *= waveSpeedConfig;
    vec2 uv1 = worldUvs(26) - animationFrame(sqrt(11.) / speed * waterType.duration / vec2(-1, 4));
    vec2 uv2 = worldUvs(6) - animationFrame(sqrt(3.) / speed * waterType.duration * 1.5 /vec2(2, -1));

    // get diffuse textures
    vec3 n1 = linearToSrgb(texture(textureArray, vec3(uv1, MAT_WATER_NORMAL_MAP_1.colorMap)).xyz);
    vec3 n2 = linearToSrgb(texture(textureArray, vec3(uv2, MAT_WATER_NORMAL_MAP_2.colorMap)).xyz);

    // Normalize
    n1.xy = (n1.xy * 2 - 1);
    n2.xy = (n2.xy * 2 - 1);
    // Tangent space to world
    n1.z *= -1;
    n2.z *= -1;
    n1.xyz = n1.xzy;
    n2.xyz = n2.xzy;
    n1.y /= 0.225; // scale normals

    if(waterTypeIndex == 6 || waterTypeIndex == 8 || waterTypeIndex == 9 || waterTypeIndex == 12) // black tar, ice, ice flat, abyss bile
    {
        n1.y /= 0.3;
    }

    n1.y /= waveSizeConfig;
    n1 = normalize(n1);
    n2.y /= 0.8; // scale normals

    if(waterTypeIndex == 6 || waterTypeIndex == 8 || waterTypeIndex == 9 || waterTypeIndex == 12) // black tar, ice, ice flat, abyss bile
    {
        n2.y /= 0.3;
    }

    n2.y /= waveSizeConfig;
    n2 = normalize(n2);
    vec3 N = normalize(n1 + n2);
    N = normalize(vec3(n1.xy + n2.xy, n1.z + n2.z));















    // REFLECTIONS STUFF
    vec3 R = reflect(I, N);

    // Initialize the reflection with a fake sky reflection
    vec4 reflection = vec4(srgbToLinear(fogColor), calculateFresnel(N, fragToCam, 1.333));

    if (waterReflectionEnabled && abs(IN.position.y - waterHeight) < 32) { //only render reflection on water within a quarter-tile height of correct for the reflection texture
        // for now, assume the water is level
        vec3 flatR = reflect(I, vec3(0, -1, 0));

        // TODO: use actual viewport size here
        vec2 screenSize = vec2(textureSize(waterReflectionMap, 0));
        screenSize /= PLANAR_REFLECTION_RESOLUTION;
        vec2 texelSize = 1 / screenSize;
        vec2 uv = gl_FragCoord.xy / screenSize;

        float dist = length(IN.position - cameraPos);
        float distortionFactor = 1 - exp(-dist * .0004);

        vec3 uvX = normalize(cross(flatR * vec3(1, 0, 1), flatR));
        vec3 uvY = cross(uvX, flatR);
        float x = dot(R, uvX);
        float y = dot(R, uvY);
        int waterDepth = vTerrainData[0] >> 8 & 0x7FF;
        vec2 distortion = vec2(x, y) * 50 * distortionFactor;
        // TODO: Don't distort too close to the shore
        float shoreLineMask = 1.0 - dot(IN.texBlend, vec3(vColor[0].x, vColor[1].x, vColor[2].x));
        distortion *= 1.4 - (shoreLineMask *1.54); // safety factor to remove artifacts
        uv += texelSize * distortion;

        uv = clamp(uv, texelSize, 1 - texelSize);

        // This will be linear or sRGB depending on the linear alpha blending setting
        vec3 c = texture(waterReflectionMap, uv, -1).rgb;
//        c = textureBicubic(waterReflectionMap, uv).rgb;
        #if !LINEAR_ALPHA_BLENDING
        // When linear alpha blending is on, the texture is in sRGB, and OpenGL will automatically convert it to linear
        c = srgbToLinear(c);
        #endif

        // Swap out the fake sky reflection for a real planar reflection
        reflection.rgb = c;
    }

    // ALWAYS RETURN IN sRGB FROM THIS FUNCTION (been burned by this a couple times)



















    // FOAM STUFF
    #include WATER_FOAM
    vec4 foam = vec4(0);
    #if WATER_FOAM
        vec2 flowMapUv = worldUvs(15) + animationFrame(50 * waterType.duration);
        float flowMapStrength = 0.025;
        vec2 uvFlow = texture(textureArray, vec3(flowMapUv, waterType.flowMap)).xy;
        vec2 uv3 = vUv[0].xy * IN.texBlend.x + vUv[1].xy * IN.texBlend.y + vUv[2].xy * IN.texBlend.z + uvFlow * flowMapStrength;
        float foamMask = texture(textureArray, vec3(uv3, waterType.foamMap)).r;
        float foamAmount = 1 - dot(IN.texBlend, vec3(vColor[0].x, vColor[1].x, vColor[2].x));
        float foamDistance = 1;
        vec3 foamColor = vec3(0.5);
        foamColor = srgbToLinear(foamColor) * foamMask * (ambientColor * ambientStrength + lightColor * lightStrength);
        foamAmount = clamp(pow(1.0 - ((1.0 - foamAmount) / foamDistance), 3), 0.0, 1.0) * waterType.hasFoam;
        foamAmount *= 0.05;
        foamAmount *= waterFoamAmountConfig / 100.f;
        foam = vec4(foamColor, foamAmount);

        switch (waterTypeIndex) {
            case 3: // swamp water
            case 4: // swamp water flat
                foam.rgb *= vec3(1.3, 1.3, 0.4);
                break;
            case 5: // toxic waste
                foam.rgb *= vec3(0.7, 0.7, 0.7);
                break;
            case 6: // black tar
                foam.rgb *= vec3(1.0, 1.0, 1.0);
                break;
            case 7: // blood
            case 15: // blood flat
                foam.rgb *= vec3(1.6, 0.7, 0.7);
                break;
            case 8: // ice
                foam.rgb *= vec3(0.5, 0.5, 0.5);
                break;
            case 9: // ice flat
                foam.rgb *= vec3(0.5, 0.5, 0.5);
                break;
            case 10: // muddy water
                foam.rgb *= vec3(1.0, 0.5, 0.5);
                break;
            case 11: // scar sludge
                foam.rgb *= vec3(0.9, 1.2, 0.9);
                break;
            case 12: // abyss bile
                foam.rgb *= vec3(1.0, 0.7, 0.3);
                break;
            case 13: // plain water flat
                foam.rgb *= vec3(1);
                break;
        }
    #endif



















    // SCATTERING STUFF
    float cosUp = -N.y;

    vec3 C_ss = vec3(0, .32, .32); // water scatter color
    vec3 C_f = vec3(1); // air bubble color

    float k_1 = 20;  // ~tall wave scatter
    float k_2 = 0.01; // ~refraction scatter
    float k_3 = 0.008; // ~ambient scatter
    float k_4 = 0.1;  // ~air bubble scatter

    float P_f = .01; // density of air bubbles

    float H = (1 - pow(cosUp, 1.f)) * 50; // wave height
    //        float H = height / 50;

    vec3 omega_i = lightDir; // incoming = sun to frag
    vec3 omega_o = viewDir; // outgoing = frag to camera
    vec3 omega_h = normalize(omega_o - omega_i); // half-way between incoming and outgoing
    vec3 omega_n = IN.normal.xzy; // macro scale normal
    vec3 w_n = N; // presumably wave normal?
    omega_n = w_n;

    vec3 L_sun = lightColor * lightStrength;
    vec3 L_scatter = (
        k_1*H*pow(max(0, dot(omega_i, -omega_o)), 4.f) * pow(.5 - .5*dot(omega_i, omega_n), 3.f)
        + k_2*pow(max(0, dot(omega_o, omega_n)), 2.f)
    ) * C_ss*L_sun;
    L_scatter += k_3*max(0, dot(omega_i, w_n))*C_ss*L_sun + k_4*P_f*C_f*L_sun;













    // SPECULAR STUFF
    float specularGloss = waterType.specularGloss;
    float specularStrength = waterType.specularStrength;
    vec3 sunSpecular = pow(max(0, dot(R, lightDir)), specularGloss) * lightStrength * lightColor * specularStrength;

    // Point lights
    vec3 pointLightsSpecular = vec3(0);
    for (int i = 0; i < pointLightsCount; i++) {
        vec4 pos = PointLightArray[i].position;
        vec3 lightToFrag = pos.xyz - IN.position;
        float distanceSquared = dot(lightToFrag, lightToFrag);
        float radiusSquared = pos.w;
        // TODO: decide whether we want to restrict this. It doesn't really make sense to, but might help with performance
        // if (distanceSquared <= radiusSquared) {
            vec3 pointLightColor = PointLightArray[i].color;
            vec3 pointLightDir = normalize(lightToFrag);

            float attenuation = 1 - min(distanceSquared / radiusSquared, 1);
            pointLightColor *= attenuation * attenuation;

            vec3 pointLightReflectDir = reflect(-pointLightDir, N);
            pointLightsSpecular += pointLightColor * pow(max(0, dot(pointLightReflectDir, viewDir)), specularGloss) * specularStrength;
        // }
    }

    vec3 specular = sunSpecular + pointLightsSpecular;











    bool isOpaque = waterTransparencyType == 1 || waterType.isFlat;
    vec3 waterTypeColor = vec3(0);

    // Opaque setting or flat water
    if (isOpaque)
    {
        switch (waterTypeIndex) {
            case 2: // Flat cave water
                waterTypeColor += vec3(0.15, 0.37, 0.4);
                break;
            case 3: // Swamp water
            case 4: // Swamp water flat
                waterTypeColor += vec3(0.15, 0.37, 0.4);
                break;
            case 5: // toxic waste flat
                waterTypeColor += vec3(0.15, 0.37, 0.4);
                break;
            case 6: // black tar flat
                waterTypeColor += vec3(0.15, 0.37, 0.4);
                break;
            case 7: // Blood
            case 15: // Blood flat
                waterTypeColor += vec3(0.15, 0.37, 0.4);
                break;
            case 8:
            case 9: // Ice flat
                waterTypeColor += vec3(0.15, 0.37, 0.4);
                break;
            case 10: // Muddy water flat
                waterTypeColor += vec3(0.15, 0.37, 0.4);
                break;
            case 11: // Scar Sludge flat
                waterTypeColor += vec3(0.15, 0.37, 0.4);
                break;
            case 12: // abyss bile flat
                waterTypeColor += vec3(0.15, 0.37, 0.4);
                break;
            case 1: // water
            case 13: // plain water flat
            case 14: // dark blue water
                waterTypeColor += vec3(0);
                break;
        }
    }
    else // Transparent water
    {
        switch (waterTypeIndex) {
            case 3: // swamp water
                reflection.rgb *= 0.3; // dim reflection
                waterTypeColor += vec3(0.1, 0.1, 0.05); // inject color
                break;
            case 5: // toxic waste
                reflection.rgb *= 0.3; // dim reflection
                waterTypeColor += vec3(0.05, 0.05, 0.05); // inject color
                break;
             case 7: // blood
                reflection.rgb *= 0.75; // dim reflection
                waterTypeColor += vec3(0.16, 0, 0); // inject color
                break;
            case 8: // ice
                reflection.rgb *= 0.8; // dim reflection
                waterTypeColor += vec3(0.07, 0.07, 0.1); // inject color
                break;
            case 10: // muddy water
                reflection.rgb *= 0.4; // dim reflection
                waterTypeColor += vec3(0.13, 0.06, 0); // inject color
                break;
            case 11: // scar sludge
                reflection.rgb *= 0.9;
                waterTypeColor += vec3(0.3, 0.37, 0.3); // inject color
                break;
            case 12: // abyss bile
                reflection.rgb *= 0.9;
                waterTypeColor += vec3(0.42, 0.29, 0.075); // inject color
                break;
        }
    }

    // Hack for special water types, artificially adding light
    // For flat water, this could be improved by sampling underwater at a fixed vertical depth // how?
    L_scatter += waterTypeColor;







    // PUTTING IT ALL TOGETHER...

    // Begin with the reflection as a base
    vec4 dst = reflection;

    // Refraction makes up the remaining portion if we exclude specular highlights,
    // but with transparent water, we still want the underwater geometry to be visible.
    // Refraction + reflection = 1, so modulate scattering by 1 - reflection.a
    vec3 refraction = L_scatter * (1 - reflection.a);

    // Neither refraction nor specular make sense to blend in using alpha blending,
    // so we need a special way to blend in light additively, without unnecessarily
    // obscuring the underwater geometry.
    vec3 additionalLight = refraction + specular;

    // In theory, we could just add the light and be done with it, but since the color
    // will be multiplied by alpha during alpha blending, we need to divide by alpha to
    // end up with our target amount of additional light after alpha blending
    dst.rgb += additionalLight / dst.a;

    // The issue now is that or color may exceed 100% brightness, and get clipped.
    // To work around this, we can adjust the alpha component to let more of the light through,
    // and adjust our color accordingly. This necessarily causes the surface to become more opaque,
    // but since we're adding lots of light, this should have minimal impact on the final picture.
    float brightestColor = max(max(dst.r, dst.g), dst.b);
    // If the color would get clipped
    if (brightestColor > 1) {
        // Bring the brightest color back down to 1
        dst.rgb /= brightestColor;
        // And bump up the alpha to increase brightness instead
        dst.a *= brightestColor;
        // While not strictly necessary, we might as well clamp the alpha component in case it exceeds 1
        dst.a = min(1, dst.a);
    }

    dst.rgb += waterTypeColor; // Add color specific to each water type

    // If the water should be opaque, preserve the color brightness while adjusting alpha to 1
    if (isOpaque)
        dst /= dst.a;

    // Blend in foam at the very end as an overlay, using regular alpha blending
    //dst = mix(dst, vec4(foam.rgb, 1), foam.a);

    // Finally, apply gamma correction
    dst.rgb = linearToSrgb(dst.rgb);
    return dst;
}

void sampleUnderwater(inout vec3 outputColor, WaterType waterType, float depth, float lightDotNormals) {
    // underwater terrain
    outputColor = srgbToLinear(outputColor);
    outputColor.r *=0.7; // dirt texture looks unnaturally dry/bright/red in shallow water, remove some before further blending

    vec3 camToFrag = normalize(IN.position - cameraPos);
    float distanceToSurface = depth / camToFrag.y;
    float totalDistance = depth + distanceToSurface;
    int waterTypeIndex = vTerrainData[0] >> 3 & 0x1F;

    #ifdef DEVELOPMENT_WATER_TYPE
    waterTypeIndex = DEVELOPMENT_WATER_TYPE;
    #endif

    float lightPenetration = 0.5 + (waterTransparencyConfig / 44.444); // Scale from a range of 0% = 0.5, 100% = 2.75, 130% = 3.425

    // Exponential falloff of light intensity when penetrating water, different for each color
    vec3 extinctionColors = vec3(0);
    vec3 waterTypeExtinction = vec3(0);

    switch (waterTypeIndex) {
        case 1:
        case 2:
        case 8:
        case 13:
            waterTypeExtinction = vec3(1);
            break;
        case 3:
        case 4:
            waterTypeExtinction = vec3(2, 2, 2); // Light absorption for swamp water
            break;
        case 5:
            waterTypeExtinction = vec3(3, 3, 3); // Light absorption for toxic waste
            break;
        case 7:
            waterTypeExtinction = vec3(0.6, 30, 30); // Light absorption for blood
            break;
        case 10:
            waterTypeExtinction = vec3(1.5, 3, 6); // Light absorption for muddy water
            break;
        case 11:
            waterTypeExtinction = vec3(0.75, 1.125, 1.5); // Light absorption for scar sludge
            break;
        case 12:
            waterTypeExtinction = vec3(1, 1, 1); // Light absorption for abyss bile
            break;
    }

    extinctionColors.r = exp(-totalDistance * (0.003090 / lightPenetration) * waterTypeExtinction.r);
    extinctionColors.g = exp(-totalDistance * (0.001981 / lightPenetration) * waterTypeExtinction.g);
    extinctionColors.b = exp(-totalDistance * (0.001548 / lightPenetration) * waterTypeExtinction.b);

    if (shorelineCaustics && (waterTransparencyType ==0 || depth <=500)) {
        const float scale = 2.5;
        vec2 causticsUv = worldUvs(scale);
        causticsUv *= 0.75;
        const ivec2 direction = ivec2(1, -2);
        vec2 flow1 = causticsUv + animationFrame(17) * direction;
        vec2 flow2 = causticsUv * 1.5 + animationFrame(23) * -direction;
        vec3 caustics = sampleCaustics(flow1, flow2, .005);
        vec3 causticsColor = underwaterCausticsColor * underwaterCausticsStrength;
        if (waterTransparencyType == 1 && depth <= 500) // reduce caustics brightness for shallow opaque water
        {
            causticsColor *= 0.5;
        }
        if (waterTypeIndex == 8 || waterTypeIndex == 9) // ice
        {
            causticsColor *= 0;
        }
        outputColor *= 1 + caustics * causticsColor * extinctionColors * lightDotNormals * lightStrength * (waterCausticsStrengthConfig / 100.f);
    }

    outputColor = mix(vec3(0), outputColor, extinctionColors);
    outputColor = linearToSrgb(outputColor);
}
#endif
