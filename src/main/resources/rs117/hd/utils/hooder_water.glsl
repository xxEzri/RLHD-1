#include utils/texture_tiling.glsl

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

vec4 sampleWater(int waterTypeIndex, vec3 viewDir) {
    WaterType waterType = getWaterType(waterTypeIndex);

    const float scale = 1;
    const float waveHeight = 1;
    const float speed = .1 / sqrt(scale);
    vec2 uv1 = worldUvs(26 * scale) + animationFrame(sqrt(26. * scale) / speed * waterType.duration);
    vec2 uv2 = worldUvs(6 * scale) + animationFrame(sqrt(6. * scale) / speed * waterType.duration) * vec2(-2, 3);

    // get diffuse textures
//    vec3 n1 = linearToSrgb(texture(textureArray, vec3(uv1, MAT_WATER_NORMAL_MAP_1.colorMap)).xyz);
//    vec3 n2 = linearToSrgb(texture(textureArray, vec3(uv2, MAT_WATER_NORMAL_MAP_2.colorMap)).xyz);
    vec3 n1 = linearToSrgb(textureBicubic(textureArray, vec3(uv1, MAT_WATER_NORMAL_MAP_1.colorMap)).xyz);
    vec3 n2 = linearToSrgb(textureBicubic(textureArray, vec3(uv2, MAT_WATER_NORMAL_MAP_2.colorMap)).xyz);

    // Normalize
    n1.xy = (n1.xy * 2 - 1);
    n2.xy = (n2.xy * 2 - 1);
    // Tangent space to world
    n1.z *= -1;
    n2.z *= -1;
    n1.xyz = n1.xzy;
    n2.xyz = n2.xzy;
    n1 = normalize(vec3(1, 1 / waveHeight, 1) * n1);
    n2 = normalize(vec3(1, 1 / waveHeight, 1) * n2);
    vec3 normals = normalize(n1 + n2);

    vec3 fragToCam = viewDir;

    // fresnel reflection
    float fresnel = calculateFresnel(normals, fragToCam, IOR_WATER);

    vec3 reflectionColor = srgbToLinear(fogColor);
    if (waterReflectionEnabled && abs(IN.position.y - waterHeight) < 16) {
        vec3 I = -viewDir; // incident
        vec3 N = normals; // normal
        vec3 R = reflect(I, N);

        // for now, assume the water is level
        vec3 flatR = reflect(I, vec3(0, -1, 0));

        // TODO: use actual viewport size here
        vec2 screenSize = vec2(textureSize(waterReflectionMap, 0));
        screenSize /= PLANAR_REFLECTION_RESOLUTION;
        vec2 texelSize = 1 / screenSize;
        vec2 uv = gl_FragCoord.xy / screenSize;

        vec3 flatRxz = normalize(flatR - vec3(0, flatR.y, 0));
        vec3 uvY = normalize(flatR - flatRxz);
        vec3 uvX = normalize(cross(flatR, uvY));

        float dist = length(IN.position - cameraPos);
        float distortionFactor = 1 - exp(-dist * .0004);

        float x = dot(R, uvX);
        float y = dot(R, uvY);
        int waterDepth = vTerrainData[0] >> 8 & 0x7FF;
        vec2 distortion = vec2(x, y) * 100 * distortionFactor;
        // TODO: Don't distort too close to the shore
        float shoreLineMask = 1 - dot(IN.texBlend, vec3(vColor[0].x, vColor[1].x, vColor[2].x));
        distortion *= 1 - shoreLineMask;
        uv += texelSize * distortion;

        uv = clamp(uv, texelSize, 1 - texelSize);
//        float mipLevel = 5;
//        float mipRadius = pow(2, mipLevel);
//        uv = clamp(uv, texelSize * mipRadius, 1 - texelSize * mipRadius);

        // This will be linear or sRGB depending on the linear alpha blending setting
//        reflectionColor = texture(waterReflectionMap, uv, .5).rgb;
        reflectionColor = textureBicubic(waterReflectionMap, uv).rgb;
        #if !LINEAR_ALPHA_BLENDING
        // When linear alpha blending is on, the texture is in sRGB, and OpenGL will automatically convert it to linear
        reflectionColor = srgbToLinear(reflectionColor);
        #endif
//        return vec4(linearToSrgb(reflectionColor), 1);
    }

    // Begin constructing final output color
    vec4 dst = vec4(reflectionColor, fresnel);

    if (waterTypeIndex == 7) {
        if (dot(reflectionColor, reflectionColor) == 0) {
            dst.rgb = srgbToLinear(vec3(100, 0, 0) / 255.f) * 2.5;
        }
    } else {
        float cosUp = -normals.y;

        float H = (1 - pow(cosUp, 1.f)) * 50; // wave height
        float k_1 = 1;
        float k_2 = .01;
        float k_3 = .02;
        float k_4 = 0;
        vec3 C_ss = vec3(0, .32, .32); // water scatter color
        vec3 C_f = vec3(1); // air bubble color
        float P_f = .01; // density of air bubbles

        vec3 omega_i = lightDir; // incoming = sun to frag
        vec3 omega_o = viewDir; // outgoing = frag to camera
        vec3 omega_h = normalize(omega_o - omega_i); // half-way between incoming and outgoing
        vec3 omega_n = IN.normal.xzy; // macro scale normal
        vec3 w_n = normals; // presumably wave normal?
        omega_n = w_n;

        vec3 L_sun = lightColor * lightStrength;
        vec3 L_scatter = (
            k_1*H*pow(max(0, dot(omega_i, -omega_o)), 4.f) * pow(.5 - .5*dot(omega_i, omega_n), 3.f)
            + k_2*pow(max(0, dot(omega_o, omega_n)), 2.f)
        ) * C_ss*L_sun;
        L_scatter += k_3*max(0, dot(omega_i, w_n))*C_ss*L_sun + k_4*P_f*C_f*L_sun;
//        return vec4(linearToSrgb(L_scatter), 1);

        dst.rgb += L_scatter / dst.a;

        float roughnessSquared = .05;
        omega_h = normalize(omega_o + omega_i); // half-way between incoming and outgoing
        float p_22 = 1 / (PI * roughnessSquared) * exp((-omega_h.x*omega_h.x - omega_h.z*omega_h.z) / roughnessSquared);
        vec3 L_specular = L_sun * calculateFresnel(omega_h, -omega_i, IOR_WATER) * p_22 / (4 * dot(omega_n, omega_o));
//        return vec4(linearToSrgb(L_specular), 1);
//        dst.rgb += L_specular / 5;

        float clip = max(max(dst.r, dst.g), dst.b);
        if (clip > 1) {
            dst.a *= clip;
            dst.rgb /= clip;
        }
    }

    // If the water is opaque, blend in a fake underwater surface
    if (waterType.isFlat) {
        vec3 underwaterSrgb = packedHslToSrgb(6676);
        int depth = 5000;
        sampleUnderwater(underwaterSrgb, waterType, depth, 0);
        dst.rgb = dst.rgb * dst.a + srgbToLinear(underwaterSrgb) * (1 - dst.a);
        dst.a = 1;
    }
    // Like before, sampleWater needs to return sRGB
    dst.rgb = linearToSrgb(dst.rgb);
    return dst;
}

void sampleUnderwater(inout vec3 outputColor, WaterType waterType, float depth, float shadow) {
    outputColor = srgbToLinear(outputColor);

    // Pure water based on https://en.wikipedia.org/wiki/Electromagnetic_absorption_by_water#/media/File:Absorption_coefficient_of_water.svg
    // Converted to RGB through https://en.wikipedia.org/wiki/CIE_1931_color_space#Color_matching_functions
    vec3 waterAbsorbance = vec3(5.89243, 1.44154, 0.195755);
    float extinctionCoefficient = .001;

    // Trying to approximate some kind of ocean water with phytoplankton absorption based on https://www.oceanopticsbook.info/view/absorption/absorption-by-oceanic-constituents
    waterAbsorbance += vec3(0.105, .315, .525);

    int waterTypeIndex = vTerrainData[0] >> 3 & 0x1F;
    if (waterTypeIndex == 7) {
        waterAbsorbance = 1 - srgbToLinear(vec3(25, 0, 0) / 255.f);
        extinctionCoefficient = .2;
        outputColor = vec3(0);
    }

    // TODO: extinction towards camera * (
    //     directional light * extinction in lightdir + ambient light * extinction down
    // )

    // Our environmental ambient lighting might be totally out of whack for this
    vec3 ambientLight = ambientColor * ambientStrength;
//    ambientLight = fogColor; // maybe this is more correct?
//    outputColor *= ambientLight;

    vec3 extinctionCoefficientRgb = extinctionCoefficient * waterAbsorbance;

    // Since refraction displacement is applied in geom, this distance is the refracted distance
    vec3 camToFrag = normalize(IN.position - cameraPos);
    float distanceToSurface = depth / camToFrag.y;
    float totalDistance = depth + distanceToSurface;

    vec3 extinction = exp(-totalDistance * extinctionCoefficientRgb);
    outputColor *= extinction;

    // This is wrong
    vec3 shadowRgb = shadow * extinction;
    outputColor *= 1 - shadowRgb;

    if (underwaterCaustics) {
        vec2 causticsUv = worldUvs(7);
        const vec2 direction = vec2(1, -2);
        vec2 flow1 = causticsUv + animationFrame(13) * direction;
        vec2 flow2 = causticsUv * 1.5 + animationFrame(17) * -direction;
        vec3 caustics = sampleCaustics(flow1, flow2, depth * .000025);

        // Artificially bump up the brightness
        caustics *= 3;

        // Apply caustics color based on the environment
        // Usually this falls back to directional lighting
        caustics *= underwaterCausticsColor * underwaterCausticsStrength;

        // Attenuate by light absorption by the water above
        caustics *= extinction;

        // Fade caustics out too close to the shoreline
        caustics *= min(1, depth / 128);

        // Mix light from caustics in with the seabed
        outputColor.rgb *= 1 + caustics;
    }

    outputColor = clamp(outputColor, vec3(0), vec3(1));
    outputColor = linearToSrgb(outputColor);
}
