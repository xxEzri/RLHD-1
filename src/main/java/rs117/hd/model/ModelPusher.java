package rs117.hd.model;

import com.google.common.primitives.Ints;
import lombok.extern.slf4j.Slf4j;
import net.runelite.api.*;
import net.runelite.api.kit.KitType;
import org.lwjgl.system.MemoryUtil;
import rs117.hd.HdPlugin;
import rs117.hd.HdPluginConfig;
import rs117.hd.data.BakedModels;
import rs117.hd.data.materials.Material;
import rs117.hd.data.materials.Overlay;
import rs117.hd.data.materials.Underlay;
import rs117.hd.data.materials.UvType;
import rs117.hd.model.objects.InheritTileColorType;
import rs117.hd.model.objects.ObjectProperties;
import rs117.hd.model.objects.ObjectType;
import rs117.hd.model.objects.TzHaarRecolorType;
import rs117.hd.scene.ProceduralGenerator;
import rs117.hd.utils.HDUtils;

import static rs117.hd.utils.HDUtils.dotNormal3Lights;
import rs117.hd.utils.buffer.GpuFloatBuffer;
import rs117.hd.utils.buffer.GpuIntBuffer;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.lang.ref.PhantomReference;
import java.lang.ref.ReferenceQueue;
import java.nio.Buffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.util.*;

/**
 * Pushes models
 */
@Singleton
@Slf4j
public class ModelPusher {
    @Inject
    private HdPlugin hdPlugin;

    @Inject
    private HdPluginConfig config;

    @Inject
    private Client client;

    @Inject
    private ProceduralGenerator proceduralGenerator;

    private IntBufferCache vertexDataCache;
    private FloatBufferCache normalDataCache;
    private FloatBufferCache uvDataCache;
    private final Map<PhantomReference<Buffer>, BufferInfo> bufferInfo;
    private final ReferenceQueue<Buffer> bufferReferenceQueue;
    private long bytesCached;
    private long maxByteCapacity;
    private final Object bytesCachedLock;

    private long lastHintTime;

//    private int pushes = 0;
//    private int vertexDataHits = 0;
//    private int normalDataHits = 0;
//    private int uvDataHits = 0;

    public ModelPusher() {
        this.bufferInfo = new HashMap<>();
        this.bufferReferenceQueue = new ReferenceQueue<>();
        this.bytesCached = 0;
        this.bytesCachedLock = new Object();
        this.lastHintTime = System.currentTimeMillis();
    }

    public void init() {
        // allocate half of the budget to actively used memory
        // 80% to vertex data
        // 15% to normal data
        // 5% to uv data
        this.vertexDataCache = new IntBufferCache((long)(config.modelCacheSizeMB() / 2 * 1000000 * 0.80));
        this.normalDataCache = new FloatBufferCache((long)(config.modelCacheSizeMB() / 2 * 1000000 * 0.15));
        this.uvDataCache = new FloatBufferCache((long)(config.modelCacheSizeMB() / 2 * 1000000 * 0.05));
        this.maxByteCapacity = config.modelCacheSizeMB() * 1000000L;
    }

    // subtracts the X lowest lightness levels from the formula.
    // helps keep darker colors appropriately dark
    private static final int ignoreLowLightness = 3;
    // multiplier applied to vertex' lightness value.
    // results in greater lightening of lighter colors
    private static final float lightnessMultiplier = 3f;
    // the minimum amount by which each color will be lightened
    private static final int baseLighten = 10;
    // same thing but for the normalBuffer and uvBuffer
    private final static float[] zeroFloats = new float[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    private final static int[] twoInts = new int[2];
    private final static int[] fourInts = new int[4];
//    private final static int[] eightInts = new int[8];
    private final static int[] twelveInts = new int[12];
    private final static float[] twelveFloats = new float[12];
    private final static int[] modelColors = new int[HdPlugin.MAX_TRIANGLE * 4];

    public void clearModelCache() {
        vertexDataCache.clear();
        normalDataCache.clear();
        uvDataCache.clear();
        System.gc();
    }

//    public void printStats() {
//        StringBuilder stats = new StringBuilder();
//        stats.append("\nModel pusher cache stats:\n");
//        stats.append("Vertex cache hit ratio: ").append((float)vertexDataHits/pushes*100).append("%\n");
//        stats.append("Normal cache hit ratio: ").append((float)normalDataHits/pushes*100).append("%\n");
//        stats.append("UV cache hit ratio: ").append((float)uvDataHits/pushes*100).append("%\n");
//        stats.append(vertexDataCache.size()).append(" vertex datas consuming ").append(vertexDataCache.getBytesConsumed()).append(" bytes\n");
//        stats.append(normalDataCache.size()).append(" normal datas consuming ").append(normalDataCache.getBytesConsumed()).append(" bytes\n");
//        stats.append(uvDataCache.size()).append(" uv datas consuming ").append(uvDataCache.getBytesConsumed()).append(" bytes\n");
//
//        log.debug(stats.toString());
//
//        vertexDataHits = 0;
//        normalDataHits = 0;
//        uvDataHits = 0;
//        pushes = 0;
//    }

    public void freeFinalizedBuffers() {
        int freeCount = 0;
        int freeAttempts = 0;
        PhantomReference<Buffer> reference;

        while ((reference = (PhantomReference<Buffer>) this.bufferReferenceQueue.poll()) != null) {
            freeAttempts++;

            synchronized (this.bufferInfo) {
                BufferInfo bi = this.bufferInfo.get(reference);
                if (bi != null) {
                    freeCount++;
                    MemoryUtil.nmemFree(bi.getAddress());
                    synchronized (this.bytesCachedLock) {
                        this.bytesCached -= bi.getBytes();
                    }
                    this.bufferInfo.remove(reference);
                }
            }


            if (freeAttempts != freeCount) {
                // I've thought about removing this bit, but it's probably a good assertion to leave in place.
                // Given that this is a memory leak it's something we should look out for
                log.error("failed to free cache reference!");
            }
        }
    }

    public int[] pushModel(Renderable renderable, Model model, GpuIntBuffer vertexBuffer, GpuFloatBuffer uvBuffer, GpuFloatBuffer normalBuffer, int tileX, int tileY, int tileZ, ObjectProperties objectProperties, ObjectType objectType, boolean noCache, ModelHasher modelHasher) {
//        pushes++;
        final int faceCount = Math.min(model.getFaceCount(), HdPlugin.MAX_TRIANGLE);
        int vertexLength = 0;
        int uvLength = 0;

        // ensure capacity upfront
        vertexBuffer.ensureCapacity(12 * 2 * faceCount);
        normalBuffer.ensureCapacity(12 * 2 * faceCount);
        uvBuffer.ensureCapacity(12 * 2 * faceCount);

        boolean cachedVertexData = false;
        boolean cachedNormalData = false;
        boolean cachedUvData = false;
        int vertexCacheHash = 0;
        int normalDataCacheHash = 0;
        int uvDataCacheHash = 0;

        if (!noCache) {
            vertexCacheHash = modelHasher.calculateVertexCacheHash();
            normalDataCacheHash = modelHasher.calculateNormalCacheHash();
            uvDataCacheHash = modelHasher.calculateUvCacheHash(objectProperties == null ? new int[]{} : objectProperties.getId());

            IntBuffer vertexData = vertexDataCache.get(vertexCacheHash);
            cachedVertexData = vertexData != null && vertexData.remaining() == faceCount * 12;
            if (cachedVertexData) {
//                vertexDataHits++;
                vertexLength = faceCount * 3;
                vertexBuffer.put(vertexData);
                vertexData.rewind();
            }

            FloatBuffer normalData = normalDataCache.get(normalDataCacheHash);
            cachedNormalData = normalData != null && normalData.remaining() == faceCount * 12;
            if (cachedNormalData) {
//                normalDataHits++;
                normalBuffer.put(normalData);
                normalData.rewind();
            }

            FloatBuffer uvData = uvDataCache.get(uvDataCacheHash);
            cachedUvData = uvData != null;
            if (cachedUvData) {
//                uvDataHits++;
                uvLength = 3 * (uvData.remaining()/12);
                uvBuffer.put(uvData);
                uvData.rewind();
            }

            if (cachedVertexData && cachedUvData && cachedNormalData) {
                twoInts[0] = vertexLength;
                twoInts[1] = uvLength;
                return twoInts;
            }
        }

        IntBuffer fullVertexData = null;
        FloatBuffer fullNormalData = null;
        FloatBuffer fullUvData = null;

        boolean cachingVertexData = !cachedVertexData && !noCache;
        if (cachingVertexData) {
            fullVertexData = MemoryUtil.memAllocInt(faceCount * 12);
            synchronized (this.bufferInfo) {
                this.bufferInfo.put(new PhantomReference<>(fullVertexData, this.bufferReferenceQueue), new BufferInfo(MemoryUtil.memAddress(fullVertexData), faceCount * 12L * 4L));
            }
            synchronized (this.bytesCachedLock) {
                this.bytesCached += (long) faceCount * 12 * 4;
            }
        }

        boolean cachingNormalData = !cachedNormalData && !noCache;
        if (cachingNormalData) {
            fullNormalData = MemoryUtil.memAllocFloat(faceCount * 12);
            synchronized (this.bufferInfo) {
                this.bufferInfo.put(new PhantomReference<>(fullNormalData, this.bufferReferenceQueue), new BufferInfo(MemoryUtil.memAddress(fullNormalData), faceCount * 12L * 4L));
            }
            synchronized (this.bytesCachedLock) {
                this.bytesCached += (long) faceCount * 12 * 4;
            }
        }

        boolean cachingUvData = !cachedUvData && !noCache;
        if (cachingUvData) {
            fullUvData = MemoryUtil.memAllocFloat(faceCount * 12);
            synchronized (this.bufferInfo) {
                this.bufferInfo.put(new PhantomReference<>(fullUvData, this.bufferReferenceQueue), new BufferInfo(MemoryUtil.memAddress(fullUvData), faceCount * 12L * 4L));
            }
            synchronized (this.bytesCachedLock) {
                this.bytesCached += (long) faceCount * 12 * 4;
            }
        }

        boolean hideBakedEffects = config.hideBakedEffects();
        for (int face = 0; face < faceCount; face++) {
            if (!cachedVertexData) {
                int[] tempVertexData = getVertexDataForFace(model, getColorsForFace(renderable, model, objectProperties, objectType, tileX, tileY, tileZ, face, hideBakedEffects), face);
                vertexBuffer.put(tempVertexData);
                vertexLength += 3;

                if (cachingVertexData) {
                    fullVertexData.put(tempVertexData);
                }
            }

            if (!cachedNormalData) {
                float[] tempNormalData = getNormalDataForFace(model, objectProperties, face);
                normalBuffer.put(tempNormalData);

                if (cachingNormalData) {
                    fullNormalData.put(tempNormalData);
                }
            }

            if (!cachedUvData) {
                float[] tempUvData = getUvDataForFace(model, objectProperties, face);
                if (tempUvData != null) {
                    uvBuffer.put(tempUvData);
                    uvLength += 3;

                    if (cachingUvData) {
                        fullUvData.put(tempUvData);
                    }
                }
            }
        }

        if (cachingVertexData) {
            fullVertexData.flip();
            vertexDataCache.put(vertexCacheHash, fullVertexData);
        }

        if (cachingNormalData) {
            fullNormalData.flip();
            normalDataCache.put(normalDataCacheHash, fullNormalData);
        }

        if (cachingUvData) {
            fullUvData.flip();
            uvDataCache.put(uvDataCacheHash, fullUvData);
        }

        twoInts[0] = vertexLength;
        twoInts[1] = uvLength;

        return twoInts;
    }

    // hint the gc to run if we're holding more cache than the max capacity
    // this will allow the inactive portion of the cache to be finalized and thus freed
    public void hintGC() {
        // do not hint the GC more than once every 5 seconds
        if (this.bytesCached >= this.maxByteCapacity && System.currentTimeMillis() - this.lastHintTime > 5000) {
            System.gc();
            this.lastHintTime = System.currentTimeMillis();
        }
    }

    private int[] getVertexDataForFace(Model model, int[] faceColors, int face) {
        final int[] xVertices = model.getVerticesX();
        final int[] yVertices = model.getVerticesY();
        final int[] zVertices = model.getVerticesZ();
        final int triA = model.getFaceIndices1()[face];
        final int triB = model.getFaceIndices2()[face];
        final int triC = model.getFaceIndices3()[face];

        twelveInts[0] = xVertices[triA];
        twelveInts[1] = yVertices[triA];
        twelveInts[2] = zVertices[triA];
        twelveInts[3] = faceColors[3] | faceColors[0];
        twelveInts[4] = xVertices[triB];
        twelveInts[5] = yVertices[triB];
        twelveInts[6] = zVertices[triB];
        twelveInts[7] = faceColors[3] | faceColors[1];
        twelveInts[8] = xVertices[triC];
        twelveInts[9] = yVertices[triC];
        twelveInts[10] = zVertices[triC];
        twelveInts[11] = faceColors[3] | faceColors[2];

        return twelveInts;
    }

    private float[] getNormalDataForFace(Model model, ObjectProperties objectProperties, int face) {
        if ((objectProperties != null && objectProperties.isFlatNormals()) || model.getFaceColors3()[face] == -1) {
            return zeroFloats;
        }

        final int triA = model.getFaceIndices1()[face];
        final int triB = model.getFaceIndices2()[face];
        final int triC = model.getFaceIndices3()[face];
        final int[] xVertexNormals = model.getVertexNormalsX();
        final int[] yVertexNormals = model.getVertexNormalsY();
        final int[] zVertexNormals = model.getVertexNormalsZ();

        twelveFloats[0] = xVertexNormals[triA];
        twelveFloats[1] = yVertexNormals[triA];
        twelveFloats[2] = zVertexNormals[triA];
        twelveFloats[3] = 0;
        twelveFloats[4] = xVertexNormals[triB];
        twelveFloats[5] = yVertexNormals[triB];
        twelveFloats[6] = zVertexNormals[triB];
        twelveFloats[7] = 0;
        twelveFloats[8] = xVertexNormals[triC];
        twelveFloats[9] = yVertexNormals[triC];
        twelveFloats[10] = zVertexNormals[triC];
        twelveFloats[11] = 0;

        return twelveFloats;
    }

    private float[] getUvDataForFace(Model model, ObjectProperties objectProperties, int face) {
        final short[] faceTextures = model.getFaceTextures();
        final float[] uv = model.getFaceTextureUVCoordinates();

        boolean isVanillaTextured = faceTextures != null && faceTextures[face] != -1 && uv != null;
        Material material = objectProperties == null ? Material.NONE : objectProperties.getMaterial();

        if (isVanillaTextured) {
            material = Material.getTexture(faceTextures[face]);
            int packedMaterialData = packMaterialData(material, false);
            int idx = face * 6;

            twelveFloats[0] = packedMaterialData;
            twelveFloats[1] = uv[idx];
            twelveFloats[2] = uv[idx + 1];
            twelveFloats[3] = 0;
            twelveFloats[4] = packedMaterialData;
            twelveFloats[5] = uv[idx + 2];
            twelveFloats[6] = uv[idx + 3];
            twelveFloats[7] = 0;
            twelveFloats[8] = packedMaterialData;
            twelveFloats[9] = uv[idx + 4];
            twelveFloats[10] = uv[idx + 5];
            twelveFloats[11] = 0;

            return twelveFloats;
        } else if (material == Material.NONE) {
            return faceTextures == null ? null : zeroFloats;
        } else {
            final int triA = model.getFaceIndices1()[face];
            final int triB = model.getFaceIndices2()[face];
            final int triC = model.getFaceIndices3()[face];

            final int[] xVertices = model.getVerticesX();
            final int[] zVertices = model.getVerticesZ();

            int packedMaterialData = packMaterialData(hdPlugin.configObjectTextures ? material : Material.NONE, false);

            if (objectProperties.getUvType() == UvType.GROUND_PLANE) {
                twelveFloats[0] = packedMaterialData;
                twelveFloats[1] = (xVertices[triA] % Perspective.LOCAL_TILE_SIZE) / (float) Perspective.LOCAL_TILE_SIZE;
                twelveFloats[2] = (zVertices[triA] % Perspective.LOCAL_TILE_SIZE) / (float) Perspective.LOCAL_TILE_SIZE;
                twelveFloats[3] = 0;
                twelveFloats[4] = packedMaterialData;
                twelveFloats[5] = (xVertices[triB] % Perspective.LOCAL_TILE_SIZE) / (float) Perspective.LOCAL_TILE_SIZE;
                twelveFloats[6] = (zVertices[triB] % Perspective.LOCAL_TILE_SIZE) / (float) Perspective.LOCAL_TILE_SIZE;
                twelveFloats[7] = 0;
                twelveFloats[8] = packedMaterialData;
                twelveFloats[9] = (xVertices[triC] % Perspective.LOCAL_TILE_SIZE) / (float) Perspective.LOCAL_TILE_SIZE;
                twelveFloats[10] = (zVertices[triC] % Perspective.LOCAL_TILE_SIZE) / (float) Perspective.LOCAL_TILE_SIZE;
                twelveFloats[11] = 0;

                return twelveFloats;
            } else {
                twelveFloats[0] = packedMaterialData;
                twelveFloats[1] = 0;
                twelveFloats[2] = 0;
                twelveFloats[3] = 0;
                twelveFloats[4] = packedMaterialData;
                twelveFloats[5] = 1;
                twelveFloats[6] = 0;
                twelveFloats[7] = 0;
                twelveFloats[8] = packedMaterialData;
                twelveFloats[9] = 0;
                twelveFloats[10] = 1;
                twelveFloats[11] = 0;

                return twelveFloats;
            }
        }
    }

    public int packMaterialData(Material material, boolean isOverlay) {
        return material.ordinal() << 1 | (isOverlay ? 1 : 0);
    }

    private int[] getColorsForModel(Renderable renderable, Model model, ObjectProperties objectProperties, ObjectType objectType, int tileX, int tileY, int tileZ, int faceCount) {
        boolean hideBakedEffects = config.hideBakedEffects();

        for (int face = 0; face < faceCount; face++) {
            System.arraycopy(getColorsForFace(renderable, model, objectProperties, objectType, tileX, tileY, tileZ, face, hideBakedEffects), 0, modelColors, face * 4, 4);
        }

        return Arrays.copyOfRange(modelColors, 0, faceCount * 4);
    }

    private int[] removeBakedGroundShading(int face, int triA, int triB, int triC, byte[] faceTransparencies, short[] faceTextures, int[] yVertices) {
        if (faceTransparencies != null && (faceTextures == null || faceTextures[face] == -1) && (faceTransparencies[face] & 0xFF) > 100) {
            int aHeight = yVertices[triA];
            int bHeight = yVertices[triB];
            int cHeight = yVertices[triC];
            if (aHeight >= -8 && aHeight == bHeight && aHeight == cHeight) {
                fourInts[0] = 0;
                fourInts[1] = 0;
                fourInts[2] = 0;
                fourInts[3] = 0xFF << 24;
                return fourInts;
            }
        }

        return null;
    }

    private int[] getColorsForFace(Renderable renderable, Model model, ObjectProperties objectProperties, ObjectType objectType, int tileX, int tileY, int tileZ, int face, boolean hideBakedEffects) {
        int color1 = model.getFaceColors1()[face];
        int color2 = model.getFaceColors2()[face];
        int color3 = model.getFaceColors3()[face];
        final short[] faceTextures = model.getFaceTextures();
        final byte[] faceTransparencies = model.getFaceTransparencies();
        final byte overrideAmount = model.getOverrideAmount();
        final byte overrideHue = model.getOverrideHue();
        final byte overrideSat = model.getOverrideSaturation();
        final byte overrideLum = model.getOverrideLuminance();
        final int triA = model.getFaceIndices1()[face];
        final int triB = model.getFaceIndices2()[face];
        final int triC = model.getFaceIndices3()[face];
        final int[] yVertices = model.getVerticesY();
        final int[] xVertexNormals = model.getVertexNormalsX();
        final int[] yVertexNormals = model.getVertexNormalsY();
        final int[] zVertexNormals = model.getVertexNormalsZ();
        final Tile tile = client.getScene().getTiles()[tileZ][tileX][tileY];

        if (hideBakedEffects) {
            // hide the shadows and lights that are often baked into models by setting the colors for the shadow faces to transparent
            NPC npc = renderable instanceof NPC ? (NPC) renderable : null;
            Player player = renderable instanceof Player  ? (Player) renderable : null;
            GraphicsObject graphicsObject = renderable instanceof GraphicsObject ? (GraphicsObject) renderable : null;

            if ((npc != null && BakedModels.NPCS.contains(npc.getId())) || (graphicsObject != null && BakedModels.OBJECTS.contains(graphicsObject.getId())) || (player != null &&  player.getPlayerComposition().getEquipmentId(KitType.WEAPON) == ItemID.MAGIC_CARPET)) {
                int[] transparency = removeBakedGroundShading(face, triA, triB, triC, faceTransparencies, faceTextures, yVertices);
                if (transparency != null) {
                    return transparency;
                }
            }
        }

        if (color3 == -2) {
            fourInts[0] = 0;
            fourInts[1] = 0;
            fourInts[2] = 0;
            fourInts[3] = 0xFF << 24;
            return fourInts;
        } else if (color3 == -1) {
            color2 = color3 = color1;
        } else if ((faceTextures == null || faceTextures[face] == -1) && overrideAmount > 0) {
            // HSL override is not applied to flat shade faces or to textured faces
            color1 = interpolateHSL(color1, overrideHue, overrideSat, overrideLum, overrideAmount);
            color2 = interpolateHSL(color2, overrideHue, overrideSat, overrideLum, overrideAmount);
            color3 = interpolateHSL(color3, overrideHue, overrideSat, overrideLum, overrideAmount);
        }

        int color1H = color1 >> 10 & 0x3F;
        int color1S = color1 >> 7 & 0x7;
        int color1L = color1 & 0x7F;
        int color2H = color2 >> 10 & 0x3F;
        int color2S = color2 >> 7 & 0x7;
        int color2L = color2 & 0x7F;
        int color3H = color3 >> 10 & 0x3F;
        int color3S = color3 >> 7 & 0x7;
        int color3L = color3 & 0x7F;

        // reduce the effect of the baked shading by approximately inverting the process by which
        // the shading is added initially.
        int lightenA = (int) (Math.max((color1L - ignoreLowLightness), 0) * lightnessMultiplier) + baseLighten;
        float dotA = Math.max(dotNormal3Lights(new float[]{
                xVertexNormals[triA],
                yVertexNormals[triA],
                zVertexNormals[triA],
        }), 0);
        color1L = (int) HDUtils.lerp(color1L, lightenA, dotA);

        int lightenB = (int) (Math.max((color2L - ignoreLowLightness), 0) * lightnessMultiplier) + baseLighten;
        float dotB = Math.max(dotNormal3Lights(new float[]{
                xVertexNormals[triB],
                yVertexNormals[triB],
                zVertexNormals[triB],
        }), 0);
        color2L = (int) HDUtils.lerp(color2L, lightenB, dotB);

        int lightenC = (int) (Math.max((color3L - ignoreLowLightness), 0) * lightnessMultiplier) + baseLighten;
        float dotC = Math.max(dotNormal3Lights(new float[]{
                xVertexNormals[triC],
                yVertexNormals[triC],
                zVertexNormals[triC],
        }), 0);
        color3L = (int) HDUtils.lerp(color3L, lightenC, dotC);

        int maxBrightness = 55;
        if (faceTextures != null && faceTextures[face] != -1) {
            maxBrightness = 90;
            // set textured faces to pure white as they are harder to remove shadows from for some reason
            color1H = color2H = color3H = 0;
            color1S = color2S = color3S = 0;
            color1L = color2L = color3L = 127;
        }

        if (tile != null && objectProperties != null && objectProperties.getInheritTileColorType() != InheritTileColorType.NONE) {
            SceneTileModel tileModel = tile.getSceneTileModel();
            SceneTilePaint tilePaint = tile.getSceneTilePaint();

            if (tilePaint != null || tileModel != null) {
                int[] tileColorHSL;

                // No point in inheriting tilepaint color if the ground tile does not have a color, for example above a cave wall
                if (tilePaint != null && tilePaint.getTexture() == -1 && tilePaint.getRBG() != 0) {
                    // pull any corner color as either one should be OK
                    tileColorHSL = HDUtils.colorIntToHSL(tilePaint.getSwColor());

                    // average saturation and lightness
                    tileColorHSL[1] =
                            (
                                    tileColorHSL[1] +
                                            HDUtils.colorIntToHSL(tilePaint.getSeColor())[1] +
                                            HDUtils.colorIntToHSL(tilePaint.getNwColor())[1] +
                                            HDUtils.colorIntToHSL(tilePaint.getNeColor())[1]
                            ) / 4;

                    tileColorHSL[2] =
                            (
                                    tileColorHSL[2] +
                                            HDUtils.colorIntToHSL(tilePaint.getSeColor())[2] +
                                            HDUtils.colorIntToHSL(tilePaint.getNwColor())[2] +
                                            HDUtils.colorIntToHSL(tilePaint.getNeColor())[2]
                            ) / 4;

                    int overlayId = client.getScene().getOverlayIds()[tileZ][tileX][tileY];
                    int underlayId = client.getScene().getUnderlayIds()[tileZ][tileX][tileY];
                    if (overlayId != 0) {
                        Overlay overlay = Overlay.getOverlay(overlayId, tile, client, config);
                        tileColorHSL = proceduralGenerator.recolorOverlay(overlay, tileColorHSL);
                    } else {
                        Underlay underlay = Underlay.getUnderlay(underlayId, tile, client, config);
                        tileColorHSL = proceduralGenerator.recolorUnderlay(underlay, tileColorHSL);
                    }

                    color1H = color2H = color3H = tileColorHSL[0];
                    color1S = color2S = color3S = tileColorHSL[1];
                    color1L = color2L = color3L = tileColorHSL[2];

                } else if (tileModel != null && tileModel.getTriangleTextureId() == null) {
                    int faceColorIndex = -1;
                    for (int i = 0; i < tileModel.getTriangleColorA().length; i++) {
                        boolean isOverlayFace = proceduralGenerator.isOverlayFace(tile, i);
                        // Use underlay if the tile does not have an overlay, useful for rocks in cave corners.
                        if(objectProperties.getInheritTileColorType() == InheritTileColorType.UNDERLAY || tileModel.getModelOverlay() == 0) {
                            // pulling the color from UNDERLAY is more desirable for green grass tiles
                            // OVERLAY pulls in path color which is not desirable for grass next to paths
                            if (!isOverlayFace) {                                
                                faceColorIndex = i;
                                break;
                            }
                        }  
                        else if(objectProperties.getInheritTileColorType() == InheritTileColorType.OVERLAY) {
                            if (isOverlayFace) {
                                // OVERLAY used in dirt/path/house tile color blend better with rubbles/rocks
                                faceColorIndex = i;
                                break;
                            }
                        }                     
                    }

                    if (faceColorIndex != -1) {
                        tileColorHSL = HDUtils.colorIntToHSL(tileModel.getTriangleColorA()[faceColorIndex]);

                        int underlayId = client.getScene().getUnderlayIds()[tileZ][tileX][tileY];
                        Underlay underlay = Underlay.getUnderlay(underlayId, tile, client, config);
                        tileColorHSL = proceduralGenerator.recolorUnderlay(underlay, tileColorHSL);

                        color1H = color2H = color3H = tileColorHSL[0];
                        color1S = color2S = color3S = tileColorHSL[1];
                        color1L = color2L = color3L = tileColorHSL[2];
                    }
                }
            }
        }

        int packedAlphaPriority = getPackedAlphaPriority(model, face);

        if (hdPlugin.configTzhaarHD && objectProperties != null && objectProperties.getTzHaarRecolorType() != TzHaarRecolorType.NONE) {
            int[][] tzHaarRecolored = proceduralGenerator.recolorTzHaar(objectProperties, yVertices[triA], yVertices[triB], yVertices[triC], packedAlphaPriority, objectType, color1H, color1S, color1L, color2H, color2S, color2L, color3H, color3S, color3L);
            color1H = tzHaarRecolored[0][0];
            color1S = tzHaarRecolored[0][1];
            color1L = tzHaarRecolored[0][2];
            color2H = tzHaarRecolored[1][0];
            color2S = tzHaarRecolored[1][1];
            color2L = tzHaarRecolored[1][2];
            color3H = tzHaarRecolored[2][0];
            color3S = tzHaarRecolored[2][1];
            color3L = tzHaarRecolored[2][2];
            packedAlphaPriority = tzHaarRecolored[3][0];
        }

        color1L = Ints.constrainToRange(color1L, 0, maxBrightness);
        color2L = Ints.constrainToRange(color2L, 0, maxBrightness);
        color3L = Ints.constrainToRange(color3L, 0, maxBrightness);

        color1 = (color1H << 3 | color1S) << 7 | color1L;
        color2 = (color2H << 3 | color2S) << 7 | color2L;
        color3 = (color3H << 3 | color3S) << 7 | color3L;

        fourInts[0] = color1;
        fourInts[1] = color2;
        fourInts[2] = color3;
        fourInts[3] = packedAlphaPriority;

        return fourInts;
    }

    private static int interpolateHSL(int hsl, byte hue2, byte sat2, byte lum2, byte lerp) {
        int hue = hsl >> 10 & 63;
        int sat = hsl >> 7 & 7;
        int lum = hsl & 127;
        int var9 = lerp & 255;
        if (hue2 != -1) {
            hue += var9 * (hue2 - hue) >> 7;
        }

        if (sat2 != -1) {
            sat += var9 * (sat2 - sat) >> 7;
        }

        if (lum2 != -1) {
            lum += var9 * (lum2 - lum) >> 7;
        }

        return (hue << 10 | sat << 7 | lum) & 65535;
    }

    private int getPackedAlphaPriority(Model model, int face) {
        final short[] faceTextures = model.getFaceTextures();
        final byte[] faceTransparencies = model.getFaceTransparencies();
        final byte[] facePriorities = model.getFaceRenderPriorities();

        int alpha = 0;
        if (faceTransparencies != null && (faceTextures == null || faceTextures[face] == -1)) {
            alpha = (faceTransparencies[face] & 0xFF) << 24;
        }
        int priority = 0;
        if (facePriorities != null) {
            priority = (facePriorities[face] & 0xff) << 16;
        }
        return alpha | priority;
    }
}
