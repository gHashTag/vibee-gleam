// Test render request for SplitTalkingHead with test assets
const payload = {
  type: "video",
  compositionId: "SplitTalkingHead",
  inputProps: {
    lipSyncVideo: "/lipsync/lipsync.mp4",
    segments: [
      { type: "fullscreen", startFrame: 0, durationFrames: 90, caption: "" },
      { type: "split", startFrame: 90, durationFrames: 150, bRollUrl: "/backgrounds/business/01.mp4", bRollType: "video", caption: "" },
      { type: "fullscreen", startFrame: 240, durationFrames: 90, caption: "" },
      { type: "split", startFrame: 330, durationFrames: 150, bRollUrl: "/backgrounds/business/02.mp4", bRollType: "video", caption: "" },
      { type: "fullscreen", startFrame: 480, durationFrames: 108, caption: "" }
    ],
    backgroundMusic: "/audio/music/phonk_01.mp3",
    musicVolume: 0.10,
    captions: [],
    showCaptions: false
  }
};

async function testRender() {
  const url = "https://vibee-remotion.fly.dev/render";
  console.log("Sending render request to:", url);
  console.log("Payload:", JSON.stringify(payload, null, 2));

  try {
    const response = await fetch(url, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });

    const data = await response.json();
    console.log("\n=== Response ===");
    console.log("Status:", response.status);
    console.log("Data:", JSON.stringify(data, null, 2));

    if (data.renderId) {
      console.log("\n=== Render Started ===");
      console.log("Render ID:", data.renderId);
      console.log("Download URL when ready:", `https://vibee-remotion.fly.dev/renders/${data.renderId}`);
    }
  } catch (error) {
    console.error("Error:", error.message);
  }
}

testRender();
