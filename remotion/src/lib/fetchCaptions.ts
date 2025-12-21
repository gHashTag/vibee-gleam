/**
 * Fetch captions from transcription API
 * Used by calculateMetadata for dynamic caption loading
 * Falls back to static JSON when API unavailable
 */

export interface Caption {
  text: string;
  startMs: number;
  endMs: number;
  timestampMs: number;
  confidence: number;
}

const RENDER_SERVER_URL = process.env.RENDER_SERVER_URL || "http://localhost:3333";

// Static captions for lipsync.mp4 (fallback when API unavailable)
const LIPSYNC_CAPTIONS: Caption[] = [
  { text: "Уже", startMs: 0, endMs: 310, timestampMs: 0, confidence: 1 },
  { text: "в", startMs: 310, endMs: 370, timestampMs: 310, confidence: 1 },
  { text: "эту", startMs: 370, endMs: 690, timestampMs: 370, confidence: 1 },
  { text: "пятницу", startMs: 690, endMs: 1340, timestampMs: 690, confidence: 1 },
  { text: "17", startMs: 1590, endMs: 1720, timestampMs: 1590, confidence: 1 },
  { text: "октября", startMs: 1720, endMs: 2510, timestampMs: 1720, confidence: 1 },
  { text: "в", startMs: 2540, endMs: 2630, timestampMs: 2540, confidence: 1 },
  { text: "11", startMs: 2630, endMs: 2880, timestampMs: 2630, confidence: 1 },
  { text: "утра", startMs: 2880, endMs: 3240, timestampMs: 2880, confidence: 1 },
  { text: "приглашаю", startMs: 3240, endMs: 3760, timestampMs: 3240, confidence: 1 },
  { text: "вас", startMs: 3760, endMs: 3950, timestampMs: 3760, confidence: 1 },
  { text: "в", startMs: 3950, endMs: 4140, timestampMs: 3950, confidence: 1 },
  { text: "кафе", startMs: 4140, endMs: 4320, timestampMs: 4140, confidence: 1 },
  { text: "13", startMs: 4320, endMs: 5000, timestampMs: 4320, confidence: 1 },
  { text: "на", startMs: 5000, endMs: 5040, timestampMs: 5000, confidence: 1 },
  { text: "бизнес-завтрак", startMs: 5040, endMs: 5890, timestampMs: 5040, confidence: 1 },
  { text: "IP", startMs: 5890, endMs: 5980, timestampMs: 5890, confidence: 1 },
  { text: "Business", startMs: 5980, endMs: 6540, timestampMs: 5980, confidence: 1 },
  { text: "Club", startMs: 6540, endMs: 6780, timestampMs: 6540, confidence: 1 },
  { text: "Я", startMs: 7000, endMs: 7080, timestampMs: 7000, confidence: 1 },
  { text: "поделюсь", startMs: 7080, endMs: 7720, timestampMs: 7080, confidence: 1 },
  { text: "своим", startMs: 7720, endMs: 8140, timestampMs: 7720, confidence: 1 },
  { text: "опытом", startMs: 8140, endMs: 8600, timestampMs: 8140, confidence: 1 },
  { text: "создания", startMs: 8600, endMs: 9240, timestampMs: 8600, confidence: 1 },
  { text: "IE", startMs: 9240, endMs: 9330, timestampMs: 9240, confidence: 1 },
  { text: "агентов", startMs: 9330, endMs: 9880, timestampMs: 9330, confidence: 1 },
  { text: "расскажу", startMs: 10000, endMs: 10910, timestampMs: 10000, confidence: 1 },
  { text: "как", startMs: 11060, endMs: 11120, timestampMs: 11060, confidence: 1 },
  { text: "можно", startMs: 11120, endMs: 11410, timestampMs: 11120, confidence: 1 },
  { text: "просто", startMs: 11410, endMs: 11650, timestampMs: 11410, confidence: 1 },
  { text: "и", startMs: 11650, endMs: 11680, timestampMs: 11650, confidence: 1 },
  { text: "быстро", startMs: 11680, endMs: 12040, timestampMs: 11680, confidence: 1 },
  { text: "создавать", startMs: 12040, endMs: 12470, timestampMs: 12040, confidence: 1 },
  { text: "приложения", startMs: 12470, endMs: 13000, timestampMs: 12470, confidence: 1 },
  { text: "и", startMs: 13000, endMs: 13200, timestampMs: 13000, confidence: 1 },
  { text: "ботов", startMs: 13200, endMs: 14000, timestampMs: 13200, confidence: 1 },
  { text: "с", startMs: 14000, endMs: 14050, timestampMs: 14000, confidence: 1 },
  { text: "помощью", startMs: 14050, endMs: 14450, timestampMs: 14050, confidence: 1 },
  { text: "Вайп-кодинга", startMs: 14450, endMs: 15000, timestampMs: 14450, confidence: 1 },
  { text: "используя", startMs: 15000, endMs: 15600, timestampMs: 15000, confidence: 1 },
  { text: "обычный", startMs: 15600, endMs: 16090, timestampMs: 15600, confidence: 1 },
  { text: "язык", startMs: 16090, endMs: 16320, timestampMs: 16090, confidence: 1 },
  { text: "вместо", startMs: 16320, endMs: 16730, timestampMs: 16320, confidence: 1 },
  { text: "программирования", startMs: 16730, endMs: 18000, timestampMs: 16730, confidence: 1 },
  { text: "Покажу", startMs: 18000, endMs: 18400, timestampMs: 18000, confidence: 1 },
  { text: "на", startMs: 18400, endMs: 18660, timestampMs: 18400, confidence: 1 },
  { text: "реальных", startMs: 18660, endMs: 19070, timestampMs: 18660, confidence: 1 },
  { text: "примерах", startMs: 19070, endMs: 19610, timestampMs: 19070, confidence: 1 },
  { text: "как", startMs: 19670, endMs: 19870, timestampMs: 19670, confidence: 1 },
  { text: "интегрировать", startMs: 19870, endMs: 20740, timestampMs: 19870, confidence: 1 },
  { text: "IE", startMs: 20740, endMs: 20800, timestampMs: 20740, confidence: 1 },
  { text: "находить", startMs: 21000, endMs: 21720, timestampMs: 21000, confidence: 1 },
  { text: "клиентов", startMs: 21720, endMs: 22180, timestampMs: 21720, confidence: 1 },
  { text: "и", startMs: 22180, endMs: 22210, timestampMs: 22180, confidence: 1 },
  { text: "монетизировать", startMs: 22210, endMs: 23200, timestampMs: 22210, confidence: 1 },
  { text: "эти", startMs: 23200, endMs: 23410, timestampMs: 23200, confidence: 1 },
  { text: "навыки", startMs: 23410, endMs: 23830, timestampMs: 23410, confidence: 1 },
  { text: "Вас", startMs: 24000, endMs: 24180, timestampMs: 24000, confidence: 1 },
  { text: "ждет", startMs: 24180, endMs: 24420, timestampMs: 24180, confidence: 1 },
  { text: "профессиональное", startMs: 24420, endMs: 25380, timestampMs: 24420, confidence: 1 },
  { text: "сообщество", startMs: 25380, endMs: 26000, timestampMs: 25380, confidence: 1 },
  { text: "обмен", startMs: 26000, endMs: 26340, timestampMs: 26000, confidence: 1 },
  { text: "опытом", startMs: 26340, endMs: 26840, timestampMs: 26340, confidence: 1 },
  { text: "и", startMs: 26840, endMs: 26860, timestampMs: 26840, confidence: 1 },
  { text: "нетворкинг.", startMs: 26860, endMs: 27440, timestampMs: 26860, confidence: 1 },
];

/**
 * Fetch captions from video via transcription API
 * Falls back to static captions when API unavailable
 */
export async function fetchCaptions(
  videoUrl: string,
  language: string = "ru"
): Promise<Caption[]> {
  // Check if this is the default lipsync video - use static fallback
  if (videoUrl.includes("lipsync.mp4") || videoUrl === "/lipsync/lipsync.mp4") {
    try {
      const response = await fetch(`${RENDER_SERVER_URL}/transcribe`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ videoUrl, language }),
      });

      if (response.ok) {
        const data = await response.json();
        if (data.captions && data.captions.length > 0) {
          return data.captions;
        }
      }
    } catch {
      // API unavailable, use static fallback
    }

    console.log("Using static captions for lipsync.mp4");
    return LIPSYNC_CAPTIONS;
  }

  // For other videos, try API only
  try {
    const response = await fetch(`${RENDER_SERVER_URL}/transcribe`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ videoUrl, language }),
    });

    if (!response.ok) {
      throw new Error(`Transcription failed: ${response.statusText}`);
    }

    const data = await response.json();
    return data.captions || [];
  } catch (error) {
    console.error("Failed to fetch captions:", error);
    return [];
  }
}
