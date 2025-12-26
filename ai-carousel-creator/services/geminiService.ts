import { GoogleGenAI, Modality, Type } from "@google/genai";
import type { GenerateContentResponse } from "@google/genai";
import { Slide, Language, ReferenceType } from "../types";

const API_KEY = process.env.API_KEY;

if (!API_KEY) {
  throw new Error("API_KEY environment variable not set");
}

const ai = new GoogleGenAI({ apiKey: API_KEY });

/**
 * Analyzes raw text and breaks it down into structured slides with visual prompts.
 */
export const parseStoryToSlides = async (rawText: string, language: Language): Promise<{ text: string; imagePrompt: string }[]> => {
  try {
    const langName = language === 'ru' ? 'RUSSIAN' : 'ENGLISH';
    const prompt = `
      You are an expert viral content creator for Instagram and LinkedIn in ${langName}.
      Task: Convert the provided text into a high-engagement carousel structure in ${langName}.
      
      Guidelines for Content:
      1. **Language**: Output MUST be in ${langName}.
      2. **Structure**:
         - **Slide 1 (Hook)**: Short, punchy headline.
         - **Middle Slides**: ONE distinct thought per slide. No long paragraphs.
         - **Last Slide**: Call to Action (CTA).
      3. **Formatting**: Wrap key impactful words (1-3 words per slide) in <b> tags for highlighting (e.g. "This is <b>important</b>").
      4. **Quantity**: Create 5-10 slides.
      5. **Visuals**: Provide an English image prompt for the background.
      
      Input Text:
      """
      ${rawText}
      """
    `;

    const response = await ai.models.generateContent({
      model: 'gemini-2.5-flash',
      contents: prompt,
      config: {
        responseMimeType: "application/json",
        responseSchema: {
          type: Type.ARRAY,
          items: {
            type: Type.OBJECT,
            properties: {
              text: { type: Type.STRING, description: `The content for the slide overlay in ${langName}, with <b> tags.` },
              imagePrompt: { type: Type.STRING, description: "A visual description for the background image in English." }
            },
            required: ["text", "imagePrompt"]
          }
        }
      }
    });

    let jsonText = response.text;
    if (!jsonText) throw new Error("Empty response from AI");
    
    // Sanitize markdown code blocks if present
    jsonText = jsonText.replace(/```json\n?/, '').replace(/```\n?$/, '').trim();

    return JSON.parse(jsonText);

  } catch (error) {
    console.error("Error parsing story:", error);
    throw new Error("Failed to parse story into slides.");
  }
};

export const makeTextViral = async (currentText: string, type: 'hook' | 'body' | 'cta', language: Language): Promise<string> => {
    try {
        const langName = language === 'ru' ? 'RUSSIAN' : 'ENGLISH';
        let instruction = "";
        if (type === 'hook') {
            instruction = `Rewrite this text in ${langName} to be a viral 'Hook' headline. Use strong triggers (Fear, Curiosity, Gain). Keep it under 10 words. Wrap the main trigger word in <b> tags.`;
        } else if (type === 'cta') {
            instruction = `Rewrite this text in ${langName} to be a powerful Call to Action. Encourage saves/shares. Short. Wrap the action word in <b> tags.`;
        } else {
            instruction = `Rewrite this text in ${langName} to be clearer and punchier. One main thought. Wrap the key concept in <b> tags.`;
        }

        const response = await ai.models.generateContent({
            model: 'gemini-2.5-flash',
            contents: `Original Text: "${currentText}"\n\nInstruction: ${instruction}\n\nOutput only the rewritten text.`,
        });

        return response.text?.trim() || currentText;
    } catch (error) {
        console.error("Error making text viral:", error);
        return currentText;
    }
};

export const generateImage = async (prompt: string, style: string, referenceImage?: string, referenceType?: ReferenceType): Promise<string> => {
  try {
    // Combine user prompt with selected art style
    // Added "family friendly, safe for work" to help avoid blocks on abstract prompts
    let fullPrompt = `${prompt}. Style: ${style}. Abstract, minimalist, high quality background wallpaper. No text. Family friendly.`;
    
    if (referenceImage && referenceType) {
        fullPrompt += ` IMPORTANT: Use the provided image as a strong reference for ${referenceType}. Incorporate the ${referenceType} from the image into the scene described.`;
    }

    const contents: any = {
      parts: [
          { text: fullPrompt }
      ],
    };

    // Add image part if reference is provided
    if (referenceImage) {
        // Base64 string usually comes as "data:image/png;base64,....."
        // We need to strip the prefix for the API
        const base64Data = referenceImage.split(',')[1];
        const mimeType = referenceImage.substring(referenceImage.indexOf(':') + 1, referenceImage.indexOf(';'));
        
        contents.parts.push({
            inlineData: {
                mimeType: mimeType || 'image/png',
                data: base64Data
            }
        });
    }

    const response: GenerateContentResponse = await ai.models.generateContent({
      model: 'gemini-2.5-flash-image',
      contents: contents,
      config: {
        responseModalities: [Modality.IMAGE],
      },
    });

    if (response.promptFeedback?.blockReason) {
      throw new Error(`Blocked: ${response.promptFeedback.blockReason}`);
    }

    const candidate = response.candidates?.[0];
    if (!candidate) throw new Error("No response candidate");

    const imagePart = candidate.content?.parts?.find(part => part.inlineData);

    if (imagePart?.inlineData) {
      const { data, mimeType } = imagePart.inlineData;
      return `data:${mimeType};base64,${data}`;
    }

    throw new Error("No image data generated.");

  } catch (error) {
    console.error("Error generating image:", error);
    throw error;
  }
};