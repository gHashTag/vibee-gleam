/**
 * Montserrat Font Loading for Russian Captions
 *
 * Loads Montserrat with all weights and Cyrillic support
 */

import { loadFont, fontFamily } from '@remotion/google-fonts/Montserrat';

// Load Montserrat with all weights and subsets (including Cyrillic)
const { waitUntilDone } = loadFont();

// Caption font configuration
export const CAPTION_FONT = {
  fontFamily: fontFamily,
  fontWeight: 700 as const,
};

// Export font family string for use in components
export const MONTSERRAT_BOLD = fontFamily;

// Export wait function for ensuring font is loaded before rendering
export { waitUntilDone };
