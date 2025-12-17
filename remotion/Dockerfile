FROM node:22-bookworm-slim

WORKDIR /app

# Install Chrome dependencies for headless rendering
RUN apt-get update && apt-get install -y \
    libnss3 \
    libdbus-1-3 \
    libatk1.0-0 \
    libgbm-dev \
    libasound2 \
    libxrandr2 \
    libxkbcommon-dev \
    libxfixes3 \
    libxcomposite1 \
    libxdamage1 \
    libatk-bridge2.0-0 \
    libpango-1.0-0 \
    libcairo2 \
    libcups2 \
    fonts-noto-color-emoji \
    fonts-noto-cjk \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Copy package files
COPY package.json package-lock.json* ./

# Install dependencies
RUN npm ci

# Install Chrome for Remotion
RUN npx remotion browser ensure

# Copy source code
COPY tsconfig.json remotion.config.ts ./
COPY src ./src
COPY public ./public

# Copy render server
COPY render-server.ts ./

# Create output directory
RUN mkdir -p /app/out && chown -R node:node /app

# Use existing node user (UID 1000)
USER node

EXPOSE 3333

CMD ["npx", "tsx", "render-server.ts"]
