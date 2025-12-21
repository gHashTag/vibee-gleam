FROM node:22-bookworm-slim

# Disable dbus and accessibility features for headless Chrome
ENV DBUS_SESSION_BUS_ADDRESS=/dev/null
ENV NO_AT_BRIDGE=1
ENV DISPLAY=:99

WORKDIR /app

# Install Chrome dependencies for headless rendering + fonts for Cyrillic/Russian + Xvfb
RUN apt-get update && apt-get install -y \
    libnss3 \
    libdbus-1-3 \
    dbus \
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
    fonts-noto-core \
    fonts-freefont-ttf \
    fontconfig \
    ffmpeg \
    curl \
    xvfb \
    && fc-cache -f -v \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /run/dbus

# Copy package files
COPY package.json package-lock.json* ./

# Install dependencies
RUN npm ci

# Install Chrome for Remotion
RUN npx remotion browser ensure

# Copy source code
COPY tsconfig.json remotion.config.ts ./
COPY src ./src

# Copy render server and entrypoint
COPY render-server.ts entrypoint.sh ./
RUN chmod +x entrypoint.sh

# Create placeholder directories (will be replaced by symlinks at runtime)
RUN mkdir -p /app/out /app/public /app/storage && chown -R node:node /app

# Use existing node user (UID 1000)
USER node

EXPOSE 3333

# Use entrypoint script to setup symlinks and run server
CMD ["./entrypoint.sh"]
