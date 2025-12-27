# Build stage
FROM node:20-alpine AS builder

WORKDIR /app

# Copy package files
COPY package.json package-lock.json* ./

# Install dependencies
RUN npm ci

# Copy source
COPY . .

# Set build-time environment variables for Vite
ENV VITE_RENDER_SERVER_URL=https://vibee-remotion.fly.dev

# Build
RUN npm run build

# Production stage - nginx for static files
FROM nginx:alpine

# Copy built files
COPY --from=builder /app/dist /usr/share/nginx/html

# Copy nginx config for SPA routing with proper asset handling
RUN echo 'server { \
    listen 8080; \
    root /usr/share/nginx/html; \
    index index.html; \
    \
    # Static assets - serve directly with cache \
    location /assets/ { \
        expires 1y; \
        add_header Cache-Control "public, immutable"; \
        try_files $uri =404; \
    } \
    \
    # Service worker - no cache \
    location /sw.js { \
        add_header Cache-Control "no-cache"; \
        try_files $uri =404; \
    } \
    \
    # SPA fallback for routes \
    location / { \
        add_header Cache-Control "no-cache"; \
        try_files $uri $uri/ /index.html; \
    } \
}' > /etc/nginx/conf.d/default.conf

EXPOSE 8080

CMD ["nginx", "-g", "daemon off;"]
