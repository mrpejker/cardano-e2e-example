# First build the static files of the site
FROM node:14-alpine as builder

WORKDIR /app

COPY package.json ./
COPY public/ ./public/
COPY scripts/ ./scripts/
COPY config/ ./config/
COPY src/ ./src
COPY tsconfig.json ./
COPY .env ./

RUN npm i
RUN npm run build

# Now the image that we'll be used to run the app
FROM nginx:alpine as production

COPY --from=builder /app/build /usr/share/nginx/html
# nginx config from the host
COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
