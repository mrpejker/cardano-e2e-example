FROM node:14-alpine

EXPOSE 3000

WORKDIR /app

COPY package.json ./
COPY public/ ./public/
COPY scripts/ ./scripts/
COPY config/ ./config/
COPY src/ ./src
COPY tsconfig.json ./
COPY .env ./

RUN npm i
CMD npm start