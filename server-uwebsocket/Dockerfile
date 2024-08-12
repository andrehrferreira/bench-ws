FROM node:lts

WORKDIR /usr/src/app

COPY package.json ./

RUN npm install --production

COPY . .

EXPOSE 3002

CMD ["node", "server.js"]
