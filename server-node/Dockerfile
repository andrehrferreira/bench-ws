FROM node:lts

WORKDIR /usr/src/app

COPY package.json ./

RUN npm install --production

COPY . .

EXPOSE 3001

CMD ["node", "server.js"]
