FROM maven:3.8.7-eclipse-temurin-17 AS build

WORKDIR /app

COPY pom.xml ./

RUN mvn dependency:go-offline

COPY src ./src

RUN mvn package

FROM eclipse-temurin:17-jre-alpine

WORKDIR /app

COPY --from=build /app/target/server-1.0-SNAPSHOT-jar-with-dependencies.jar /app/server.jar

EXPOSE 3012

CMD ["java", "-jar", "/app/server.jar"]
