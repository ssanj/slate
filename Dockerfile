FROM haskell:8.8.4-buster

WORKDIR /app

RUN mkdir -p /app/build/stack-root

RUN mkdir -p /app/build/work-dir

ADD . .
