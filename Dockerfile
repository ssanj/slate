FROM haskell:8.8.4-buster

WORKDIR /app

MKDIR /app/build/stack-root

MKDIR /app/build/work-dir

ADD . .
