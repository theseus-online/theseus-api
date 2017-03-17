FROM frolvlad/alpine-glibc

RUN apk update && apk add gmp-dev
ADD .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/theseus-api-exe/theseus-api-exe /usr/local/bin/theseus-api

CMD /usr/local/bin/theseus-api
