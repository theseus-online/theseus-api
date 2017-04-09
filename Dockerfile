FROM theseus/base-image

ADD .stack-work/dist/x86_64-linux*/Cabal-*/build/theseus-api-exe/theseus-api-exe /usr/local/bin/theseus-api

CMD /usr/local/bin/theseus-api
