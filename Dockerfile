FROM haskell

RUN stack update --resolver=lts-8.18
RUN apt-get update
RUN apt-get install -y libpq-dev

COPY . /freewill
WORKDIR /freewill
RUN stack build --resolver=lts-8.18

RUN echo done
