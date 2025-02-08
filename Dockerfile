FROM ocaml/opam:debian-ocaml-5.1
RUN sudo apt update && sudo apt install -y m4 libev-dev
WORKDIR /app
COPY . .
RUN opam install dune lwt
CMD ["dune", "build"]
