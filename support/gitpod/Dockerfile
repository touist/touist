FROM gitpod/workspace-full:latest

USER root
# Install custom tools, runtime, etc.

RUN add-apt-repository ppa:avsm/ppa && apt-get update && apt-get install -y opam \
    && apt-get clean && rm -rf /var/cache/apt/* && rm -rf /var/lib/apt/lists/* && rm -rf /tmp/*

USER gitpod
RUN opam init -yq --disable-sandboxing
RUN opam install touist -y --deps-only \
    && opam install -y merlin utop
RUN echo 'test -r ~/.opam/opam-init/init.sh && . ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true' >> ~/.bashrc
