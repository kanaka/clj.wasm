FROM ubuntu:24.04
LABEL maintainer="Joel Martin github@martintribe.org"

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

RUN apt-get -y update

# Required for running tests
RUN apt-get -y install make python3
RUN ln -fs /usr/bin/python3 /usr/local/bin/python

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN DEBIAN_FRONTEND=noninteractive \
    apt-get -y install libreadline-dev libedit-dev gnupg curl

# Add nodesource apt repo 23.x
RUN curl -sL https://deb.nodesource.com/setup_23.x | bash -
RUN apt-get -y install nodejs

RUN mkdir -p /opt && \
    cd /opt && \
    curl -L https://github.com/WebAssembly/binaryen/releases/download/version_120_b/binaryen-version_120_b-x86_64-linux.tar.gz | tar xvzf -

ENV PATH=$PATH:/opt/binaryen-version_120_b/bin

ENV HOME=/mal
