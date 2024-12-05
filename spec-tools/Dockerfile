FROM paulgauthier/aider-full:latest AS aider


FROM aider AS tools

RUN apt-get -y update

# Add nodesource apt repo 23.x
RUN curl -sL https://deb.nodesource.com/setup_23.x | bash -
RUN apt-get -y install nodejs

RUN apt-get -y install openjdk-17-jre-headless curl

RUN cd /usr/local/bin/ && \
    curl -L https://github.com/babashka/babashka/releases/download/v1.12.195/babashka-1.12.195-linux-amd64.tar.gz | tar xvzf - && \
    mv /usr/local/bin/bb /usr/local/bin/bb-real && \
    echo '#!/usr/bin/sh\nbb-real -Duser.home=$HOME "${@}"' > /usr/local/bin/bb && \
    chmod +x /usr/local/bin/bb

# TODO: Move up with curl
RUN apt-get -y install direnv
# TODO: move up with nodejs
RUN apt-get -y install npm

FROM tools AS user

ARG USER_ID=1000
ARG GROUP_ID=1000

RUN groupadd -g ${GROUP_ID} developer && \
    useradd -m -u ${USER_ID} -g ${GROUP_ID} developer

ENV HOME=/home/developer
ENV USER=developer
USER developer

