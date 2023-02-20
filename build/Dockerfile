FROM ${BASE_IMAGE} AS build

RUN apt-get update && apt-get install -y lsb-release sudo

ENV USER=root

COPY ebuild /ebuild

RUN /ebuild --version ${EMACS_VERSION} --build local --native-compilation ${NATIVE_COMPILATION} 

# #---------------------------------------------------------------------------------------------#

# FROM ${BASE_IMAGE}

# COPY ebuild /ebuild

# RUN apt-get update && apt-get install -y lsb-release sudo


# RUN groupadd -g 1000 ${USER}
# RUN useradd -l -m -u 1000 -g 1000 ${USER}
# #RUN groupadd -g ${GID} ${USER}
# #RUN useradd -l -m -u ${UID} -g ${GID} ${USER}
# RUN adduser ${USER} sudo
# RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
# RUN chown --changes --silent --no-dereference --recursive ${UID}:${GID} /home/${USER}
# USER ${USER}
# WORKDIR /home/${USER}
# RUN /ebuild --dependencies-only ${DEBIAN_VERSION}

# COPY --from=build /usr/local /usr/local

# CMD ["emacs", "--debug-init"]
