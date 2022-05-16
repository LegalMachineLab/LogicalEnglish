FROM swipl as base

RUN apt-get update && apt-get install -y \
    git build-essential autoconf curl unzip \
    cleancss node-requirejs

ENV SWISH_HOME /swish
# ENV SWISH_SHA1 V1.5.1

RUN echo "At version ${SWISH_SHA1}"
RUN git clone https://github.com/SWI-Prolog/swish.git && \
    cd swish
    # (cd swish && git checkout -q ${SWISH_SHA1})
RUN make -C /swish RJS="nodejs /usr/share/nodejs/requirejs/r.js" \
	yarn-zip packs min

FROM base
# LABEL maintainer "Jan Wielemaker <jan@swi-prolog.org>"

RUN apt-get update && apt-get install -y \
    graphviz imagemagick \
    git \
    wamerican && \
    rm -rf /var/lib/apt/lists/*

COPY --from=base /swish /swish
COPY entry.sh entry.sh

# FROM swipl/swish
LABEL maintainer = "galileo.sartor@gmail.com"
ENV SWISH_DIR /swish
ENV SWISH_DATA /data
ENV LOAD /app/swish/user_module_for_swish.pl
ENV LOAD_KB true

ENV MY_USER swishpersona
RUN addgroup --gid 1024 swishpersona; useradd -m -g $MY_USER -s /bin/bash $MY_USER; rm /entry.sh
COPY . /app/
COPY --chown=swishpersona swish/patches/prolog_server.js ${SWISH_DIR}/web/js/codemirror/mode/prolog/

RUN mkdir -p ${SWISH_DIR}/config-enabled 
COPY ./network.pl ${SWISH_DIR}/config-available/
COPY ./entry.sh /entry.sh

# Override with local sCASP
COPY it.pl ${SWISH_DIR}/pack/sCASP/prolog/scasp/lang/it.pl

# Swish likes to check existence of a /data directory:
WORKDIR ${SWISH_DATA}
ENTRYPOINT ["/entry.sh"]
