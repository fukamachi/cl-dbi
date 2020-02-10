FROM 40ants/base-lisp-image:0.9.0-sbcl-bin as base

RUN ros install rove
RUN apt-get update && \
    apt-get install -y libmysqlclient20 mysql-client && \
    ln -s /usr/lib/x86_64-linux-gnu/libmysqlclient.so.20 /usr/lib/x86_64-linux-gnu/libmysqlclient_r.so

COPY . /app
WORKDIR /app

ENV CL_SOURCE_REGISTRY "/app/"

CMD /app/docker/entrypoint.sh
