#!/bin/bash

set -x
set -e

: ${LISP:=sbcl-bin}

while ! mysql -u "$MYSQL_USER" \
        -h "$MYSQL_HOST" \
        -P "$MYSQL_PORT" \
        -p"$MYSQL_PASS" \
        -e 'CREATE DATABASE IF NOT EXISTS `cl-dbi`'; do \
      sleep 1
done

ros install "$LISP"
ros use "$LISP"
/root/.roswell/bin/run-prove dbi-test.asd
