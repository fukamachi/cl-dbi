#!/bin/bash

set -x
set -e

while ! mysql -u "$MYSQL_USER" \
        -h "$MYSQL_HOST" \
        -P "$MYSQL_PORT" \
        -p"$MYSQL_PASS" \
        -e 'CREATE DATABASE IF NOT EXISTS `cl-dbi`'; do \
      sleep 1
done

rove dbi.asd
