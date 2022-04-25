#!/bin/bash -ex

if ! test -e bin/bb; then
   bin/install-bb
fi

bin/bb publish
