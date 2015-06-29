#!/bin/bash

# PORT=10018
PORT=10018

echo "Getting list of buckets"
wget -q -O -"$@" 127.0.0.1:${PORT}/buckets?buckets=true
echo ""
