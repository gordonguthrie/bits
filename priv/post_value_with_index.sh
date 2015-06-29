#!/bin/bash

## Try and make the bash work safely
# Terminate on unitialised variables
set -u
# Terminate on error
set -e
# Terminate if any part of a pipeline fails
set -o pipefail

# Uncomment for debugging
# set -x

PORT=10018
#PORT=8098

BUCKET=$1
KEY=$2
VALUE=$3
INDEXNAME=$4
INDEXVAL=$5
echo "posting ${VALUE} to ${BUCKET} with key of ${KEY} on port ${PORT} with an index ${INDEXVAL} in index ${INDEXNAME}"
URL="http://127.0.0.1:${PORT}/buckets/${BUCKET}/keys/${KEY}?returnbody=true "
CT=" -H 'Content-Type: application/json'"
HD="-H 'x-riak-index-${INDEXNAME}_bin: ${INDEXVAL}'"
VL="-d ${VALUE}"
CMD="curl -v -XPOST ${CT} ${HD} ${VL} ${URL}"
echo ${CMD}
eval ${CMD}
echo ""
