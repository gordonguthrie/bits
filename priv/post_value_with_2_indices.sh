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

# PORT=10018
PORT=8098

BUCKET=$1
KEY=$2
VALUE=$3
INDEXNAME1=$4
INDEXVAL1=$5
INDEXNAME2=$6
INDEXVAL2=$7
echo "posting ${VALUE} to ${BUCKET} with key of ${KEY} on port ${PORT} with an index ${INDEXVAL1} in index ${INDEXNAME1} and index ${INDEXVAL2} in ${INDEXNAME2}"
URL="http://127.0.0.1:${PORT}/buckets/${BUCKET}/keys/${KEY}?returnbody=true "
CT=" -H 'Content-Type: application/json'"
HD1="-H 'x-riak-index-${INDEXNAME1}_bin: ${INDEXVAL1}'"
HD2="-H 'x-riak-index-${INDEXNAME2}_bin: ${INDEXVAL2}'"
VL="-d ${VALUE}"
CMD="curl -v -XPOST ${CT} ${HD1} ${HD2} ${VL} ${URL}"
echo ${CMD}
eval ${CMD}
echo ""

