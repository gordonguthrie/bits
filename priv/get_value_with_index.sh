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
# PORT=8098

BUCKET=$1
INDEXNAME=$2
INDEXVAL=$3
echo "getting from ${BUCKET} on port ${PORT} with an index ${INDEXVAL} in index ${INDEXNAME}"
URL="http://127.0.0.1:${PORT}/buckets/${BUCKET}/index/${INDEXNAME}_bin/${INDEXVAL}"
CMD="curl -v ${URL}"
echo ${CMD}
eval ${CMD}
echo ""
