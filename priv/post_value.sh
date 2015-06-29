#!/bin/bash

## Try and make the bash work safely
# Terminate on unitiialised variables
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
KEY=$2
VALUE=$3
echo "posting ${VALUE} to ${BUCKET} with key of ${KEY} on port ${PORT}"
curl -v http://127.0.0.1:${PORT}/buckets/${BUCKET}/keys/${KEY} \
	-H "Content-Type: text/plain" -d ${VALUE}
echo ""
