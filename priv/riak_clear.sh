#!/bin/bash

set -u
set -e
set -o pipefail

clear_logs_fn() {
    echo "Clearing all the logs"
    for i in `seq 1 8`;
    do
	DIR="/home/vagrant/riak/dev/dev${i}/log/*"
	echo "clearing ${DIR}"
	rm -f ${DIR}
    done
}

clear_dbs_fn() {
    echo ""
    echo "Clearing all the dbs"
    for i in `seq 1 8`;
    do
	DIR="/home/vagrant/riak/dev/dev${i}/data/leveldb/*"
	echo "clearing ${DIR}"
	rm -rf ${DIR}
    done
}


main() {
    clear_logs_fn
    clear_dbs_fn
}

main
