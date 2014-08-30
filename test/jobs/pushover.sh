#!/bin/bash
set -e

interrupted()
{
    echo "ok fine"
    exit 0;
}

trap interrupted SIGTERM
echo "$$ chillin"
echo -n $$ > pushover.pid
while true; do sleep 1; done
