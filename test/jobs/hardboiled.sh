#!/bin/bash
set -e

interrupted()
{
    echo "nah"
}

trap interrupted SIGTERM
echo "$$ chillin"
echo -n $$ > hardboiled.pid
while true; do sleep 1; done
