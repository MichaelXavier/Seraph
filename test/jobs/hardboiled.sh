#!/bin/bash
set -e

interrupted()
{
    echo "nah"
}

trap interrupted SIGTERM
echo "chillin"
while true; do sleep 1; done
