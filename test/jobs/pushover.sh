#!/bin/bash
set -e

interrupted()
{
    echo "ok fine"
    exit 0;
}

trap interrupted SIGTERM
echo "chillin"
while true; do sleep 1; done
