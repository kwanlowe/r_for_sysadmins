#!/bin/sh
set -x

ssh $1 "LC_ALL=C sar -A -f /var/log/sa/sa${2}"
