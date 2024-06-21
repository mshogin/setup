#!/bin/bash

set -x

lockfile=/var/run/run-puppet.lock

if ( set -o noclobber; echo "$$" > "$lockfile") 2> /dev/null;
then
    trap 'rm -f "$lockfile"; exit $?' INT TERM EXIT

    /opt/puppetlabs/bin/puppet apply /etc/puppetlabs/code/environments/production/manifests/site.pp

    rm -f "$lockfile"
    trap - INT TERM EXIT
fi
