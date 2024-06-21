#!/bin/bash

set -e

sudo apt-get update
sudo apt-get install -y ruby-full curl
sudo gem install r10k

rm -f /tmp/puppet.deb
curl https://apt.puppet.com/puppet7-release-focal.deb --output /tmp/puppet.deb
sudo dpkg -i /tmp/puppet.deb
sudo apt-get update
sudo apt-get install -y puppet-agent

sudo mv -f /etc/puppetlabs/code/environments/production /etc/puppetlabs/code/environments/production.orig1

sudo ln -s `pwd`/puppet /etc/puppetlabs/code/environments/production
