#!/usr/bin/env bash
set -e
case $TESTS in
    chef)
        echo "Setting up test environment for Chef cookbooks..."
        pushd chef/cookbooks/crowbar
        bundle install
        ;;
    webui)
        echo "Setting up test environment for the WebUI..."
        pushd crowbar_framework
        bundle install
        bundle exec rake db:migrate
        sed -i -e "s#use_bundler\:\ false#use_bundler\:\ true#" config/app_config.yml
        cp config/catalog.yml.example config/catalog.yml
        ;;
esac
popd
echo "Test setup complete."
