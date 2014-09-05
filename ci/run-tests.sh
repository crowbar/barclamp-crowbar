#!/usr/bin/env bash

set -e
case $TESTS in
    chef)
        echo "Running Chef cookbooks tests..."
        pushd chef/cookbooks/crowbar
        bundle exec rspec spec --format doc --color
        ;;
    webui)
        echo "Running WebUI tests..."
        pushd crowbar_framework
        bundle exec rake spec
        bundle exec rake brakeman:run
        ;;
esac
popd
echo "Tests run complete."
