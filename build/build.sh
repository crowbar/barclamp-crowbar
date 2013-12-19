#!/bin/bash

bc_needs_build() {
    return 0 # yes
}

bc_build() {
    sudo pip install pip2pi
    $BC_DIR/build/build.rb
}
