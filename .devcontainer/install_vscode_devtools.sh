#!/usr/bin/env bash

# Abort script if any command exits with non-zero status. Not foolproof.
set -e 

NCPUS=${NCPUS:-"-1"}

apt-get update -qq
apt-get -y --no-install-recommends install \
  git \
  htop \
  libfontconfig1-dev \
  libfribidi-dev \
  libgit2-dev \
  libharfbuzz-dev \
  pipx \
  shellcheck

rm -rf /var/lib/apt/lists/*

PIPX_BIN_DIR=/usr/local/bin pipx install radian

install2.r --error --skipinstalled -n $NCPUS \
    covr \
    devtools \
    languageserver \
    lobstr \
    microbenchmark \
    profvis \
    quarto \
    rlang

# httpgd is currently off of CRAN because of c++ compiler conflicts
# https://github.com/nx10/httpgd/issues/218

R -e "remotes::install_github('nx10/httpgd')"
