#!/bin/bash

git clone git@github.com:jeksterslab/dynUtils.git
rm -rf "$PWD.git"
mv dynUtils/.git "$PWD"
rm -rf dynUtils
