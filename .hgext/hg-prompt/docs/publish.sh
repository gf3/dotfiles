#!/usr/bin/env bash

markdoc build
rsync --delete -az .html/ ~/src/sjl.bitbucket.org/hg-prompt
hg -R ~/src/sjl.bitbucket.org commit -Am 'hg-prompt: Update documentation.'
hg -R ~/src/sjl.bitbucket.org push
