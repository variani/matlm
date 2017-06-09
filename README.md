# matlm

[![travis-ci build status](https://travis-ci.org/variani/matlm.svg?branch=master)](https://travis-ci.org/variani/matlm)

![](docs/figures/timing-matlm-vs-lm.png)

## About

The `matlm` R package fits linear models in matrix form and avoids calling `lm`.
That makes computation efficient if many predictors need to be tested 
and calling `lm` for every marker.
