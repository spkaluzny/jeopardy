#!/bin/sh
R --no-save --no-restore << GOSYSIN
rmarkdown::render("jeopardy.Rmd")
GOSYSIN
