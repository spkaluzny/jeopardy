#!/bin/sh
if [ $# -eq 0 ]; then
    FILE=jeopardy.Rmd
fi

OPTIONS=$@
for OPTION in "$@"; do
    case "$OPTION" in
        -s*)
            FILE=jeopardy.Rpres
        ;;
        -r*)
            FILE=jeopardy_rmdshower.Rmd
        ;;
        * )  # no options
    esac
done

R --no-save --no-restore << GOSYSIN
rmarkdown::render("$FILE")
GOSYSIN
