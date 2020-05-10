RMDS:=$(wildcard *.Rmd)
HTML:=$(patsubst %.Rmd, %.html,$(RMDS))
.PRECIOUS: %.Rhtml

all: $(HTML)

%.Rhtml: %.Rmd
	sed 's/```{r}/<!--begin.rcode/; s/```/end.rcode-->/; s/`r \(.*\)`/<!--rinline \1 -->/' $^ | pandoc -o $@ --to=html5 --mathjax --highlight-style=tango --standalone

%.html: %.Rhtml
	Rscript -e "require(knitr); knit('$^')"

clean:
	rm -f $(HTML) *.Rhtml figure/*
	rmdir figure
