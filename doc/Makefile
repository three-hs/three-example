
UML_FILES = $(shell find . -name "*.uml")
PNG_FILES = $(UML_FILES:.uml=.png)
SVG_FILES = $(UML_FILES:.uml=.svg)

.PHONY: all clean

all: $(PNG_FILES) ${SVG_FILES}

%.png: %.uml
	plantuml -charset UTF-8 -DPLANTUML_LIMIT_SIZE=32000 -Tpng $<

%.svg: %.uml
	plantuml -charset UTF-8 -DPLANTUML_LIMIT_SIZE=32000 -Tsvg $<

clean:
	rm -f $(PNG_FILES) ${SVG_FILES}

