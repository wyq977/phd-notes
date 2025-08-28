# Makefile: format all .tex files (including subfolders) quietly using latexindent

TEXFILES := $(shell find . -name "*.tex")

.PHONY: all format clean

all: format

format:
	@for f in $(TEXFILES); do \
		echo "Formatting $$f"; \
		latexindent -w -s "$$f"; \
	done

clean:
	@echo "Removing backup files..."
	@find . -name "*.bak" -type f -delete
