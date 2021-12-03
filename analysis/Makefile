remote_host = cloud
remote_dir = ~/sites/stats/public_html/donors-crackdowns-aid
remote_dest = $(remote_host):$(remote_dir)

## serve	:	Serve _site/ directory at http://localhost:7000
.PHONY: serve
serve:
	Rscript -e "servr::httw(dir = '_site', port = 7000)"

## html	:	Render all the .Rmd files as a complete HTML site in _site/
.PHONY: html
html:
	Rscript -e "rmarkdown::render_site(encoding = 'UTF-8')"

## clean	:	Delete _site/
.PHONY: clean
clean:
	Rscript -e "rmarkdown::clean_site()"

## upload	:	Use rsync to upload _site/ to remote server
.PHONY: upload
upload:
	rsync -crvP --delete _site/ $(remote_dest)

# Self-documenting Makefiles from The Carpentries
# https://swcarpentry.github.io/make-novice/08-self-doc/index.html
## help	:	Show possible targets
.PHONY: help
help: Makefile
	@sed -n 's/^##//p' $<
