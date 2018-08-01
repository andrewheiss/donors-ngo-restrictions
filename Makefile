remote_host = cloud
remote_dir = ~/sites/stats/public_html/donors-ngo-restrictions
remote_dest = $(remote_host):$(remote_dir)

.PHONY: clean html upload

html:
	Rscript -e "rmarkdown::render_site(encoding = 'UTF-8')"

clean:
	Rscript -e "rmarkdown::clean_site()"

upload:
	rsync -crvP --delete _site/ $(remote_dest)
