remote_host = cloud
remote_dir = ~/sites/stats/public_html/donors-ngo-restrictions
remote_dest = $(remote_host):$(remote_dir)

.PHONY: upload

upload:
	rsync -P -rvzcv --delete --files-from=upload_files.txt . $(remote_dest)
