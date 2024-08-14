check: document
	R -s -e "devtools::check()"

document:
	R -s -e "devtools::document()"

install: check
	R -s -e "devtools::install()"
