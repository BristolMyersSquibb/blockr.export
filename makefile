check: document
	R -s -e "devtools::check()"

document:
	R -s -e "devtools::document()"
