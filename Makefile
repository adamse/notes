.PHONY: img.ppm
img.ppm:
	cargo build --release
	cargo run --release

.PHONY: tags
tags:
	ctags -f tags --options=ctags.rust --recurse .

