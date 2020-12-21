.PHONY: img.ppm
img.ppm: build
	cargo run --release

.PHONY: build
build:
	cargo build --release

.PHONY: tags
tags:
	ctags -f tags --options=ctags.rust --recurse .

.PHONY: perf
perf: build
	perf record --call-graph=dwarf target/release/ray
