img.ppm:
	cargo build --release
	cargo run --release

tags:
	ctags -f tags --options=ctags.rust --recurse .

