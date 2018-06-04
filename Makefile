b: build
build:
	@esy jbuilder build --dev

build-release:
	@esy build

clean:
	@esy jbuilder clean
