live: resetdistfolder
	elm-live src/Main.elm -- --output dist/index.js

test-watch:
	elm-test --watch tests/ProgressTest.elm

# Use this to create the soundsprite. Not require for CI, just run each time the sounds are changed and then commit.
audio:
	cd sounds && audiosprite --output audiosprite -b 32 -r 24000 -f howler --export mp3 prepare.mp3 go.mp3 rest.mp3 allfinished.mp3

resetdistfolder:
	rm -rf dist
	mkdir dist
	cp assets/* dist

netlify: resetdistfolder
	npm i -g elm uglify-js
	elm make src/Main.elm --optimize --output dist/index.full.js
	uglifyjs dist/index.full.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=dist/index.js

phony: live text-watch audio resetdistfolder netlify