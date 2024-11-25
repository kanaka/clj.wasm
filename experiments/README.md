# Experiments related to Clojure in WebAssembly

## Usage

* First, build a docker image with the tools:

```
docker build -t clj.wasm .
```

* Run the docker image with volume mount of the experiments directory:

```
cd experiments/
docker run -it --rm -v $(pwd):$(pwd) -w $(pwd) -e HOME=$(pwd) -u $(id -u):$(id -g) clj.wasm bash
```

* Install npm deps (in the clj.wasm container):

```
npm install
```

* Build and run the `trivial` example:

This example has a single exported `add` function that takes two
numbers, adds them, and returns the result.

From the docker container install deps, compile the wasm file, run the
JS file (that loads the wasm file):

```
make trivial.wasm
node trivial.js trivial.wasm
```

* Build and run the `strings-linear` example:

This example uses linear memory buffers for string input and output
but handles strings internally as GC'd character arrays `(array (mut
i8))`. The main function will call an imported `readline`  function
(which uses koffi/FFI to do full line edit input). Then it will
concat a prefix to the string and call the imported `printline` to print the result.

From the docker container install deps, compile the wasm file, run the
JS file (that loads the wasm file):

```
make strings/strings-linear.wasm
node strings/strings-linear.js strings/strings-linear.wasm
# waits for a line on input
```
