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

From the docker container install deps, compile the wasm file, run the
JS file (that loads the wasm file):

```
make trivial.wasm
node trivial.js trivial.wasm
```
