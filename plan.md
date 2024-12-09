# Tentative Plan of Attack

Question: Can we achieve full first class support for WebAssembly
(e.g. eval, macros, runtime ns/class loading, full library support,
etc.)?

Here is a general / tentative plan of attack for answering that
question. Most of the following tasks can be worked in parallel
although they follow a rough dependency and increasing complexity
order.

* Explore new WebAssembly Features (GC, ref types, bulk mem, etc)
  * Create babashka program to extract ENBF from wasm spec
    * Partially completed [here](spec-tools/spec-extract.cljs)
      * Unfortunately, the spec document is more rendering centric
        than I hoped so extracting an EBNF grammar reliably is
        difficult.
  * Update [wam](https://github.com/kanaka/wam)
    * Convert from JS to Clojure/babashka (maybe waet-based?)
    * Use extracted spec EBNF
  * Create wasmgc mal implementation using wam

* Convert ClojureScript [compiler](https://github.com/clojure/clojurescript/blob/master/src/main/clojure/cljs/compiler.cljc) to emit wat.

* Port Clojure's immutable data structures to WasmGC
  * [Clojure](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/) (files `Persistent*.java`)
  * [ClojureScript](https://github.com/clojure/clojurescript/blob/master/src/main/cljs/cljs/core.cljs) (search for `;;; Persistent`)
  * Benchmarks comparison (CPU and mem)

* Integrate with WebAssembly Components / WIT system

* Implement dynamic recompile/relink (eval / JIR)

* Self-hosted / bootstrapped
