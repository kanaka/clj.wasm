# An Incomplete Chronology of Clojure WebAssembly Events

* 2018-10-16 - Clojurians #wasm channel created

* 2024-10-23 - Clojure/conj Unsession

Unsession summary:

3:15-3:55pm in the Conference Room. There were perhaps 30-50 people in attendance. Joel Martin proposed the unsession to spur discussion. It was a mostly free form discussion "round table" with no formal presentation. The following was discussed during my intro and subsequent discussion (although not strictly in this order): The WebAssembly Garbage Collection (WasmGC) standard is now enabled by default in most browsers and JS engines (node, deno). This was the major missing capability that prevented Clojure from being ported to Wasm. The WasmGC standard is design to enable languages that require GC such as Java, Go, Dart, Kotlin, etc to natively support the wasm platform and to interoperate with other WasmGC languages. The wasm platform is building momentum and all the languages mentioned above already have implementations targeting WasmGC at various stages of maturity.

Wasm seems like it has a chance at becoming a true universal runtime (and avoiding many pitfalls of earlier attempts at this). All other major GC languages seem like they either have or will soon have support for WasmGC. Now seems like the right time to bring Clojure to wasm. We might even be a bit late already. But we should start exploring this now and also give feedback to the standards work to make sure that wasm works well for languages like Clojure too.

I (Joel Martin) would personally love to see full/first class support for wasm (eval, macros, runtime ns/class loading, all popular libraries, etc). However, the path to get there still has many unanswered questions. We will probably need to start with something more like the  ClojureScript model. Replacing the ClojureScript JS emitter and porting Clojure's efficient/immutable data structures seems like it might be an early phase 1.

* 2024-11-25

Create https://github.com/kanaka/clj.wasm repo.

Includes two WAT experiments:
* Trivial add example
* Strings example that uses linear memory for I/O and handles strings
  internall using GC'd character arrays.
