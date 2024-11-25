# Clojure for WebAssembly

Resources and experiments to explore the feasibility of WebAssembly as
a full Clojure target/platform.

## Rationale

With the recent finalization of Garbage Collection (WasmGC) and
related standards, WebAssembly has the potential to become a true
universal runtime (avoiding many of the pitfalls of earlier attempts
at this goal). Other major GC languages seem like they either have or
will soon have a backend targeting WebAssembly. Now seems like the
right time to bring full WebAssembly support to Clojure. We should
start exploring this now and also give feedback to the standards work
to make sure that Wasm works well for languages like Clojure.

## Links

High relevance:

* "A new way to bring garbage collected programming languages
  efficiently to WebAssembly" (Alon Zakai):
  https://v8.dev/blog/wasm-gc-porting
* "just-in-time code generation within webassembly":
  https://wingolog.org/archives/2022/08/18/just-in-time-code-generation-within-webassembly 
* Hoot: Scheme on WebAssembly (specifically Wasm GC):
  https://spritely.institute/hoot/
  * Guile on WebAssembly ABI:
    https://gitlab.com/spritely/guile-hoot/-/blob/main/design/ABI.md
  * Type definitions:
    https://gitlab.com/spritely/guile-hoot/-/blob/main/module/wasm/types.scm

General:

* https://github.com/webassembly
* https://webassembly.org/
* "Nonprofit creating secure new SW foundation on WebAssembly and WASI"
  https://bytecodealliance.org/

Tutorials / HOWTOs:

* Learning WebAssembly Series:
  https://blog.ttulka.com/learning-webassembly-series/
* Wasm Intro
  * part 1 - binary format:
    https://coinexsmartchain.medium.com/wasm-introduction-part-1-binary-format-57895d851580
  * part 2 - Instruction Set & Operand Stack:
    https://coinexsmartchain.medium.com/wasm-introduction-part-2-instruction-set-operand-stack-38e5171b52e6
  * part 3 - Memory:
    https://coinexsmartchain.medium.com/wasm-introduction-part-3-memory-7426f19c9624
  * part 4 - Function Call:
    https://coinexsmartchain.medium.com/wasm-introduction-part-4-function-call-9ddf62272f15
  * part 5 - Control Instructions:
    https://coinexsmartchain.medium.com/wasm-introduction-part-5-control-instructions-1cc21a180618
  * part 6 - Table & Indirect Call:
    https://coinexsmartchain.medium.com/wasm-introduction-part-6-table-indirect-call-65ad0404b003
  * part 7 - Text Format:
    https://coinexsmartchain.medium.com/wasm-introduction-part-7-text-format-2d608e50daab

Standardization:

* Features/standards support across browsers and engines:
  https://webassembly.org/features/
* proposal status/phase listing:
  https://github.com/WebAssembly/proposals

Standards:

* Garbage collection (WasmGC):
  https://github.com/WebAssembly/gc/blob/main/proposals/gc/Overview.md
  * final proposal for prototyping (with lots of tables and diagrams):
    https://docs.google.com/document/d/1DklC3qVuOdLHSXB5UXghM_syCh-4cMinQ50ICiXnK3Q/edit?tab=t.0
  * v1:
    https://github.com/WebAssembly/gc/blob/main/proposals/gc/MVP.md
  * post-v1:
    https://github.com/WebAssembly/gc/blob/main/proposals/gc/Post-MVP.md
  * New types and instructions in WasmGC:
    https://tanishiking.github.io/posts/wasm-gc/
  * Discussion of JS interop (in context of GC types):
    https://docs.google.com/document/d/17hCQXOyeSgogpJ0I0wir4LRmdvu4l7Oca6e1NkbVN8M/edit?tab=t.0#heading=h.jnkwn2nuqgbi
  * GC story for strings?
    https://github.com/WebAssembly/gc/issues/145    
* Reference types:
  https://github.com/WebAssembly/reference-types/blob/master/proposals/reference-types/Overview.md
* Type function references:
  https://github.com/WebAssembly/function-references/blob/master/proposals/function-references/Overview.md
* Type import/export:
  https://github.com/WebAssembly/proposal-type-imports/blob/main/proposals/type-imports/Overview.md
* Bulk memory and conditional segment initialization:
  https://github.com/WebAssembly/bulk-memory-operations/blob/master/proposals/bulk-memory-operations/Overview.md
* JS String builtins:
  https://github.com/WebAssembly/js-string-builtins/blob/main/proposals/js-string-builtins/Overview.md
* Multiple memories:
  https://github.com/WebAssembly/multi-memory/blob/master/proposals/multi-memory/Overview.md
* Import/export mutable globals:
  https://github.com/WebAssembly/mutable-global/blob/master/proposals/mutable-global/Overview.md
* WebAssembly System Interface:
  https://github.com/WebAssembly/WASI
* Component model (language interop):
  https://component-model.bytecodealliance.org/

Projects:

* binaryen (Optimizer and compiler/toolchain library for WebAssembly):
  https://github.com/WebAssembly/binaryen
  * Discussion of GC support in binaryen (2020):
    https://github.com/WebAssembly/binaryen/issues/2935
* wabt (WebAssembly binary toolkit):
  https://github.com/WebAssembly/wabt
* wasmtime (fast and secure runtime for WebAssembly):
  https://github.com/bytecodealliance/wasmtime
* wasmer (Wasm Runtime supporting WASIX and WASI)
  https://github.com/wasmerio/wasmer
* WebAssembly module decoder in C++:
  https://github.com/WebAssembly/wasp
* Native JVM WebAssembly runtime:
  https://github.com/dylibso/chicory
* Javy: JS to WebAssembly toolchain:
  https://github.com/bytecodealliance/javy

Other/misc:

* ClojureScript compiler (core and emitter):
  https://github.com/clojure/clojurescript/blob/master/src/main/clojure/cljs/compiler.cljc
* Node.js with WebAssembly:
  https://nodejs.org/en/learn/getting-started/nodejs-with-webassembly
* Spec compliant WebAssembly compiler, decompiler, and generator (initial prototype work):
  https://github.com/helins/wasm.cljc
* WebAssembly numbers are encoded in the binary format as LEB128:
  https://en.wikipedia.org/wiki/LEB128

Other languages/implementations:

* Go:
  https://go.dev/wiki/WebAssembly
  * Go support ticket (GC limitations):
    https://github.com/WebAssembly/gc/issues/59
* Kotlin:
  https://kotlinlang.org/docs/wasm-overview.html
  * Kotlin/Wasm examples:
    https://github.com/Kotlin/kotlin-wasm-examples
  * Exploring WAT Files Generated from Kotlin/Wasm:
    https://tanishiking.github.io/posts/kotlin-wasm-deep-dive/
* Scala:
  * Initial PR:
    https://github.com/scala-js/scala-js/pull/4988/files#diff-bf82a844d8401f39d51214462f130733e1de5c3605d814b6fe72c053f4c7097a 
  * scala-wasm (now integrated into main Scala repo):
    https://github.com/tanishiking/scala-wasm
* Graal:
  * Wasm target architecture for native image:
    https://github.com/oracle/graal/issues/3391
* Python:
  * py2wasm blog post:
    https://wasmer.io/posts/py2wasm-a-python-to-wasm-compiler
* Java:
  * Java to Closure JavaScript transpiler:
    https://github.com/google/j2cl
    * Getting started for J2CL/Wasm (Experimental):
      https://github.com/google/j2cl/blob/master/docs/getting-started-j2wasm.md
* Schism (Self-hosting Scheme to WebAssmebly compiler):
  https://github.com/schism-lang/schism
  * reference types and tail calls but not GC
* Flutter:
  https://docs.flutter.dev/platform-integration/web/wasm
  * Uses GC with fallback to JS backend
