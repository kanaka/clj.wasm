import { readFileSync } from 'fs'

const wasmBytes = readFileSync(process.argv[2])
const wasm = await WebAssembly.instantiate(wasmBytes, {})
console.log("(add 7 8) = ", wasm.instance.exports.add(7, 8))
