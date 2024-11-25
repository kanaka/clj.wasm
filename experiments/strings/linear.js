import { readFileSync } from 'fs'
import { TextEncoder, TextDecoder } from 'util'
import node_readline from './node_readline.js'
import sourceMapSupport from 'source-map-support'

const wasmPath = process.argv[2]
const wasmBytes = readFileSync(wasmPath)
const wasmMapBytes = readFileSync(wasmPath+'.map', 'utf8')

sourceMapSupport.install({
  retrieveSourceMap(source) {
    if (source.startsWith('wasm://wasm/')) {
      return { url: source, map: wasmMapBytes }
    }
    return null
  }
})

const encoder = new TextEncoder()
const decoder = new TextDecoder()

// Create memory and read length-prefixed string
const getString = (memory, ptr) => {
  const len = new Int32Array(memory.buffer)[ptr / 4]
  const bytes = new Uint8Array(memory.buffer, ptr + 4, len)
  return decoder.decode(bytes)
}

// Write length-prefixed string
const putString = (memory, ptr, str) => {
  const bytes = encoder.encode(str)
  new Int32Array(memory.buffer)[ptr / 4] = bytes.length
  new Uint8Array(memory.buffer, ptr + 4).set(bytes)
}

const readline = (wasm) => {
  const prompt = getString(wasm.instance.exports.memory, 65536)
  const result = node_readline.readline(prompt)
  putString(wasm.instance.exports.memory, 0, result)
}

const printline = (wasm) => {
  const str = getString(wasm.instance.exports.memory, 65536)
  console.log(str)
}

const readfile = (wasm) => {
  const path = getString(wasm.instance.exports.memory, 65536)
  const result = fs.readFileSync(path)
  putString(wasm.instance.exports.memory, 0, result)
}


const wasm = await WebAssembly.instantiate(
  wasmBytes,
  {
    console: {
      readline: () => readline(wasm),
      printline: () => printline(wasm)
    },
    fs: {
      readfile: () => readfile(wasm)
    }
  }
)

wasm.instance.exports.main()
