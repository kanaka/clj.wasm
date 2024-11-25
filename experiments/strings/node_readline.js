// IMPORTANT: choose one
const RL_LIB = "libreadline.so"; // NOTE: libreadline is GPL
//const RL_LIB = "libedit.so";

import path from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';
import koffi from 'koffi';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const HISTORY_FILE = path.join(process.env.HOME, '.node-readline-history');

const rlwrap = {}; // namespace for this module in web context

const koffi_rl = koffi.load(RL_LIB);

const rllib = {
  readline: koffi_rl.func("char *readline(char *prompt)"),
  add_history: koffi_rl.func("int add_history(char *line)"),
};

let rl_history_loaded = false;

export function readline(prompt = "user> ") {
  if (!rl_history_loaded) {
    rl_history_loaded = true;
    let lines = [];
    if (fs.existsSync(HISTORY_FILE)) {
      lines = fs.readFileSync(HISTORY_FILE).toString().split("\n");
    }
    // Max of 2000 lines
    lines = lines.slice(Math.max(lines.length - 2000, 0));
    for (const line of lines) {
      if (line) {
        rllib.add_history(line);
      }
    }
  }

  const line = rllib.readline(prompt);
  if (line) {
    rllib.add_history(line);
    try {
      fs.appendFileSync(HISTORY_FILE, line + "\n");
    } catch (exc) {
      // ignored
    }
  }

  return line;
}

export default { readline };
