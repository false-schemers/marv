/* Run a WCPL/WASI-built MARV with private preopened source and output trees. */
const fs = require('fs');
const { WASI } = require('wasi');
const wasi = new WASI({
  version: 'preview1',
  args: ['marv', '-T', '/repo/target', '-t', 'cc', '-c',
         '-o', '/out/wasi-data.c', '/repo/test/native-data.mvs'],
  env: {},
  preopens: { '/repo': process.env.MARV_TEST_ROOT, '/out': process.env.MARV_TEST_OUT },
  returnOnExit: true
});
const wasmModule = new WebAssembly.Module(fs.readFileSync(process.argv[2]));
const instance = new WebAssembly.Instance(wasmModule, { wasi_snapshot_preview1: wasi.wasiImport });
process.exit(wasi.start(instance));
