import esbuild from 'esbuild';

const production = process.argv.includes('--production');
const watch = process.argv.includes('--watch');

/** @type {import('esbuild').BuildOptions} */
const common = {
  bundle: true,
  format: 'cjs',
  platform: 'node',
  target: 'node18',
  // `vscode` is provided by the host at runtime; everything else (the LSP
  // libraries) is bundled so the VSIX runs without node_modules.
  external: ['vscode'],
  sourcemap: !production,
  minify: production,
  logLevel: 'info',
};

const entries = [
  { entryPoints: ['client/src/extension.ts'], outfile: 'dist/extension.js' },
  { entryPoints: ['server/src/server.ts'], outfile: 'dist/server.js' },
];

async function main() {
  const contexts = await Promise.all(
    entries.map((e) => esbuild.context({ ...common, ...e })),
  );
  if (watch) {
    await Promise.all(contexts.map((c) => c.watch()));
    console.log('[esbuild] watching for changes…');
  } else {
    await Promise.all(contexts.map((c) => c.rebuild()));
    await Promise.all(contexts.map((c) => c.dispose()));
    console.log('[esbuild] build complete');
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
