// Build-time generator for the bundled GNU Smalltalk kernel index (US-413, AC1).
//
//   npm run gen:kernel-index [-- <kernel-dir>]
//
// Parses the bundled GST 3.2.5 kernel `.st` sources with our own parser and
// writes the dialect-neutral, facts-only index to server/data/kernel-index.json.
// The corpus lives OUTSIDE the repo and is absent in CI, so the JSON is a
// COMMITTED artifact — regenerate locally whenever the indexer or corpus changes
// (the kernelIndex.test.ts drift guard catches staleness when the corpus exists).

import fs from 'node:fs';
import path from 'node:path';
import {
  BUNDLED_HEADER,
  bundledIndexPath,
  bundledKernelDir,
  indexKernelDirectory,
  serializeKernelIndex,
} from '../server/src/kernel/indexer';

const dir = process.argv[2] ?? bundledKernelDir();

if (!fs.existsSync(dir)) {
  console.error(`gen-kernel-index: kernel directory not found: ${dir}`);
  console.error('Pass a kernel source directory, or place the corpus at ../smalltalk-3.2.5/kernel.');
  process.exit(1);
}

const data = indexKernelDirectory(dir, BUNDLED_HEADER);
const outPath = bundledIndexPath();
fs.mkdirSync(path.dirname(outPath), { recursive: true });
fs.writeFileSync(outPath, serializeKernelIndex(data), 'utf8');

console.log(
  `gen-kernel-index: wrote ${path.relative(process.cwd(), outPath)} ` +
    `(${data.library}: ${data.classCount} classes, ${data.selectorCount} selectors) from ${dir}`,
);
