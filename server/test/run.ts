// Entry point for the parser test suites (lexer + parser), so a single
// `npm run test:parser [-- --update]` runs both and shares process.argv.
import './lexer.test.ts';
import './parser.test.ts';
import './container.test.ts';
import './chunk.test.ts';
import './symbols.test.ts';
import './kernel.test.ts';
import './cartridgeLoader.test.ts';
import './kernelService.test.ts';
import './providers.test.ts';
