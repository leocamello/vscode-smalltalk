// Entry point for the parser test suites (lexer + parser), so a single
// `npm run test:parser [-- --update]` runs both and shares process.argv.
import './lexer.test.ts';
import './parser.test.ts';
