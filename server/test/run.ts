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
import './diagnostics.test.ts';
import './codeAction.test.ts';
import './comments.test.ts';
import './hover.test.ts';
import './semanticTokens.test.ts';
import './signatureHelp.test.ts';
import './unknownSelectorGate.test.ts';
import './workspaceXref.test.ts';
import './resolve.test.ts';
import './callHierarchy.test.ts';
import './crossReference.test.ts';
import './format.property.test.ts';
import './rename.test.ts';
import './rename.property.test.ts';
import './classRename.test.ts';
