const fs = require('fs');
const path = require('path');
const vsctm = require('vscode-textmate');
const oniguruma = require('vscode-oniguruma');

const wasmPath = path.join(__dirname, '../../node_modules/vscode-oniguruma/release/onig.wasm');
const wasmBin = fs.readFileSync(wasmPath);

const vscodeOnigurumaLib = oniguruma.loadWASM(wasmBin).then(() => {
    return {
        createOnigScanner(patterns) {
            return new oniguruma.OnigScanner(patterns);
        },
        createOnigString(s) {
            return new oniguruma.OnigString(s);
        }
    };
});

const registry = new vsctm.Registry({
    onigLib: vscodeOnigurumaLib,
    loadGrammar: (scopeName) => {
        if (scopeName === 'source.smalltalk.gnu') {
            const grammarPath = path.join(__dirname, '../../syntaxes/gnu-smalltalk.tmLanguage.json');
            const grammarContent = fs.readFileSync(grammarPath, 'utf8');
            return Promise.resolve(vsctm.parseRawGrammar(grammarContent, grammarPath));
        }
        return Promise.resolve(null);
    }
});

async function run() {
    const grammar = await registry.loadGrammar('source.smalltalk.gnu');
    const testDir = path.join(__dirname, '../../docs/research/gst-syntax/test-cases');
    const snapshotDir = path.join(__dirname, 'snapshots');
    
    if (!fs.existsSync(snapshotDir)) {
        fs.mkdirSync(snapshotDir, { recursive: true });
    }

    const files = fs.readdirSync(testDir).filter(f => f.endsWith('.st'));
    let errors = 0;

    for (const file of files) {
        const content = fs.readFileSync(path.join(testDir, file), 'utf8');
        const lines = content.split(/\r\n|\r|\n/);
        let ruleStack = vsctm.INITIAL;
        let output = [];

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const result = grammar.tokenizeLine(line, ruleStack);
            ruleStack = result.ruleStack;
            
            output.push(`LINE: ${line}`);
            for (const token of result.tokens) {
                const tokenText = line.substring(token.startIndex, token.endIndex);
                output.push(`  [${token.startIndex}, ${token.endIndex}] ${token.scopes.join(' ')} : "${tokenText}"`);
            }
        }

        const snapshotPath = path.join(snapshotDir, file + '.snap');
        const snapshotContent = output.join('\n');

        if (process.argv.includes('--update')) {
            fs.writeFileSync(snapshotPath, snapshotContent);
            console.log(`Updated snapshot: ${file}`);
        } else {
            if (fs.existsSync(snapshotPath)) {
                const expected = fs.readFileSync(snapshotPath, 'utf8');
                if (expected !== snapshotContent) {
                    console.error(`Snapshot mismatch for ${file}`);
                    errors++;
                } else {
                    console.log(`Verified: ${file}`);
                }
            } else {
                console.error(`Missing snapshot for ${file}. Run with --update to create.`);
                errors++;
            }
        }
    }

    if (errors > 0) {
        console.error(`${errors} failures.`);
        process.exit(1);
    } else {
        console.log('All grammar tests passed.');
    }
}

run();
