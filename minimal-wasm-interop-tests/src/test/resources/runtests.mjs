import { readFile } from "node:fs/promises";

const linkerOutput = process.env["SCALAJS_LINKER_OUTPUT"];
console.log(`running tests from ${linkerOutput}`);
const helperModulesDir = process.env["HELPER_MODULES_DIR"];
console.log(`loading helpers from ${helperModulesDir}`);

async function loadModuleExports(moduleFile, importsObj) {
  const buffer = await readFile(moduleFile);
  const module = await WebAssembly.instantiate(buffer, importsObj);
  return module.instance.exports;
}

const i8ArrayAccess = await loadModuleExports(`${helperModulesDir}/i8ArrayAccess.wasm`);

function makeWasmArray(arrayAccessModule, elems) {
  const result = arrayAccessModule.create(elems.length);
  for (let i = 0; i < elems.length; i++)
    arrayAccessModule.set(result, i, elems[i]);
  return result;
}

const importsObj = {
  "foo": {
    foobar: (x) => x * 5,
  },
};

const exports = await loadModuleExports(`${linkerOutput}/main.wasm`, importsObj);

let testFailures = [];

function assertEquals(expected, actual) {
  if (actual !== expected)
    throw new Error(`expected ${expected}; but got ${actual}`);
}

function test(testName, testBody) {
  console.log("----------------------");
  console.log(testName);
  try {
    testBody();
    console.log("OK");
  } catch (e) {
    testFailures.push(testName);
    console.error(e);
  }
}

test("basic int", () => {
  assertEquals(20, exports.foo(4));
});

test("basic i8 array", () => {
  assertEquals(-21, exports.i8ArraySum(makeWasmArray(i8ArrayAccess, [5, 10, -17, 101, -120])));
})

if (testFailures.length > 0) {
  console.error("----------------------");
  console.error("there were test failures:");
  for (const testName of testFailures)
    console.error(testName);
  process.exit(1);
}
