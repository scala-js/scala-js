let buffer = "";

// A function that can send one Scala Char at the time to the console.
export function printc(c) {
  if (c == 0x0a) {
    console.log(buffer);
    buffer = "";
  } else {
    buffer += String.fromCharCode(c);
  }
}
