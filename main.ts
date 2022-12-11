let [year, day] = Deno.args;
if (!day) {
  day = year;
  year = new Date().getFullYear().toString();
}

console.log(`Running ${year} ${day}`);

// const timeStart = performance.now();
console.time('Finished in');
const p = Deno.run({
  cmd: ['deno', 'run', '--allow-read', `${year}/day${day}/day${day}.ts`],
  stdout: 'piped',
  stderr: 'piped',
  stdin: 'null',
});
const { success } = await p.status();
// const timeEnd = performance.now();
console.timeEnd('Finished in');

if (success) {
  await Deno.stdout.write(await p.output());
} else {
  console.log(new TextDecoder().decode(await p.stderrOutput()));
}
