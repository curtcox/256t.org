const field = document.querySelector('#base');
field.value = location.hash.slice(1);
history.replaceState(null, '', location.pathname);
const result = document.querySelector('#result');
let client;
document.querySelector('#run').addEventListener('click', async () => {
  result.textContent = 'Verifying and loading…';
  try {
    client?.dispose(); client = new T256Client(field.value.trim());
    const uri = async (text, suffix) => `t256:${await t256URI.computeCid(text)}.${suffix}`;
    const image = await uri('<svg xmlns="http://www.w3.org/2000/svg"><circle r="50"/></svg>', 'svg');
    await client.image(document.querySelector('#preview'), image);
    await client.script(await uri('window.t256DemoOrder=[1]', 'js'));
    await client.script(await uri('window.t256DemoOrder.push(2)', 'js'));
    document.querySelector('#script-result').textContent = `Script order: ${window.t256DemoOrder.join(' → ')}`;
    result.textContent = `Verified image and scripts.\nImage URI: ${image}`;
  } catch (error) { result.textContent = error.message; }
});
