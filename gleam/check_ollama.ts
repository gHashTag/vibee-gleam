async function main() {
  try {
    const resp = await fetch("http://localhost:11434/api/version");
    const data = await resp.json();
    console.log("Ollama is running:", data);
  } catch (e) {
    console.log("Ollama is not running");
  }
}
main();
