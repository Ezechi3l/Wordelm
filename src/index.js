import { Elm } from "./Wordle.elm"
import storageKey from "./StorageManager"

const storageManager = storageKey("wordelm-data")({ wordsDone: [], words: [] })

const app = Elm.Wordle.init({
  node: document.getElementById("app"),
  flags: storageManager.get(),
})

app.ports.updateStorage.subscribe(function(state) {
  storageManager.set(state)
});
