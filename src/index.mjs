import Main from "./Main.elm";

const params = new URLSearchParams(window.location.search);
Main.init({
  flags: {
    letters: params.get("letters"),
  },
  node: document.getElementById("root"),
});
