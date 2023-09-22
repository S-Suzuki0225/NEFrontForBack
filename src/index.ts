import "./styles/index.css";
import { Elm } from "./Main.elm";

const elem = document.getElementById("main");
if (elem) {
  Elm.Main.init({ node: elem });
}
