type Cmd<T> = {
  subscribe: (fn: (value: T) => void) => void;
};
type Sub<T> = {
  send: (value: T) => void;
};
interface Ports {}
type Flags = undefined;

export namespace Elm {
  namespace Main {
    export interface App {
      ports: Ports;
    }
    export function init(param: { node: HTMLElement; flags: Flags }): Elm.Main.App;
  }
}
