{}:

let
  nixage = import ../nix { exposeNixage = true; };

in nixage.buildNixProject ./.
