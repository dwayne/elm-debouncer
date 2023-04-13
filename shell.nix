let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/44302d48a0b26421d23ee375ade3e47f9aab21df.tar.gz") {};
in
pkgs.mkShell {
  packages = [
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.nodejs-18_x
  ];

  shellHook =
    ''
    export project="$PWD"
    export PATH="$project/bin:$PATH"

    npm install --loglevel error >/dev/null
    '';
}
