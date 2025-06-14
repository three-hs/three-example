with import <nixpkgs> {};

mkShell {
  buildInputs = [
    gnumake
    plantuml
  ];
}

