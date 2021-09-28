with (import <nixpkgs> {});

mkShell {
  buildInputs = [
    libev
  ];
}
