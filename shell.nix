{ pkgs ? import <nixpkgs> {}
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    cargo
    rustc
    cmake
    protobuf
  ];

  PROTO_INCLUDE = "${pkgs.protobuf}/include";
}
