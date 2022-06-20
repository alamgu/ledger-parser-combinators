{ pkgs ? import <nixpkgs> {}
}:
let
  ledger-platform = import ./dep/ledger-platform {};
in
ledger-platform.rustShell.overrideAttrs(old: {
  shellHook = (old.shellHook or "") + ''
     export PATH=${pkgs.protobuf}/bin:$PATH
  '';
  
  PROTO_INCLUDE = "${pkgs.protobuf}/include";
})
