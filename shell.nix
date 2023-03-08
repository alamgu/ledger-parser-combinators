let
  alamgu = import ./dep/alamgu {};
  inherit (alamgu) pkgs;
in
alamgu.rustShell.overrideAttrs (old: {
  shellHook = (old.shellHook or "") + ''
     export PATH=${pkgs.protobuf}/bin:$PATH
  '';
  
  PROTO_INCLUDE = "${pkgs.protobuf}/include";
})
