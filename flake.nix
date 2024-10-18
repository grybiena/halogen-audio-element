{
  description = "halogen-audio-element";

  inputs = {
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, easy-purescript-nix }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]; # "aarch64-linux" not supported
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default =
          let
            easy-ps = import easy-purescript-nix { inherit pkgs; };
          in
          pkgs.mkShell {
            packages = with pkgs; [ nodejs ]
            ++ (with easy-ps; [
              purescript
              purs
              spago
              purescript-language-server
              purs-tidy
              esbuild
            ]);
          };
      });
    };
}
