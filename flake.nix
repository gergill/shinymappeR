{
  description = "ShinyMappeR flake for usage and development.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      # Define which platforms you support
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      # Helper to make per-system outputs
      forAllSystems = nixpkgs.lib.genAttrs systems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };

          rEnv = pkgs.rWrapper.override {
            packages = with pkgs.rPackages; [
              mappeR
              ggplot2
              patchwork
              shiny
              dplyr
              tidyr
              devtools
              remotes
              RColorBrewer
              mclust
              nortest
              dendextend
              igraph
              httpuv
              styler
              lintr
            ];
          };

          texlive = pkgs.texlive.combine {
            inherit (pkgs.texlive)
              scheme-basic
              collection-latexextra
              biber
              latexmk
              ;
          };
        in
        {
          formatter = pkgs.nixfmt-rfc-style;

          devShells.default = pkgs.mkShell {
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
              pkgs.stdenv.cc.cc
              pkgs.libz
            ];

            LC_ALL = "en_US.UTF-8";

            buildInputs = with pkgs; [
              rEnv
              texlive
              pkg-config
              zlib.dev
              openssl.dev
              curl.dev
              just
              watchexec
              self.formatter.${system}
            ];

            shellHook = ''
              export R_LIBS_USER=""
              echo "Entering R dev environment for ${system}..."
              echo "R: $(R --version | head -n 1)"
              echo "LaTeX: $(which pdflatex)"
              just --list-heading $'Commands:\n' \
                   --list-prefix "    just " \
                   --no-aliases --list
            '';
          };
        }
      );
    in
    {
      formatter = nixpkgs.lib.genAttrs systems (system: forAllSystems.${system}.formatter);

      devShells = nixpkgs.lib.genAttrs systems (system: forAllSystems.${system}.devShells);
    };
}
