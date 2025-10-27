{
  description = "ShinyMappeR flake for usage and development.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      rEnv = pkgs.rWrapper.override {
        packages = with pkgs.rPackages; [
          mappeR
          ggplot2
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
        inherit (pkgs.texlive) scheme-small collection-latexextra;
      };
    in
    {
      formatter.${system} = pkgs.nixfmt-rfc-style;

      devShells.${system}.default = pkgs.mkShell {
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
          pkgs.stdenv.cc.cc
          pkgs.libz
        ];

        LC_ALL = "en_US.UTF-8";

        buildInputs = with pkgs; [
          rEnv
          texlive
          pkgs.R
          pkgs.pkg-config
          pkgs.zlib.dev
          pkgs.openssl.dev
          pkgs.curl.dev
          just
          self.formatter.${system}
        ];

        shellHook = ''
          echo "Entering R dev environment..."
          echo "R: $(R --version | head -n 1)"
          echo "LaTeX: $(which pdflatex)"
          just --list-heading $'Commands:\n' --list-prefix "    just " --no-aliases --list
        '';
      };
    };
}
