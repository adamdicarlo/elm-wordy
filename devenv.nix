{
  config,
  pkgs,
  lib,
  ...
}: {
  # https://devenv.sh/basics/

  # https://devenv.sh/packages/
  packages = [
    pkgs.bun
    pkgs.git
    pkgs.nodejs_24
  ];

  # https://devenv.sh/languages/
  languages.elm.enable = true;
  languages.elm.lsp.enable = true;
  languages.javascript.bun.install.enable = true;

  # https://devenv.sh/processes/

  # https://devenv.sh/services/

  # https://devenv.sh/scripts/
  scripts = {
    build = {
      exec = ''cd "$DEVENV_ROOT" && clean && generate-dictionary && bunx vite build "$@"'';
      description = "Build the project for production; params are passed through to Vite";
    };
    clean = {
      exec = ''rm -rf "$DEVENV_ROOT/dist"'';
      description = "Clean build outputs";
    };
    format = {
      exec = ''
        PRETTIER_ARG=--write
        ELM_FORMAT_ARG=--yes
        if [[ "$1" == "--check" ]]; then
          PRETTIER_ARG=--check
          ELM_FORMAT_ARG=--validate
        fi
        cd "$DEVENV_ROOT"
        bunx prettier "$PRETTIER_ARG" .
        bunx elm-format "$ELM_FORMAT_ARG" ./src
      '';
      description = "Format code. Pass --check to only check formatting.";
    };
    generate-dictionary = {
      exec = ./scripts/generate-dictionary.js;
      description = "Generate an Elm module containing all words in the game's dictionary.";
      package = pkgs.nodejs_24;
    };
    start = {
      exec = ''cd "$DEVENV_ROOT" && clean && generate-dictionary && bunx vite'';
      description = "Start the local development server";
    };
    tests = {
      exec = ''
        cd "$DEVENV_ROOT"
        format --check

        echo >&2 -e "\n# Running elm-test"
        bunx elm-test
      '';
      description = "Run tests";
    };
  };

  # https://devenv.sh/basics/
  enterShell = ''
    bun install

    echo -e '\nAll set! The following project commands are available in this shell:\n'

    ${pkgs.gnused}/bin/sed -e 's| |••|g' -e 's|=| |' <<EOF | ${pkgs.util-linuxMinimal}/bin/column -t | ${pkgs.gnused}/bin/sed -e 's|^|> |' -e 's|••| |g'
    ${lib.generators.toKeyValue {} (lib.mapAttrs (name: value: value.description) config.scripts)}
    EOF
  '';

  # https://devenv.sh/tasks/

  # https://devenv.sh/git-hooks/
  git-hooks.hooks.elm-format.enable = true;
  git-hooks.hooks.elm-test.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
