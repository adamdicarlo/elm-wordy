{pkgs, ...}: {
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

  # https://devenv.sh/processes/

  # https://devenv.sh/services/

  # https://devenv.sh/scripts/

  # https://devenv.sh/basics/
  enterShell = ''
    bun install && echo -e '\nAll set! Use "npm start" to run the dev server.\n'
  '';

  # https://devenv.sh/tasks/

  # https://devenv.sh/tests/
  enterTest = ''
    elm-format --validate ./src
    elm-test
  '';

  # https://devenv.sh/git-hooks/
  git-hooks.hooks.elm-format.enable = true;
  git-hooks.hooks.elm-test.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
