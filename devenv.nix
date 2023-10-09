{ pkgs, ... }:

{
  # https://devenv.sh/basics/
  # env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = with pkgs; [
    bun
    nodejs

    # Installing packages with bun is super fast. Unfortunately, it doesn't
    # build native modules, and one of our dependencies has one, so we must
    # manually build it in an npm postinstall hook with node-gyp-build, which
    # requires node-gyp and python.
    nodePackages.node-gyp
    nodePackages.node-gyp-build
    python3
  ];

  # https://devenv.sh/scripts/

  enterShell = ''
    bun install && echo -e '\nAll set! Use "npm start" to run the dev server.\n'
  '';

  # https://devenv.sh/languages/
  languages.elm.enable = true;

  # https://devenv.sh/pre-commit-hooks/
  pre-commit.hooks.elm-format.enable = true;
  pre-commit.hooks.elm-test.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
