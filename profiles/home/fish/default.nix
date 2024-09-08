{ ... }:
{
  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      set fish_greeting # Disable greeting
    '';

    shellAliases = {
      k = "kubectl";
      la = "eza -lah";
      tp = "trash-put";
    };
  };

  programs.starship.enable = true;
  home.file = {
    ".config/starship.toml" = {
      source = ./starship.toml;
    };
  };
}
