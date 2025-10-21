{ ... }:
{
  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      set fish_greeting # Disable greeting
    '';

    shellAliases = {
      g = "lazygit";
      h = "hx .";
      la = "eza -lah";
      tp = "trash-put";
    };
  };
}
