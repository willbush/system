{ ... }:
{
  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      set fish_greeting # Disable greeting
    '';

    shellAliases = {
      la = "eza -lah";
      tp = "trash-put";
      h = "hx .";
    };
  };
}
