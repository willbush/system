{ ... }:
{
  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      set fish_greeting # Disable greeting
    '';

    shellAliases = {
      g = "lazygit";
      v = "nvim -c 'FzfLua files'";
      la = "eza -lah";
      tp = "trash-put";
    };
  };
}
