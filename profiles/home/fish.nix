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
      v = "nvim -c 'FzfLua files'";
      la = "eza -lah";
      tp = "trash-put";
    };
  };
}
