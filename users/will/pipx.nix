{ pkgs, ... }: {
  home = {
    # Only use pipx to install python apps that are not available in nixpkgs.
    packages = with pkgs; [ pipx ];
    sessionPath = [ "$HOME/.local/bin" ];
  };
}
